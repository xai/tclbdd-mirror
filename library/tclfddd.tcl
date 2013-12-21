package require tclbdd 0.1

namespace eval bdd::fddd {
    namespace export concatenate domain interleave reader support
}

#-----------------------------------------------------------------------------
#
# The next few procedures are for working with domain descriptions, which
# describe a finite, multivalued domain within a BDD. A domain description
# consists of two parts:
#	    
#	(1) A dictionary whose keys are domain names and whose values
#	    are lists of variable indices used to encode values in the given
#	    domains. The lists appear least significant bit first.
#
#	(2) A list that gives the layout of BDD variables that
#	    represent values in the given domains. The list appears in
#	    order by variable index. It consists of alternating domain
#	    names and bit positions in values in the domain (with
#	    position zero being the least significant bit).
#
#-----------------------------------------------------------------------------

# bdd::fddd::Invert --
#
#	Given the list of column names and bit positions in the database
#	layout, makes an inverted list indexed by column giving the
#	bit positions of that column

proc bdd::fddd::Invert {bitlist} {
    set i 0
    set result {}
    foreach {name pos} $bitlist {
	if {[dict exists $result $name]} {
	    set l [dict get $result $name]
	} else {
	    set l {}
	}
	while {[llength $l] <= $pos} {
	    lappend l {}
	}
	lset l $pos $i
	dict set result $name $l
	incr i
    }
    return $result
}

# bdd::fddd::domain --
#
#	Defines a new finite domain
#
# Usage:
#	bdd::fddd::domain name width ?endian?
#
# Parameters:
#	name - Name of the domain
#	width - Log base 2 of the number of distinct values.
#	endian - 'littleendian' (default) if the values are to be
#		 encoded in BDD's with the least significant bit first.
#		 'bigendian' if they are to be encoded with the most
#		 significant bit first
#
# Results:
#	Returns a domain description.

proc bdd::fddd::domain {name width {endian littleendian}} {
    switch -exact -- $endian {
	littleendian {
	    set l {}
	    for {set i 0} {$i < $width} {incr i} {
		lappend l $name $i
	    }
	}
	bigendian {
	    set l {}
	    for {set i [expr {$width-1}]} {$i >= 0} {incr i -1} {
		lappend l $name $i
	    }
	}
	default {
	    return -code error -errorcode [list FDDD BadEndian $endian] \
		"unknown endian \"$endian\": must be bigendian or littleendian"
	}
    }
    return [list [Invert $l] $l]
}

# bdd::bdd::fddd::interleave --
#
#	Interleaves some number of finite domains so that their bit positions
#	in a BDD alternate.
#
# Usage:
#	bdd::fddd::interleave ?description...?
#
# Parameters:
#	Zero or more domain descriptions whose bits are to be interleaved.
#	All domains in the descriptions must be distinct.
#
# Results:
#	Returns a domain description of the interleaved domains.
#
# Errors:
#	{FDDD DuplicateName $name} if any domain is not distinct
#
# The domains are interleaved by taking one bit from the first domain,
# one from the second, and so on. If they are of differing length, then
# the process ceases taking bits from the shorter ones when they run out.

proc bdd::fddd::interleave {args} {
    set N 0
    set names {}
    set bits {}
    foreach domain $args {
	dict for {name poslist} [lindex $domain 0] {
	    if {[dict exists $names $name]} {
		return -code error -errorcode [list FDDD DuplicateName $name] \
		    "domain named \"$name\" appears in multiple places"
	    }
	    incr N [llength $poslist]
	    dict set names $name {}
	}
	lappend bits [lindex $domain 1]
    }
    set processed 0
    set scrambled {}
    while {$processed < $N} {
	set i 0
	foreach b $bits {
	    if {[llength $b] > 0} {
		lset bits $i [lassign $b name pos]
		lappend scrambled $name $pos
		incr processed
	    }
	    incr i
	}
    }
    return [list [Invert $scrambled] $scrambled]
}

# bdd::fddd::concatenate --
#
#	Concatenates the descriptions of a set of finite domains
#
# Usage:
#	bdd::fddd::concatenate ?description...?
#
# Parameters:
#	Zero or more finite domain descriptions.
#
# Results:
#	Returns a description with the bits of the domains concatenated
#	together.
#
# Errors:
#	{FDDD DuplicateName $name} if any domain is not distinct

proc bdd::fddd::concatenate {args} {
    set N 0
    set names {}
    set bits {}
    foreach domain $args {
	dict for {name poslist} [lindex $domain 0] {
	    if {[dict exists $names $name]} {
		return -code error -errorcode [list FDDD DuplicateName $name] \
		    "domain named \"$name\" appears in multiple places"
	    }
	    incr N [llength $poslist]
	    dict set names $name {}
	}
	lappend bits [lindex $domain 1]
    }
    set chain {}
    foreach b $bits {
	lappend chain {*}$b
    }
    return [list [Invert $chain] $chain]
}

if 0 { # this needs to be a method of the database

# bdd::fddd::support --
#
#	Makes a call to the BDD engine to determine the support of a relation
#	in a finite domain
#
# Usage:
#	bdd::fddd::support sysName relationName layout
#
# Parameters:
#	sysName - Name of the system of BDD's
#	relationName - Name of the relation to be analyzed
#	layout - Description of the finite domain
#
# Results:
#	Returns the names of variables on which the relation depends.

proc bdd::fddd::support {sysName relationName layout} {
    set haveVar {}
    foreach bit [$sysName support $relationName] {
	dict set haveVar [lindex $layout 1 [expr {2 * $bit}]] {}
    }
    return [dict keys $haveVar]
}

}

# Class: bdd::fddd::database
#
#	Class that represents a database containing relations whose
#       columns belong to finite domains. The database is held in a
#	system of Binary Decision Diagrams (BDD's).

oo::class create bdd::fddd::database {

    # m_columns is a dictionary whose keys are columns and whose values are
    #           lists of BDD variables that represent the column values.
    #
    # m_relcolumns is a dictionary whose keys are the relations known to
    #              the database and whose values are lists of columns that
    #              participate in the relations.
    #
    # m_variables is a list of alternating column names and bit positions.
    #             Variable number i in the BDD system represents
    #             the 2**[lindex $m_variables [expr {2*$i+1}]] bit of
    #	          the column whose name is [lindex $m_variables [expr {2*$i}]]
    
    variable m_columns m_relcolumns m_variables

    # The constructor accepts aes its sole parameter a domain layout
    # that is computed by some series of calls to bdd::fddd::domain,
    # bdd::fddd::interleave and bdd::fddd::concatenate.

    constructor {layout} {
	
	# Make sure that the layout contains two sublists

	if {[llength $layout] != 2} {
	    if {[string length $layout] > 40} {
		set elayout [string range $layout 0 39]...
	    } else {
		set elayout $layout
	    }
	    return -code error -errorcode [list FDDD NotLayout $layout] \
		"expected a domain layout but found \"$elayout\""
	}

	# Unpack the sublists into 'm_columns' and 'm_variables'

	lassign $layout m_columns m_variables

	# There are no relations defined yet.

	set m_relcolumns {}

	# Create the underlying system of BDD's in the current object's
	# namespace.

	bdd::system create [namespace current]::sys
    }

    # Private method: Varlist
    #	
    #	Enumerates the BDD variables that make up a relation.
    #
    # Usage:
    #	$db Varlist $relation
    #
    # Parameters:
    #	relation - Name of the relation being processed.
    #
    # Results:
    # 	Returns a list of the BDD variables for all the columns of the
    #	relation.

    method Varlist {relation} {
	set retval {}
	foreach col [dict get $m_relcolumns $relation] {
	    lappend retval {*}[dict get $m_columns $col]
	}
	return $retval
    }

    # Method: ===
    #
    #	Generates code to test whether two relations are equal: that is,
    #	contain exactly the same rows.
    #
    # Usage:
    #	$db === $rel1 $rel2
    #
    # Parameters:
    #	rel1 - First relation to test
    #   rel2 - Second relation to test
    #
    # Results:
    #	Returns a command that will evaluate to 1 if the relations
    #   are equal and 0 if they are not.
    #
    # This method does not perform the test directly: it generates
    # code to perform the test.
    #
    # The test executes in constant time.

    method === {rel1 rel2} {
	if {![dict exists $m_relcolumns $rel1]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $rel1] \
		"relation \"$rel1\" is not defined in this database"
	}
	if {![dict exists $m_relcolumns $rel2]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $rel2] \
		"relation \"$rel2\" is not defined in this database"
	}
	if {[dict get $m_relcolumns $rel1]
	    ne [dict get $m_relcolumns $rel2]} {
	    return -code error \
		-errorcode [list FDDD WrongColumns $rel1 $rel2] \
		"relations \"$rel1\" and \"$rel2\" have different columns"
	}
	return [list [namespace which sys] === $rel1 $rel2]
    }
    export ===

    # Method: antijoin
    #
    #	Generates code to antijoin a pair of relations
    #
    # Usage:
    #	$db antijoin dest source1 source2
    #
    # Results:
    #	Returns a fragment of code that will perform the antijoin.
    #
    # The result of the antijoin is the set of rows that are
    # members of the $source relation but not members of the $dest
    # relation. This is well defined for any two relations: if either
    # relation contains excess columns, the other relation is presumed
    # to contain tuples with every possible value in those columns.
    # Of course, the excess columns may be projected away before or
    # after the antijoin. Most commonly, the columns of $source2 will
    # all appear in $source1.
    #
    # The result will have the union of the columns of the two inputs.
    #
    # This method does not perform the copy directly: it generates
    # code to perform the copy.
    #
    # The antijoin executes in time proportional to the size of the
    # source1 BDD.

    method antijoin {dest source1 source2} {
	if {![dict exists $m_relcolumns $dest]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $dest] \
		"relation \"$dest\" is not defined in this database"
	}
	if {![dict exists $m_relcolumns $source1]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $source1] \
		"relation \"$source1\" is not defined in this database"
	}
	if {![dict exists $m_relcolumns $source2]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $source2] \
		"relation \"$source2\" is not defined in this database"
	}
	set destcolumns [dict get $m_relcolumns $source1]
	lappend destcolumns {*}[dict get $m_relcolumns $source2]
	set destcolumns [lsort -ascii -unique $destcolumns]
	set destcolumns_sb [dict get $m_relcolumns $dest]
	if {$destcolumns ne $destcolumns_sb} {
	    return -code error \
		-errorcode [list FDDD BadJoin $destcolumns $destcolumns_sb] \
		"result of the antijoin has columns $destcolumns but the\
                 target relation has columns $destcolumns_sb"
	}

	return [list [namespace which sys] > $dest $source1 $source2]
    }

    # Method: columns
    #
    #	Enumerates all columns in the database
    #
    # Usage:
    #	$db columns
    #
    # Results:
    #	Returns a list of the column names that are known to the databsae.

    method columns {} {
	return [dict keys $m_columns]
    }

    # Method: enumerate
    #
    #	Iterates over all the tuples in a relation
    #
    # Usage:
    #	$db enumerate dictvar relation { script }
    #
    # Parameters:
    #	dictvar - Variable that will receive a dictionary whose keys
    #		  are column names are whose values are the values in
    #		  the given columns
    #	relation - Name of the relation to iterate over
    #   script -   Script to execute on each tuple.
    #
    # Results:
    #	None.
    #
    # This call iterates over the given relation. For each row that is
    # found, the given variable is set to a dictionary containing the rows
    # values, and the given script is evaluated.
    #
    # If evaluating the script causes an error, the iteration is terminated.
    # If the script performs a return, the return has its natural meaning.
    # If the script performs a break or continue, the iteration is
    # terminated (for a break) or recommences with the next tuple
    # (for a continue).
    # [return -code 6] is synonymous with [break]
    # Any other nonstandard status code terminates the iteration.

    method enumerate {dictvar relation script} {
	upvar 1 $dictvar valdict

	# Make sure the relation exists
	if {![dict exists $m_relcolumns $relation]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $relation] \
		"relation \"$relation\" is not defined in this database"
	}

	# Iterate over the relation, getting SAT terms. Iterate over
	# the variable assignments that satisfy the terms.
	sys foreach_sat satterm $relation {
	    bdd::foreach_fullsat vars [my Varlist $relation] $satterm {

		# Iterate over the columns and populate the dictionary
		set i 0
		set valdict {}
		foreach column [dict get $m_relcolumns $relation] {
		    set varsForColumn [dict get $m_columns $column]
		    set value 0
		    for {set j 0} {$j < [llength $varsForColumn]} {incr j} {
			set value [expr {$value | ([lindex $vars $i] << $j)}]
			incr i
		    }
		    dict set valdict $column $value
		}

		# Evaluate the script and handle the status returns
		try {
		    uplevel 1 $script
		} on error {message options} {
		    dict incr options -level 1
		    return -options $options $message
		} on return {retval options} {
		    dict incr options -level 1
		    return -options $options $message
		} on break {} {
		    return -level 0 -code 6
		} on continue {} {
		    continue
		}
	    }
	}
	return
    }

    # Method: forget
    #
    #	Removes relations from the database
    #
    # Usage:
    #	db forget ?relation...?
    #
    # Parameters:
    #	relation... - Names of relations to remove
    #
    # Results:
    #	None.

    method forget_relation {args} {
	foreach name $args {
	    sys unset $name
	    dict unset m_relcolumns $name
	}
	return
    }

    # Method: gc
    #
    #	Garbage collects the database
    #
    # Usage:
    #	$db gc
    #
    # Results:
    #	Returns the number of beads in the BDD system after garbage collection
    #
    # This method should not ordinarily be called; the incremental garbage
    # collector is faster than the mark and sweep done by this call. This call
    # exists for leak checking, making sure that all beads are reclaimed after
    # a unit test.

    method gc {} {
	return [sys gc]
    }

    # Method: join
    #
    #	Generates code to join a pair of relations
    #
    # Usage:
    #	$db join dest source1 source2
    #
    # Results:
    #	Returns a fragment of code that will perform the join.
    #
    # The result of the join is the set of rows that are
    # members of the $source relation but not members of the $dest
    # relation. This is well defined for any two relations: if either
    # relation contains excess columns, the other relation is presumed
    # to contain tuples with every possible value in those columns.
    # Of course, the excess columns may be projected away before or
    # after the join. Most commonly, the columns of $source2 will
    # all appear in $source1.
    #
    # The result will have the union of the columns of the two inputs.
    #
    # This method does not perform the copy directly: it generates
    # code to perform the copy.
    #
    # The join executes in time proportional to the size of the
    # source1 BDD.

    method join {dest source1 source2} {
	if {![dict exists $m_relcolumns $dest]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $dest] \
		"relation \"$dest\" is not defined in this database"
	}
	if {![dict exists $m_relcolumns $source1]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $source1] \
		"relation \"$source1\" is not defined in this database"
	}
	if {![dict exists $m_relcolumns $source2]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $source2] \
		"relation \"$source2\" is not defined in this database"
	}
	set destcolumns [dict get $m_relcolumns $source1]
	lappend destcolumns {*}[dict get $m_relcolumns $source2]
	set destcolumns [lsort -ascii -unique $destcolumns]
	set destcolumns_sb [dict get $m_relcolumns $dest]
	if {$destcolumns ne $destcolumns_sb} {
	    return -code error \
		-errorcode [list FDDD BadJoin $destcolumns $destcolumns_sb] \
		"result of the join has columns $destcolumns but the\
                 target relation has columns $destcolumns_sb"
	}
	return [list [namespace which sys] & $dest $source1 $source2]
    }

    # Method: loader
    #
    #	Generates code to call the BDD engine to construct a term 
    #   corresponding to a tuple in a finite domain.
    #
    # Usage:
    #	$db loader relation
    #
    # Parameters:
    #   db - Name of the database
    #   relation - Name of the relation being loaded
    #
    # Results:
    #	Returns a command prefix for a command that will add a tuple
    #	to the given relation.
    #
    # To the prefix should be appended the values of the columns,
    # in [lsort -ascii] order by the colun names.

    method loader {relation} {
	if {![dict exists $m_relcolumns $relation]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $relation] \
		"relation \"$relation\" is not defined in this database"
	}
	set i 0
	foreach column [dict get $m_relcolumns $relation] {
	    dict set cmdpos $column $i
	    incr i
	}
	set result {}
	set p 0
	foreach {column bit} $m_variables {
	    if {[dict exists $cmdpos $column]} {
		lappend result $p [dict get $cmdpos $column] $bit
	    }
	    incr p
	}
	set cmd [list [namespace which sys] load $relation $result]
	return $cmd
    }


    method project {dest source} {
	# columns to project away are determined by dest and source
	if {![dict exists $m_relcolumns $dest]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $dest] \
		"relation \"$dest\" is not defined in this database"
	}
	if {![dict exists $m_relcolumns $source]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $source] \
		"relation \"$source\" is not defined in this database"
	}
	set discards {}
	foreach col [dict get $m_relcolumns $dest] {
	    dict set want $col {}
	}
	foreach col [dict get $m_relcolumns $source] {
	    if {![dict exists $want $col]} {
		lappend discards {*}[dict get $m_columns $col]
	    } else {
		dict unset want $col
	    }
	}
	if {[dict size $want] != 0} {
	    return -code error \
		-errorcode [list FDDD ProjectColumnMissing {*}[dict keys $want]]\
		"columns missing from source relation: [dict keys $want]"
	}
	return [list [namespace which sys] project $dest \
		    [lsort -integer $discards] $source]
	return
    }

    # Method: set
    #
    #	Generates code to copy one relation to another
    #
    # Usage:
    #	$db set $dest $source
    #
    # Parameters:
    #	dest   - Name of the output relation
    #	source - Name of the input relation
    #
    # Results:
    #	Returns a command that will make the first relation a copy of
    #	the second.
    #
    # The source may be one of the special values:
    #	{} - (The empty string) - generates a relation with no rows.
    #	_  - generates a relation containin every combination of column
    #        values.
    #
    # This method does not perform the copy directly: it generates
    # code to perform the copy.
    #
    # The copy executes in constant time.

    method set {dest source} {
	if {![dict exists $m_relcolumns $dest]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $dest] \
		"relation \"$dest\" is not defined in this database"
	}
	if {$source eq {}} {
	    set source 0
	} elseif {$source eq {_}} {
	    set source 1
	} else {
	    if {![dict exists $m_relcolumns $source]} {
		return -code error \
		    -errorcode [list FDDD RelationNotDefined $source] \
		    "relation \"$source\" is not defined in this database"
	    }
	    if {[dict get $m_relcolumns $dest]
		ne [dict get $m_relcolumns $source]} {
		return -code error \
		    -errorcode [list FDDD WrongColumns $dest $source] \
		    "relations \"$dest\" and \"$source\" have different columns"
	    }
	}
	return [list [namespace which sys] := $dest $source]
    }

    method equate {dest col1 col2} {
	if {![dict exists $m_columns $col1]} {
	    return -code error -errorcode [list FDDD NoSuchColumn $col1] \
		"no such column: \"$col1\""
	}
	if {![dict exists $m_columns $col2]} {
	    return -code error -errorcode [list FDDD NoSuchColumn $col2] \
		"no such column: \"$col2\""
	}
	set vars1 [dict get $m_columns $col1]
	set vars2 [dict get $m_columns $col2]
	if {[llength $vars1] != [llength $vars2]} {
	    return -code error \
		-errorcode [list FDDD EquateWrongDomains $col1 $col2] \
		"cannot equate domains \"$col1\" and \"$col2\":\
                 sizes do not match"
	}
	# TODO - Typecheck $dest and sort one list in decreasing order by var#
	sys := $dest 1
	foreach v $vars1 v2 $vars2 {
	    sys nthvar _a $v
	    sys nthvar _b $v2
	    sys == temp _a _b
	    sys & $dest $dest temp
	}
	sys unset temp _a _b
    }

    method load {name list} {
	if {![dict exists $m_relcolumns $name]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $name] \
		"relation \"$name\" is not defined in this database"
	}
	set nColumns [llength [dict get $m_relcolumns $name]]
	if {[llength $list] % $nColumns != 0} {
	    return -code error \
		-errorcode [list FDDD WrongListLength $nColumns] \
		"list must have a multiple of $nColumns values"
	}
	set reader [my loader $name]
	set nCM1 [expr {$nColumns - 1}]
	while {[llength $list] > 0} {
	    {*}$reader {*}[lrange $list 0 $nCM1]
	    set list [lreplace ${list}[set list {}] 0 $nCM1]
	}
	return
    }

    method profile {relation} {
	if {![dict exists $m_relcolumns $relation]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $relation] \
		"relation \"$relation\" is not defined in this database"
	}
	sys profile $relation
    }

    method relation {name args} {
	if {[dict exists $m_relcolumns $name]} {
	    return -code error \
		-errorcode [list FDDD RelationAlreadyDefined $name] \
		"relation \"$name\" is already defined in this database"
	}
	set havecol {}
	foreach col $args {
	    if {[dict exists $havecol $col]} {
		return -code error -errorcode [list FDDD DuplicateColumn $col] \
		    "column $col is duplicated in the column list"
	    }
	    if {![dict exists $m_columns $col]} {
		return -code error -errorcode [list FDDD NoSuchColumn $col] \
		    "no such column: \"$col\""
	    }
	    dict set $havecol $col {}
	}
	dict set m_relcolumns $name [lsort -ascii $args]
	return $name
    }

    method replace {dest source args} {
	if {![dict exists $m_relcolumns $dest]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $dest] \
		"relation \"$dest\" is not defined in this database"
	}
	if {![dict exists $m_relcolumns $source]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $source] \
		"relation \"$source\" is not defined in this database"
	}
	set sourcecols {}
	foreach col [dict get $m_relcolumns $source] {
	    dict set sourcecols $col {}
	}
	set destcols {}
	foreach col [dict get $m_relcolumns $dest] {
	    dict set destcols $col {}
	}
	set colsAdded {}
	foreach {to from} $args {
	    if {[dict exists $colsAdded $to]} {
		return -code error \
		    -errorcode [list FDDD DuplicateProjectOutput $to] \
		    "attempt to rename two input columns to \"$to\""
	    }
	    if {![dict exists $sourcecols $from]} {
		return -code error \
		    -errorcode [list FDDD BadProjectInput $from] \
		    "attempt to rename an input column \"from\" that\
		     does not exist or has already been renamed"
	    }
	    if {![dict exists $destcols $to]} {
		return -code error \
		    -errorcode [list FDDD BadProjectOutput $to] \
		    "attempt to rename an input column to \"$to\",
                     which does not exist in \"$dest\""
	    }
	    dict unset sourcecols $from
	    dict set colsAdded $to {}
	}
	foreach {to from} $args {
	    if {[dict exists $sourcecols $to]} {
		return -code error \
		    -errorcode [list FDDD ProjectOutputOverInput $to] \
		    "attempt to rename an input column to \"$to\",
                     but another input column already has that name."
	    }
	    dict set sourcecols $to {}
	}
	if {[lsort [dict keys $sourcecols]] ne [dict get $m_relcolumns $dest]} {
	    return -code error \
		-errorcode [list FDDD ProjectWrongColumns \
				[dict keys $sourcecols] \
				[dict get $m_relcolumns $dest]] \
		"replacement yields columns [dict keys $sourcecols]\
                 but target relation has columns\
                 [dict get $m_relcolumns $dest]"
	}
	set fromvars {}
	set tovars {}
	foreach {to from} $args {
	    set tv [dict get $m_columns $to]
	    set fv [dict get $m_columns $from]
	    if {[llength $tv] < [llength $fv]} {
		return -code error \
		    -errorcode [list FDDD ProjectColumnTooNarrow $from $to] \
		    "replacement column \"$to\" is to narrow to hold values\
                     from column \"$from\""
	    }
	    lappend fromvars {*}$fv
	    lappend tovars {*}[lrange $tv 0 [expr {[llength $fv] - 1}]]
	}
	return [list [namespace which sys] replace $dest $fromvars $tovars $source]
    }
	    

    method union {dest source1 source2} {
	# TODO typecheck
	return [list [namespace which sys] | $dest $source1 $source2]
    }
}

package provide tclbdd::fddd 0.1
