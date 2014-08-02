# tclfddd.tcl --
#
#	Class definitions and Tcl-level methods for the bdd::fddd package:
#	Binary Decision Diagram (BDD) implementation of Finite Domain
#	Decision Diagrams (FDDD's).
#
# Copyright (c) 2014 by Kevin B. Kenny
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#------------------------------------------------------------------------------

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

# bdd::fddd::interleave --
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

    # Private method: ColumnsMustBe
    #
    #	Makes sure that a relation has the correct set of columns
    #
    # Usage:
    #	my ColumnsMustBe $rel $cols
    #
    # Parameters:
    #	rel - Name of the relation
    #   cols - Names of the columns that the relation must have, in
    #          dictionary order
    #
    # Results:
    #	Returns an empty result if the column set of the relation is correct.
    #   Causes the caller to return an error if it is different.

    method ColumnsMustBe {rel cols} {
	if {[lsort -dictionary [dict get $m_relcolumns $rel]]
	    ne [lsort -dictionary $cols]} {
	    return -level 2 -code error \
		-errorcode [list FDDD WrongColumns $rel \
				[dict get $m_relcolumns $rel] $cols] \
		"relation \"$rel\" has columns\
                 [join [dict get $m_relcolumns $rel] {, }] but should have\
                 columns [join $cols {, }]"
	}
	return
    }

    # Private method: ColumnsMustBeSame
    #
    #	Makes sure that two relations span the same sets of columns
    #
    # Usage:
    #	my ColumnsMustBeSame $rel1 $rel2
    #
    # Parameters:
    #	rel1 - Name of the first relation
    #   rel2 - Name of the second relation
    #
    # Results:
    #	Returns an empty result if the column sets of the two relations
    #   are the same. Causes the caller to return an error if they are 
    #   different.

    method ColumnsMustBeSame {rel1 rel2} {
	if {[lsort -dictionary [dict get $m_relcolumns $rel1]]
	    ne [lsort -dictionary [dict get $m_relcolumns $rel2]]} {
	    return -level 2 -code error \
		-errorcode [list FDDD DifferentColumns $rel1 $rel2] \
		"relations \"$rel1\" and \"$rel2\" have different columns"
	}
	return
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
	my relationMustExist $rel1
	my relationMustExist $rel2
	my ColumnsMustBeSame $rel1 $rel2
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
    # The columns of the result relation must be the union of the
    # columns of the two inputs.
    #
    # This method does not perform the copy directly: it generates
    # code to perform the copy.
    #
    # The antijoin executes in time proportional to the size of the
    # source1 BDD.

    method antijoin {dest source1 source2} {
	my relationMustExist $dest
	my relationMustExist $source1
	my relationMustExist $source2

	# Determine the columns in the joined relations
	set destcolumns [dict get $m_relcolumns $source1]
	lappend destcolumns {*}[dict get $m_relcolumns $source2]
	set destcolumns [lsort -dictionary -unique $destcolumns]
	my ColumnsMustBe $dest $destcolumns

	# Make code to do the antijoin
	return [list [namespace which sys] > $dest $source1 $source2]
    }

    # Method: columnMustExist
    #
    #	Makes sure that a given column exists in the database
    #
    # Usage:
    #	my columnMustExist $name
    #
    # Parameters:
    #	name - Name of a column
    #
    # Results:
    #	Returns an empty result if the column exists. Causes the caller
    #	to return an error if the column is not found.

    method columnMustExist {name} {
	if {![dict exists $m_columns $name]} {
	    return -level 2 -code error \
		-errorcode [list FDDD ColumnNotDefined $name] \
		"column \"$name\" is not defined in this database"
	}
	return
    }

    # Method: columns
    #
    #	Enumerates all columns in the database, or the columns of a
    #	specific relation.
    #
    # Usage:
    #	$db columns ?relation?
    #
    # Parameters:
    #	relation - If supplied, indicates the relation being queried.
    #
    # Results:
    #
    #	Returns the list of columns that participate in the given
    #	relation.  If no relation is supplied, returns a list of all
    #	the column names that are known to the database
    #
    # This method executes directly, rather than returning a code fragment

    method columns {{relation {}}} {
	if {$relation eq {}} {
	    return [dict keys $m_columns]
	} else {
	    my relationMustExist $relation
	    return [dict get $m_relcolumns $relation]
	}
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
    #
    # This method executes directly, rather than returning a code fragment

    method enumerate {dictvar relation script} {
	upvar 1 $dictvar valdict

	my relationMustExist $relation

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

    # Method: equate
    #
    #	Generates code for an equality constraint (which is a partial
    #	implementation of a self join).
    #
    # Usage:
    #	db equate dest col1 col2
    #
    # Parameters:
    #	dest - Name of a relation that will contain one row for each
    #          value in its domain that has col1 == col2
    #   col1 - Name of the first column to equate
    #   col2 - Name of the second column to equate
    #
    # Results:
    #	Returns a fragment of code that creates the given relation.
    #
    # The destination relation must contain both col1 and col2 as columns.
    # The domains of col1 and col2 must have the same size.
    #
    # This method does not create the equality relation; it returns a fragment
    # of code that does it.
    #
    # The time taken to create the equality is highly variable. In the case
    # where the two columns' variables are interleaved, the time is linear
    # in the number of bits of the domains. In the case where they are
    # concatenated, both time and space are exponential in the number of
    # variables. Other orderings will give results between these two extremes.

    method equate {dest col1 col2} {
	my relationMustExist $dest
	my columnMustExist $col1
	my columnMustExist $col2
	set destcolumns [dict get $m_relcolumns $dest]
	if {[lsearch -exact $destcolumns $col1] == -1} {
	    return -code error -errorcode \
		[list FDDD RelationDoesNotContainColumn $dest $col1] \
		"relation \"$dest\" does not contain column \"$col1\""
	}
	if {[lsearch -exact $destcolumns $col2] == -1} {
	    return -code error -errorcode \
		[list FDDD RelationDoesNotContainColumn $dest $col1] \
		"relation \"$dest\" does not contain column \"$col1\""
	}
	set vars1 [dict get $m_columns $col1]
	set vars2 [dict get $m_columns $col2]
	if {[llength $vars1] != [llength $vars2]} {
	    return -code error \
		-errorcode [list FDDD EquateWrongDomains $col1 $col2] \
		"cannot equate domains \"$col1\" and \"$col2\":\
                 sizes do not match"
	}

	# Sort the variables in descending order by the first column's
	# positions. This will keep them ordered well for the rename in
	# the easy cases.

	set vlist {}
	foreach v $vars1 v2 $vars2 {
	    lappend vlist $v $v2
	}
	set vlist [lsort -stride 2 -integer -index 0 -decreasing $vlist]
	set code [list [namespace which sys] := $dest 1]
	foreach {v v2} $vlist {
	    append code \; [list [namespace which sys] nthvar :a $v]
	    append code \; [list [namespace which sys] nthvar :b $v2]
	    append code \; [list [namespace which sys] == :t :a :b]
	    append code \; [list [namespace which sys] & $dest $dest :t]
	}
	append code \; [list [namespace which sys] unset :a :b :t]
	return $code
    }

    # Method: forget_relation
    #
    #	Removes relations from the database
    #
    # Usage:
    #	db forget_relation ?relation...?
    #
    # Parameters:
    #	relation... - Names of relations to remove
    #
    # Results:
    #	None.
    #
    # This method executes directly, rather than returning a code fragment.

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
    #
    # This method executes directly, rather than returning a code fragment

    method gc {} {
	return [sys gc]
    }

    # Method: inequality
    #
    #	Generates code for an inequality constraint 
    #
    # Usage:
    #	db inequality dest col1 col2
    #
    # Parameters:
    #	dest - Name of a relation that will contain one row for each
    #          value in its domain that has col1 != col2
    #   col1 - Name of the first column to compare
    #   col2 - Name of the second column to compare
    #
    # Results:
    #	Returns a fragment of code that creates the given relation.
    #
    # The destination relation must contain both col1 and col2 as columns.
    # The domains of col1 and col2 must have the same size.
    #
    # This method does not create the inequality relation; it returns a fragment
    # of code that does it.
    #
    # The time taken to create the inequality is highly variable. In the case
    # where the two columns' variables are interleaved, the time is linear
    # in the number of bits of the domains. In the case where they are
    # concatenated, both time and space are exponential in the number of
    # variables. Other orderings will give results between these two extremes.

    method inequality {dest col1 col2} {
	my relationMustExist $dest
	my columnMustExist $col1
	my columnMustExist $col2
	set destcolumns [dict get $m_relcolumns $dest]
	if {[lsearch -exact $destcolumns $col1] == -1} {
	    return -code error -errorcode \
		[list FDDD RelationDoesNotContainColumn $dest $col1] \
		"relation \"$dest\" does not contain column \"$col1\""
	}
	if {[lsearch -exact $destcolumns $col2] == -1} {
	    return -code error -errorcode \
		[list FDDD RelationDoesNotContainColumn $dest $col1] \
		"relation \"$dest\" does not contain column \"$col1\""
	}
	set vars1 [dict get $m_columns $col1]
	set vars2 [dict get $m_columns $col2]
	if {[llength $vars1] != [llength $vars2]} {
	    return -code error \
		-errorcode [list FDDD EquateWrongDomains $col1 $col2] \
		"cannot equate domains \"$col1\" and \"$col2\":\
                 sizes do not match"
	}

	# Sort the variables in descending order by the first column's
	# positions. This will keep them ordered well for the rename in
	# the easy cases.

	set vlist {}
	foreach v $vars1 v2 $vars2 {
	    lappend vlist $v $v2
	}
	set vlist [lsort -stride 2 -integer -index 0 -decreasing $vlist]
	set code [list [namespace which sys] := $dest 0]
	foreach {v v2} $vlist {
	    append code \; [list [namespace which sys] nthvar :a $v]
	    append code \; [list [namespace which sys] nthvar :b $v2]
	    append code \; [list [namespace which sys] != :t :a :b]
	    append code \; [list [namespace which sys] | $dest $dest :t]
	}
	append code \; [list [namespace which sys] unset :a :b :t]
	return $code
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
    # The columns of the result relation must be the union of the
    # columns of the two inputs.
    #
    # This method does not perform the copy directly: it generates
    # code to perform the copy.
    #
    # The join executes in time proportional to the size of the
    # source1 BDD.

    method join {dest source1 source2} {
	my relationMustExist $dest
	my relationMustExist $source1
	my relationMustExist $source2

	# Determine the columns in the joined relations
	set destcolumns [dict get $m_relcolumns $source1]
	lappend destcolumns {*}[dict get $m_relcolumns $source2]
	set destcolumns [lsort -dictionary -unique $destcolumns]
	my ColumnsMustBe $dest $destcolumns

	# Make code to do the join
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
    # To the prefix should be appended the values of the columns

    method loader {relation} {
	my relationMustExist $relation
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

    # Method: negate
    #
    #	Generates code to compute the complement of a relation. All
    #	tuples over the relation's domain will be in the output relation
    #	if they are not in the input
    #
    # Usage:
    #	$db negate $source1
    #
    # Parameters:
    #	dest    - Name of the relation that will receive the complement
    #   source1 - Name of the input relation
    #
    # Results:
    #	Returns a burst of code that computes the complement of the
    #   relation
    #
    # Both relations must contain the same set of columns.
    #
    # This method does not compute the complement; it returns a fragment
    # of code that computes it.
    #
    # The time taken to compute the complement is linear in the 
    # size of the BDD.

    method negate {dest source} {
	my relationMustExist $dest
	my relationMustExist $source
	my ColumnsMustBeSame $dest $source
	return [list [namespace which sys] ~ $dest $source]
    }

    # Method: profile
    #
    #	Determines the number of BDD beads in use for each variable.
    #
    # Parameters:
    #	relation - Relation to profile
    #
    # Results:
    #	Returns a list ordered by variable level of elements in groups
    #	of three:
    #    - the name of a column
    #    - the bit index within the column
    #    - the number of nodes of the relation's BDD that test the given bit.
    #
    # This method executes directly, rather than returning a codeburst.

    method profile {relation} {
	my relationMustExist $relation
	set result {}
	foreach {col bit} $m_variables count [sys profile $relation] {
	    if {$count eq {}} {
		set count 0
	    }
	    if {[lsearch [dict get $m_relcolumns $relation] $col] >= 0} {
		lappend result $col $bit $count
	    } elseif {$count > 0} {
		lappend result $col? $bit $count
	    }
	}
	return $result
    }

    # Method: project
    #
    #	Uses existential quantification to project a relation onto
    #	a smaller domain.
    #
    # Usage:
    #	$db project $dest $source
    #
    # Parameters:
    #	dest   - Name of the output relation
    #	source - name of the input relation
    #
    # Results:
    #	Returns a fragment of code that will perform the projection
    #
    # All columns of the result relation must be present in the source.
    #
    # This method does not perform the projection directly: it generates
    # code to perform the projection.
    #
    # The projection executes in time proportional to the size of the
    # source BDD.

    method project {dest source} {
	# columns to project away are determined by dest and source
	my relationMustExist $dest
	my relationMustExist $source
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
		-errorcode [list FDDD ProjectColumnMissing \
				{*}[dict keys $want]]\
		"columns missing from source relation: [dict keys $want]"
	}
	return [list [namespace which sys] project $dest \
		    [lsort -integer $discards] $source]
	return
    }

    # Method: relation
    #
    #	Defines a relation in the database
    #
    # Usage:
    #	$db relation $name ?column...?
    #
    # Parameters:
    #	name      - Name of the relation
    #	column... - Set of columns that the relation constrains
    #
    # Results:
    #	None.
    #
    # Side effects:
    #	The given relation is defined. Its content in the database
    #	is initially empty.
    #
    # This command takes effect immediately, rather than returning
    # code to perform an operation at run time.

    method relation {name args} {
	if {[dict exists $m_relcolumns $name]} {
	    return -code error \
		-errorcode [list FDDD RelationAlreadyDefined $name] \
		"relation \"$name\" is already defined in this database"
	}
	set havecol {}
	foreach col $args {
	    my columnMustExist $col
	    if {[dict exists $havecol $col]} {
		return -code error -errorcode [list FDDD DuplicateColumn $col] \
		    "column $col is duplicated in the column list"
	    }
	    dict set $havecol $col {}
	}
	dict set m_relcolumns $name $args
	{*}[my set $name {}]
	return $name
    }

    # Method: relationMustExist
    #
    #	Makes sure that a given relation exists in the database
    #
    # Usage:
    #	my relationMustExist $name
    #
    # Parameters:
    #	name - Name of a relation
    #
    # Results:
    #	Returns an empty result if the relation exists. Causes the caller
    #	to return an error if the relation is not found.

    method relationMustExist {name} {
	if {![dict exists $m_relcolumns $name]} {
	    return -level 2 -code error \
		-errorcode [list FDDD RelationNotDefined $name] \
		"relation \"$name\" is not defined in this database"
	}
	return
    }

    # Method: replace
    #
    #	Generates code to reassign physical domains in a relation
    #
    # Usage:
    #	$db replace dest source ?outcol incol?...
    #
    # Parameters:
    #	dest   - Name of the output relation
    #   source - Name of the input relation
    #	outcol, incol - List of pairs of the name of a column of the
    #                   output relation and the name of the corresponding
    #                   column of the input.
    #
    # Results:
    #	Returns a fragment of code that performs the replacement.
    #
    # The destination relation must have the correct set of columns
    # defined after the replacement. All columns are replaced simultaneously.
    #
    # This method does not perform the replacement; it returns a fragment
    # of code that does it.
    #
    # The time taken for the replacement is highly variable. In the case
    # where all the BDD variables of the replacement columns are in
    # the same order as those of the input columns (RECOMMENDED), it
    # is linear in the size of the input BDD. In the worst case of variable
    # reordering, it may be exponential in the size of the input BDD.
    # This will happen, for instance, if an equality over an interleaved
    # pair of columns is replaced with an equality over a concatenated pair.

    method replace {dest source args} {
	my relationMustExist $dest
	my relationMustExist $source
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
	    my columnMustExist $to
	    my columnMustExist $from
	    if {[dict exists $colsAdded $to]} {
		return -code error \
		    -errorcode [list FDDD DuplicateReplaceOutput $to] \
		    "attempt to rename two input columns to \"$to\""
	    }
	    if {![dict exists $sourcecols $from]} {
		return -code error \
		    -errorcode [list FDDD BadReplaceInput $from] \
		    "attempt to rename an input column \"from\" that\
		     does not exist or has already been renamed"
	    }
	    if {![dict exists $destcols $to]} {
		return -code error \
		    -errorcode [list FDDD BadReplaceOutput $to] \
		    "attempt to rename an input column to \"$to\",
                     which does not exist in \"$dest\""
	    }
	    dict unset sourcecols $from
	    dict set colsAdded $to {}
	}
	foreach {to from} $args {
	    if {[dict exists $sourcecols $to]} {
		return -code error \
		    -errorcode [list FDDD ReplaceOutputOverInput $to] \
		    "attempt to rename an input column to \"$to\",
                     but another input column already has that name."
	    }
	    dict set sourcecols $to {}
	}
	my ColumnsMustBe $dest [lsort -dictionary [dict keys $sourcecols]]

	set fromvars {}
	set tovars {}
	foreach {to from} $args {
	    set tv [dict get $m_columns $to]
	    set fv [dict get $m_columns $from]
	    if {[llength $tv] < [llength $fv]} {
		return -code error \
		    -errorcode [list FDDD ReplaceColumnTooNarrow $from $to] \
		    "replacement column \"$to\" is to narrow to hold values\
                     from column \"$from\""
	    }
	    lappend fromvars {*}$fv
	    lappend tovars {*}[lrange $tv 0 [expr {[llength $fv] - 1}]]
	}

	return \
	    [list [namespace which sys] replace $dest $fromvars $tovars $source]
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
	my relationMustExist $dest
	if {$source eq {}} {
	    set source 0
	} elseif {$source eq {_}} {
	    set source 1
	} else {
	    my relationMustExist $source
	    my ColumnsMustBeSame $dest $source
	}
	return [list [namespace which sys] := $dest $source]
    }

    # Method: union
    #
    #	Generates code to compute the union of two relations
    #
    # Usage:
    #	$db union $dest $source1 $source2
    #
    # Parameters:
    #	dest    - Name of the relation that will receive the union
    #   source1 - Name of the first input relation
    #   source2 - Name of the second input relation
    #
    # Results:
    #	Returns a burst of code that computes the union of the two
    #   relations
    #
    # All three relations must contain the same set of columns.
    #
    # This method does not compute the union; it returns a fragment
    # of code that computes it.
    #
    # The time taken to compute the union is linear in the sum of the
    # sizes of the two BDD's.

    method union {dest source1 source2} {
	my relationMustExist $dest
	my relationMustExist $source1
	my relationMustExist $source2
	my ColumnsMustBeSame $dest $source1
	my ColumnsMustBeSame $dest $source2
	return [list [namespace which sys] | $dest $source1 $source2]
    }

if 0 {
    # deprecate this for now.
    method load {name list} {
	my relationMustExist $name
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
}



}

package provide tclbdd::fddd 0.1
