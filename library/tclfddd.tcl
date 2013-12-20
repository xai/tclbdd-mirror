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

# bdd::fddd::reader --
#
#	Makes a call to the BDD engine to construct a term corresponding
#	to a tuple in a finite domain.
#
# Usage:
#	bdd::fddd::reader sysName termName layout domain ?domain...?
#
# Parameters:
#	sysName - Name of the system of BDD's
#	termName - Name of the term to be constructed
#	layout - Description of the finite domain
#	domain - One or more domain names, in the order in which values
#	         will appear in an input tuple.
#
# Results:
#	Returns a command prefix for a command that will create a
#	named term containing a tuple. 
#
# To the prefix should be appended the values of the domain elements,
# in the order in which their domains appeared on this command.
#
# Example:
#
# set desc \
#     [bdd::fddd::concatenate \
#         [bdd::fddd::domain var 3 bigendian] \
#         [bdd::fddd::interleave \
#             [bdd::fddd::domain stmt 5] [bdd::fddd::domain stmt2 5]]]
#  set r [bdd::fddd::reader sys reads $desc stmt var]
#
# leaves desc set to:
#
#    {var 3 stmt 5 stmt2 5} 
#    {var 2 var 1 var 0 stmt 0 stmt2 0 stmt 1 stmt2 1 stmt 2 stmt2 2 
#     stmt 3 stmt2 3 stmt 4 stmt2 4}
#
# and r set to:
#   sys load reads {0 1 2 1 1 1 2 1 0 3 0 0 5 0 1 7 0 2 9 0 3 11 0 4}
#
# which (when two additional args are catenated on the end), asks the BDD
# system "construct a term named 'reads', where variable zero is set from
# parameter 1, the 2**2 bit, variable 1 from parameter 1 the 2**1 bit,
# variable 2 from parameter 1 the 2**0 bit, variable 3 from parameter 0 the
# 2**0 bit, variable 5 from parameter 0 the 2**1 bit ... variable 11 from
# parameter 0 the 2**4 bit."

proc bdd::fddd::reader {sysName termName layout args} {
    set i 0
    foreach name $args {
	dict set cmdpos $name $i
	incr i
    }
    set result {}
    set p 0
    foreach {name bit} [lindex $layout 1] {
	if {[dict exists $cmdpos $name]} {
	    lappend result $p [dict get $cmdpos $name] $bit
	}
	incr p
    }
    set cmd [list $sysName load $termName $result]
    return $cmd
}

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

oo::class create bdd::fddd::database {

    variable m_columns m_relcolumns m_variables

    constructor {layout} {
	#puts "LAYOUT: $layout"
	if {[llength $layout] != 2} {
	    if {[string length $layout] > 40} {
		set elayout [string range $layout 0 39]...
	    } else {
		set elayout $layout
	    }
	    return -code error -errorcode [list FDDD NotLayout $layout] \
		"expected a domain layout but found \"$elayout\""
	}
	lassign $layout m_columns m_variables
	set m_relcolumns {}
	bdd::system create [namespace current]::sys
    }

    # TODO - Check domain consistency?
    method === {rel1 rel2} {
	return [sys === $rel1 $rel2]
    }
    export ===

    method Varlist {relation} {
	#puts "What are the variables for $relation?"
	set retval {}
	foreach col [dict get $m_relcolumns $relation] {
	    #puts "$col has variables [dict get $m_columns $col]"
	    lappend retval {*}[dict get $m_columns $col]
	}
	return $retval
    }

    method columns {} {
	return [dict keys $m_columns]
    }

    method set {dest source} {
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
	if {[dict get $m_relcolumns $dest]
	    ne [dict get $m_relcolumns $source]} {
	    return -code error \
		-errorcode [list FDDD WrongColumns $dest $source] \
		"relations \"$dest\" and \"$source\" have different columns"
	}
	return [list [namespace which sys] negate $dest $source]
    }
    method antijoin {dest source1 source2} {
	# TODO typecheck
	return [list [namespace which sys] > $dest $source1 $source2]
    }

    # TODO - How can this be streamlined?

    method enumerate {dictvar relation script} {
	upvar 1 $dictvar valdict
	if {![dict exists $m_relcolumns $relation]} {
	    return -code error \
		-errorcode [list FDDD RelationNotDefined $relation] \
		"relation \"$relation\" is not defined in this database"
	}
	sys foreach_sat satterm $relation {
	    #puts "expand a term of $relation with variables [my Varlist $relation]"
	    bdd::foreach_fullsat vars [my Varlist $relation] $satterm {
		#puts "and values $vars"
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

    method forget_relation {name} {
	sys unset $name
	dict unset m_relcolumns $name
	return
    }

    method gc {} {
	return [sys gc]
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
	set reader [bdd::fddd::reader [namespace current]::sys \
			$name [list $m_columns $m_variables] \
			{*}[dict get $m_relcolumns $name]]
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

    method union {dest source1 source2} {
	# TODO typecheck
	return [list [namespace which sys] | $dest $source1 $source2]
    }
}

package provide tclbdd::fddd 0.1
