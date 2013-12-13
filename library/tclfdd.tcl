#package require tclbdd 0.1

namespace eval fdd {
    namespace export domain interleave concatenate
}

#-----------------------------------------------------------------------------
#
# The next few procedures are for working with domain descriptions, which
# describe a finite, multivalued domain within a BDD. A domain description
# consists of two parts:
#	    
#	(1) A dictionary whose keys are domain names and whose values
#	    are the number of bits used to encode values in the given
#	    domains.
#
#	(2) A list that gives the layout of BDD variables that
#	    represent values in the given domains. The list appears in
#	    order by variable index. It consists of alternating domain
#	    names and bit positions in values in the domain (with
#	    position zero being the least significant bit).
#
#-----------------------------------------------------------------------------

# fdd::domain --
#
#	Defines a new finite domain
#
# Usage:
#	fdd::domain name width ?endian?
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

proc fdd::domain {name width {endian littleendian}} {
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
	    return -code error -errorcode [list FDD BadEndian $endian] \
		"unknown endian \"$endian\": must be bigendian or littleendian"
	}
    }
    return [list [dict create $name $width] $l]
}

# fdd::interleave --
#
#	Interleaves some number of finite domains so that their bit positions
#	in a BDD alternate.
#
# Usage:
#	fdd::interleave ?description...?
#
# Parameters:
#	Zero or more domain descriptions whose bits are to be interleaved.
#	All domains in the descriptions must be distinct.
#
# Results:
#	Returns a domain description of the interleaved domains.
#
# Errors:
#	{FDD DuplicateName $name} if any domain is not distinct
#
# The domains are interleaved by taking one bit from the first domain,
# one from the second, and so on. If they are of differing length, then
# the process ceases taking bits from the shorter ones when they run out.

proc fdd::interleave {args} {
    set N 0
    set names {}
    set bits {}
    foreach domain $args {
	dict for {name width} [lindex $domain 0] {
	    if {[dict exists $names $name]} {
		return -code error -errorcode [list FDD DuplicateName $name] \
		    "domain named \"$name\" appears in multiple places"
	    }
	    incr N $width
	    dict set names $name $width
	    lappend bits [lindex $domain 1]
	}
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
    return [list $names $scrambled]
}

# fdd::concatenate --
#
#	Concatenates the descriptions of a set of finite domains
#
# Usage:
#	fdd::concatenate ?description...?
#
# Parameters:
#	Zero or more finite domain descriptions.
#
# Results:
#	Returns a description with the bits of the domains concatenated
#	together.
#
# Errors:
#	{FDD DuplicateName $name} if any domain is not distinct

proc fdd::concatenate {args} {
    set N 0
    set names {}
    set bits {}
    foreach domain $args {
	dict for {name width} [lindex $domain 0] {
	    if {[dict exists $names $name]} {
		return -code error -errorcode [list FDD DuplicateName $name] \
		    "domain named \"$name\" appears in multiple places"
	    }
	    incr N $width
	    dict set names $name $width
	}
	lappend bits [lindex $domain 1]
    }
    set chain {}
    foreach b $bits {
	lappend chain {*}$b
    }
    return [list $names $chain]
}

# fdd::reader --
#
#	Makes a call to the BDD engine to construct a minterm corresponding
#	to a tuple in a finite domain.
#
# Usage:
#	fdd::reader sysName termName layout domain ?domain...?
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
#     [fdd::concatenate \
#         [fdd::domain var 3 bigendian] \
#         [fdd::interleave \
#             [fdd::domain stmt 5] [fdd::domain stmt2 5]]]
#  set r [fdd::reader sys reads $desc stmt var]
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
# system "construct a minterm named 'reads', where variable zero is set from
# parameter 1, the 2**2 bit, variable 1 from parameter 1 the 2**1 bit,
# variable 2 from parameter 1 the 2**0 bit, variable 3 from parameter 0 the
# 2**0 bit, variable 5 from parameter 0 the 2**1 bit ... variable 11 from
# parameter 0 the 2**4 bit."

proc fdd::reader {sysName termName layout args} {
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

package provide tclfdd 0.1