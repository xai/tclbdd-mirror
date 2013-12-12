#package require tclbdd 0.1

namespace eval fdd {
    namespace export domain interleave concatenate
}

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
	puts "appended to bits: [lindex $domain 1] result=$bits"
    }
    puts "fdd::concatenate: bits: $bits"
    set scrambled {}
    foreach b $bits {
	lappend scrambled {*}$b
    }
    return [list $names $scrambled]
}

proc fdd::reader {sysName termName layout args} {
    set i 0
    foreach name $args {
	dict set cmdpos $name $i
	puts "$name appears at position $i"
	incr i
    }
    set result {}
    set p 0
    foreach {name bit} [lindex $layout 1] {
	if {[dict exists $cmdpos $name]} {
	    lappend result $p [dict get $cmdpos $name] $bit
	} else {
	    puts "$name does not appear"
	}
	incr p
    }
    set cmd [list $sysName load $termName $result]
    foreach name $args {
	append cmd " \$" $name
    }
    return $cmd
}



    