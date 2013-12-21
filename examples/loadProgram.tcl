# loads a program into an FDDD database

# Program for simple examples of flow analysis

package require tclbdd::fddd

# Find the size of the database, and find labels in the program
#
# Returns: A list of variable names used in the program, and a list of
#          labels and their locations in the list of instructions.

proc pass1 {program} {
    set vars {}
    set labels {}
    set i 0
    set vn 0
    foreach insn $program {
	if {[regexp {^(_[[:alnum:]_]+):$} [lindex $insn 0] -> label]} {
	    dict set labels $label $i
	    set insn [lrange $insn[set insn {}] 1 end]
	}
	foreach token $insn {
	    if {[regexp {^[[:alpha:]][[:alnum:]_]*$} $token]} {
		if {![dict exists $vars $token]} {
		    dict set vars $token $vn
		    incr vn
		}
	    }
	}
	incr i
    }
    return [list $vars $labels]
}

# finds the number of bits needed to represent a finite domain

proc fdbits {numvals} {
    set bits 1
    set bound 2
    while {$bound < $numvals} {
	incr bits
	incr bound $bound
    }
    return $bits
}

# Calculates 'reads', 'writes' and 'seq'

proc pass2 {program vars labels} {
    set i 0
    foreach insn $program {
	if {[regexp {^(_[[:alnum:]_]+):$} [lindex $insn 0] label]} {
	    set insn [lrange $insn[set insn {}] 1 end]
	}
	if {[lindex $insn 1] eq {:=}} {
	    lappend writes $i [dict get $vars [lindex $insn 0]]
	    set insn [lrange $insn[set insn {}] 2 end]
	}
	foreach token $insn {
	    if {[regexp {^[[:alpha:]][[:alnum:]_]*$} $token]} {
		lappend reads $i [dict get $vars $token]
	    }
	}
	if {[lindex $insn 0] eq {*GOTO}} {
	    lappend seq $i [dict get $labels [lindex $insn 1]]
	} elseif {[lindex $insn 0] eq {*IF} && [lindex $insn 2] eq {*GOTO}} {
	    lappend seq $i [dict get $labels [lindex $insn 3]] $i [expr {$i+1}]
	} else {
	    lappend seq $i [expr {$i+1}]
	}
	incr i
    }
    return [list $reads $writes $seq]
}
    
proc analyzeProgram {program db} {
    # pass 1 - find variables, resolve labels, calculate domain sizes
    lassign [pass1 $program] vars labels
    set stbits [fdbits [llength $program]]
    set vbits [fdbits [dict size $vars]]

    # Create the program database
    
    bdd::fddd::database create $db \
	[bdd::fddd::concatenate \
	     [bdd::fddd::domain v $vbits bigendian] \
	     [bdd::fddd::interleave \
		  [bdd::fddd::domain st  $stbits littleendian] \
		  [bdd::fddd::domain st2 $stbits littleendian] \
		  [bdd::fddd::domain st3 $stbits littleendian]]]
    
    $db relation reads st v
    $db relation writes st v
    $db relation seq st st2

    # pass 2 - discover 'reads', 'writes', 'seq' relations
    lassign [pass2 $program $vars $labels] reads writes seq

    # TODO - Load incrementally in pass 2
    
    $db load reads $reads
    $db load writes $writes
    $db load seq $seq

    # Return the variable dictionary, since it is not readily recoverable

    return $vars
}
