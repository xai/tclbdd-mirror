source loadscript.tcl

package require tclbdd::datalog

source [file join [file dirname [info script]] .. examples loadProgram.tcl]
source [file join [file dirname [info script]] .. examples program1.tcl]

set vars [analyzeProgram $program db]
set vnames [dict keys $vars]

db relation seq st st2
db relation writes st v
db relation flowspast v st st2
db relation reaches v st st2
db relation uninitRead st v
db relation deadWrite st v

proc reaching_defs {} [bdd::datalog::compileProgram db {
 
    % A false entry node (node 0) sets every variable and flows
    % to node 1. If any of its variables are reachable, those are
    % variables possibly used uninitialized in the program.

    writes(0, _).
    writes(st,v) :- writes0(st,v).
    seq(0, 1).
    seq(st,st2) :- seq0(st,st2).

    % flowspast(v,st,st2) means that control passes from the exit of st
    % to the entry of st2 without altering the value of v

    flowspast(_, st, st2) :- seq(st, st2).
    flowspast(v, st3, st2) :- flowspast(v, st3, st),
                             !writes(st, v),
                             flowspast(v, st, st2).

    % reaches(v,st,st2) means that st assigns a value to v, which
    % reaches st2, which reads the value of v : that is, st is a
    % reaching definition for the use of v at st2.

    reaches(v, st, st2) :- writes(st, v), flowspast(v, st, st2), reads(st2, v).

    % A variable read that is reachable from the entry is a read of a
    % possibly uninitialized variable

    uninitRead(st, v) :- reaches(v, 0, st).

    % A variable write that reaches nowhere else is dead code

    deadWrite(st, v) :- writes(st, v), !reaches(v, st, _).

}]

# Report which variable definitions reach statement $i
proc query1 {i} [bdd::datalog::compileProgram db {
    reaches(v, st, $i)?
} d {
    lappend ::flowsto [lindex $::vnames [dict get $d v]] [dict get $d st]
}]

# Report which variable uses flow from statement $i
proc query2 {i} [bdd::datalog::compileProgram db {
    reaches(v, $i, st)?
} d {
    lappend ::flowsfrom [lindex $::vnames [dict get $d v]] [dict get $d st]
}]
    
puts [info body reaching_defs]

reaching_defs
puts [format {%-16s %2s  %-32s %-16s} PRODUCERS {} INSTRUCTIONS CONSUMERS]
set i 0
foreach stmt $program {
    set flowsto {}
    query1 $i
    set flowsfrom {}
    query2 $i
    puts [format "%-16s %2d: %-32s %-16s" \
	      [lsort -stride 2 -index 0 -ascii \
		   [lsort -stride 2 -index 1 -integer $flowsto]] \
	      $i \
	      $stmt \
	      [lsort -stride 2 -index 0 -ascii \
		   [lsort -stride 2 -index 1 -integer $flowsfrom]]]
    incr i
}
