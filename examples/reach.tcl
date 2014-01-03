source [file join [file dirname [info script]] loadProgram.tcl]

proc profile! {db var} {
    puts "$var: [$db profile $var]"
}

interp alias {} profile {} profile!
# proc profile args {}

source [file join [file dirname [info script]] program1.tcl]

set vars [analyzeProgram $program db]

profile db reads
profile db writes
profile db seq

#############################################################################

# PROPOSED OBJECT CODE OF A DATALOG PROGRAM

# flowspast(v,st,st2) means: Control may pass from the end of
# instruction 'st1' to the start of instruction 'st2' without
# assigning to the variable 'v'.  Note how the induction case
# recurses twice into 'flowspast' rather than using 'seq'. This
# means that the path length of the 'flowpast' relation can
# roughly double on each pass, meaning that the number of passes
# in the iteration-to-convergence is only logarithmic in the
# length of the longest loop in the program.

# reach(v,st,st2) means: "The use of variable v at instruction st2
# might see the assignment to variable v at instruction st1"

# Reachability: relation definitions that might be generated by a Datalog
# compiler.

db relation t10 v;		# The universal set of variables
db relation t11 v st3;		# writes(v, st3)
db relation t12 v st st3;	# flowspast(v, st, st3)
db relation t13 v st st3;	# flowspast(v, st, st3),!writes(st3,v)
db relation t14 v st2 st3;	# flowspast(v, st3, st2)
db relation t15 v st st2 st3;	# flowspast(v, st, st3),
				# !writes(st3,v),
				# flowspast(v, st3, st2)
db relation t16 v st st2;	# project{v,st,st2}(t15)
db relation t17 v st st2;	# writes(st,v),flowspast(v,st,st2)
db relation t18 v st2;		# reads(st2,v)

db relation flowspast v st st2
db relation flowspast' v st st2
db relation reaches v st st2

# Reachability: code that might be generated by a Datalog compiler

proc reachability {} [subst {

    ### input: reads(st,v)
    ### input: writes(st,v)
    ### input: seq(st,st2)
    ### output: flowspast(v,st,st2)
    ### output: reaches(v,st,st2)

    ### flowspast(v,st,st2) :- seq(st,st2)
    
    [db set t10 _]
    [db join flowspast seq t10]; 		profile db flowspast

    ### flowspast(v,st,st2) :- flowspast(v,st,st3),
    ###                        !writes(st3,v),
    ###                        flowspast(v,st3,st2)

    [db replace t11 writes st3 st]; # invariant code hoisted out of loop
    [db set flowspast' {}]
    while {!\[[db === flowspast flowspast']\]} {
	[db set flowspast' flowspast]
	[db replace t12 flowspast st3 st2];	profile db t12
	[db antijoin t13 t12 t11]; 		profile db t13
	[db replace t14 flowspast st3 st];	profile db t14
	[db join t15 t13 t14];			profile db t15
	[db project t16 t15];			profile db t16
	[db union flowspast flowspast' t16];	profile db flowspast
    }

    ### reaches(v,st,st2) :- writes(st,v),
    ###                      flowspast(v,st,st2),
    ###                      reads(st2,v)

    [db join t17 writes flowspast]; 		profile db t17
    [db replace t18 reads st2 st]; 		profile db t18
    [db join reaches t17 t18];			profile db reaches

}]

#############################################################################

# DATALOG QUERY: reaches(_,_,i)?

db relation q1 st2
db relation q1result v st st2 
proc query1 {i} [subst {
    [db set q1 {}]
    [db loader q1] \$i
    [db join q1result reaches q1]
}]

#############################################################################

# DATALOG QUERY: reaches(_,i,_)?

db relation q2 st
db relation q2result v st st2 
proc query2 {i} [subst {
    [db set q2 {}]
    [db loader q2] \$i
    [db join q2result reaches q2]
}]

#############################################################################

# Report on reachability analysis

reachability

puts [format {%-16s %2s  %-32s %-16s} PRODUCERS {} INSTRUCTIONS CONSUMERS]
set i 0
foreach stmt $program {

    query1 $i
    set flowsto {}
    db enumerate result q1result {
	lappend flowsto \
	    [lindex $vars [expr {2*[dict get $result v]}]] \
	    [dict get $result st]
    }

    query2 $i
    set flowsfrom {}
    db enumerate result q2result {
	lappend flowsfrom \
	    [lindex $vars [expr {2*[dict get $result v]}]] \
	    [dict get $result st2]
    }

    puts [format "%-16s %2d: %-32s %-16s" \
	      [lsort -stride 2 -index 0 -ascii \
		   [lsort -stride 2 -index 1 -integer $flowsto]] \
	      $i \
	      $stmt \
	      [lsort -stride 2 -index 0 -ascii \
		   [lsort -stride 2 -index 1 -integer $flowsfrom]]]

    incr i
}