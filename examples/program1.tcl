source [file join [file dirname [info script]] loadProgram.tcl]

# sample program is (more or less)
#
# proc cosine {x n} {
#     set j 0
#     set s 1.0
#     set t 1.0
#     set i 0
#     while {[incr i] < $n} {
#         set t [expr {-$t * $x * $x / [incr j] / [incr j]}]
#         set s [expr  {$s + $t}]
#     }
#     return s
# }

set program [split [string trim {
    *ENTRY
    x := *PARAMETER 0
    n := *PARAMETER 1
    j := 0
    s := 1.0
    t := 1.0
    i := 0
    *GOTO _entry 
_loop: a := - t
    a := a * x
    a := a * x
    j := j + 1
    a := a / j
    j := j + 1
    t := a / j
    s := s + t
_entry: i := i + 1
    a := i < n
    *IF a *GOTO _loop
    *RETURN s
}] "\n"]
