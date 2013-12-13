# tclbdd.tcl --
#
#	Class definitions and Tcl-level methods for the bdd package
#
# Copyright (c) 2013 by Kevin B. Kenny
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#------------------------------------------------------------------------------

package require grammar::aycock;	# TEMP - Build parser in advance!
package require grammar::aycock::runtime

namespace eval bdd {
    namespace export system
}

oo::class create bdd::system {
    method === {exprName1 exprName2} {
	expr {[my beadindex $exprName1] == [my beadindex exprName2]}
    }
    export ===
    method satisfiable {exprName} {
	expr {[my beadindex $exprName] != 0}
    }
    method tautology {exprName} {
	expr {[my beadindex $exprname] == 1}
    }
    method support {exprName} {
	set i 0
	set result {}
	foreach k [my profile $exprName] {
	    if {$k != 0} {
		lappend result $i
	    }
	    incr i
	}
    }
    method beadcount {exprName} {
	tcl::mathop::+ {*}[my profile $exprName]
    }
}

proc bdd::Lexer {script} {
    set lexvals {\n ; ; ; ( ( ) ) = = ? ? : : nor NOR < < <= <=
	== == != != ^ ^ > > >= >= -> <= <- >= <-> == & & | | ~ ~ - - ! ~
	0 constant 1 constant}
    set line 0
    set cpos -1
    set symbols {}
    set values {}
    set indx 0
    while {[regexp -expanded -indices -start $indx {
	\A(?:
	   \\\n
	   | \n
	   | \s
	   | ;
	   | \(
	   | \)
	   | =
	   | \?
	   | :
	   | <
	   | <=
	   | ==
	   | !=
	   | >
	   | >=
	   | ->
	   | <-
	   | <->
	   | &
	   | \|
	   | -
	   | \^
	   | ~
	   | !
	   | 0\M
	   | 1\M
	   | [A-Za-z_][A-Za-z0-9_]*
	   )} $script token]} {
	lassign $token start end
	set value [string range $script $start $end]
	if {[dict exists $lexvals $value]} {
	    lappend tokens [dict get $lexvals $value]
	    lappend values $value
	} elseif {![string is space $value]} {
	    lappend tokens variable
	    lappend values $value
	}
	if {$token eq "\n"} {
	    incr line
	    set cpos $start
	}
	set indx [expr {$end+1}]
    }
    if {$indx == [string length $script]} {
	return [list $tokens $values]
    } else {
	set lpos [expr {$indx - $cpos}]
	set ch 	 [string index $script $indx]
	return -code error \
	    -errorCode [list BDD LexError $line $lpos $ch] \
	    "Lexical error at $line:$lpos near character '$ch'"
    }
}

set bdd::Parser [grammar::aycock::parser {
    
    start ::= script {}

    script ::= stmt {set _}
    script ::= script ; stmt {linsert [lindex $_ 0] end [lindex $_ 2]}

    stmt ::= assignment {}

    assignment ::= variable = expression {
	list [lindex $_ 1] [lindex $_ 0] [lindex $_ 2]
    }

    expression ::= expression ? orexp : expression {
	list terop ?: [lindex $_ 2] [lindex $_ 0] [lindex $_ 4]
    }
    expression ::= orexp {}

    orexp ::= orexp | xorexp {
	list binop [lindex $_ 1] [lindex $_ 0] [lindex $_ 2]
    }
    orexp ::= xorexp {}

    xorexp ::= xorexp ^ andexp {
	list binop [lindex $_ 1] [lindex $_ 0] [lindex $_ 2]
    }
    xorexp ::= andexp {}

    andexp ::= andexp & eqvexp {
	list binop [lindex $_ 1] [lindex $_ 0] [lindex $_ 2]
    }
    andexp ::= eqvexp {}

    eqvexp ::= eqvexp == ineqexp {
	list binop [lindex $_ 1] [lindex $_ 0] [lindex $_ 2]
    }
    eqvexp ::= eqvexp != ineqexp {
	list binop [lindex $_ 1] [lindex $_ 0] [lindex $_ 2]
    }
    eqvexp ::= ineqexp {}

    ineqexp ::= ineqexp <= addexp {
	list binop [lindex $_ 1] [lindex $_ 0] [lindex $_ 2]
    }
    ineqexp ::= ineqexp < addexp {
	list binop [lindex $_ 1] [lindex $_ 0] [lindex $_ 2]
    }
    ineqexp ::= ineqexp > addexp {
	list binop [lindex $_ 1] [lindex $_ 0] [lindex $_ 2]
    }
    ineqexp ::= ineqexp >= addexp {
	list binop [lindex $_ 1] [lindex $_ 0] [lindex $_ 2]
    }
    ineqexp ::= addexp {}
    
    addexp ::= addexp - primary {
	list binop [lindex $_ 1] [lindex $_ 0] [lindex $_ 2]
    }
    addexp ::= primary {}

    primary ::= ( expression ) { lindex $_ 1 }
    primary ::= variable { list variable [lindex $_ 0] }
    primary ::= CONSTANT { list constant [lindex $_ 0] }

}]
