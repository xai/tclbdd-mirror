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

# The 'bdd' namespace contains the entirety of the package

namespace eval bdd {

    namespace export foreach_fullsat system

    # The _fullsat namespace contains coroutines for 'foreach_fullsat'
    # calls in progress.
    namespace eval _fullsat {}

    # The _seq variable is a sequence number for generating unique names
    variable _seq 0
}

# bdd::system --
#
#	Represents a system of BDD's.
#
# Constructed by:
#	bdd::system new
#	bdd::system create NAME
#
# Parameters:
#	name -- Name of the system of BDD's.
#
# The system object supports a great many methods, most of which are
# written in C. Refer to 'generic/tclBdd.c' to find them.

oo::class create bdd::system {

    # Method: ===
    #
    #	Determines whether two BDD's are identical.
    #
    # Usage:
    #	$system === $exprName1 $exprName2
    #
    # Parameters:
    #	system - System of BDD's
    #	exprName1 - Name of the first expression (BDD) to compare
    #   exprName2 - Name of the second expression (BDD) to compare
    #
    # Results:
    #	Returns 1 if the BDD's are identical, 0 otherwise
    #
    # By the canonicity lemma, two BDD's return the same output on all
    # inputs if and only if they are identical.
    #
    # This method is distinguished from == in that == returns a BDD
    # specifying the conditions under which two BDDs give the same
    # output, while === tests whether they give the same output always.

    method === {exprName1 exprName2} {
	expr {[my beadindex $exprName1] == [my beadindex $exprName2]}
    }
    export ===

    # Method: satisfiable
    #
    #	Determines whether a BDD representing a Boolean formula is satisfiable
    #
    # Usage:
    #	$system satisfiable $exprName
    #
    # Parameters:
    #	system - System of BDD's
    #   exprName - Name of the expression to test
    #
    # Results:
    #	Returns 1 if the expression is satisfiable, 0 otherwise.

    method satisfiable {exprName} {
	expr {[my beadindex $exprName] != 0}
    }

    # Method: tautology
    #
    #	Determines whether a BDD representing a Boolean formula is 
    #	a tautology - that is, true for all inputs.
    #
    # Usage:
    #	$system tautology $exprName
    #
    # Parameters:
    #	system - System of BDD's
    #   exprName - Name of the expression to test
    #
    # Results:
    #	Returns 1 if the expression is a tautology, 0 otherwise.

    method tautology {exprName} {
	expr {[my beadindex $exprName] == 1}
    }

    # Method: support
    #
    #	Returns the indices of variables upon which a BDD representing
    #	a Boolean formula depends
    #
    # Usage:
    #	$system support $exprName
    #
    # Parameters:
    #	system - System of BDD's
    #   exprName - Name of the expression to test
    #
    # Results:
    #	Returns a list of integer indices of the variables whose values
    #   can affect the value of the expression. The expression does not
    #   depend on variables not listed in the result.

    method support {exprName} {
	set i 0
	set result {}
	foreach k [my profile $exprName] {
	    if {$k != 0} {
		lappend result $i
	    }
	    incr i
	}
	return $result
    }

    # Method: beadcount
    #
    #	Returns the number of beads in a BDD.
    #
    # Usage:
    #	$system beadcount $exprName
    #
    # Parameters:
    #	system - System of BDD's
    #   exprName - Name of the expression to test
    #
    # Results:
    #	Returns the number of beads in the BDD for the given expression.

    method beadcount {exprName} {
	tcl::mathop::+ {*}[my profile $exprName]
    }
}

# bdd::foreach_fullsat --
#
#	Expands a term produced by the 'foreach_sat' method on
#	the 'bdd::system' object to list all combinations of a certain
#	set of variables.
#
# Usage:
#	bdd::foreach_fullsat v varList satterm script
#
# Parameters:
#	v - Name of a variable that will hold a list of values of
#	    the variables in a BDD.
#	varList - List of the integer indices of the variables of
#	          interest. All variables of the BDD must appear in
#		  varList, or else foreach_fullsat will execute
#		  redundant calls.
#	satterm - Term produced by the 'foreach_sat' method. The term
#		  will be represented as a dictionary whose keys are
#		  variable indices and whose values are the Boolean values.
#	script - Script that will be executed once per satisfying combination,
#		 with $v set to a list of Boolean values corresponding to the
#		 variable indices in $varList.
#
# Results:
#	None.
#
# Notes:
#	Within the script, 'break', 'continue' and 'return' have their
#	natural meaning. 'return -level 0 -code 6' may also be useful: it
#	translates to causing the 'foreach_fullsat' construct to return
#	with the 'break' return code. Since 'foreach_fullsat' is normally
#	nested directly inside 'foreach_sat', this return causes both
#	loops to break at once.

proc bdd::foreach_fullsat {v varlist satterm script} {
    variable _seq
    upvar 1 $v var
    set coro [coroutine [namespace current]::_fullsat::[incr _seq] \
		  [namespace which ExpandedSatLauncher] $varlist $satterm]
    try {
	while {[llength [set var [$coro]]] != 0} {
	    try {
		uplevel 1 $script
	    } on error {message options} {
		dict incr options -level 1
		return -options $options $message
	    } on return {retval options} {
		dict incr options -level 1
		return -options $options $retval
	    } on break {} {
		break
	    } on continue {} {
		continue
	    } on 6 {} {
		return -code break
	    }
	}
    } finally {
	if {[namespace which $coro] ne {}} {
	    rename $coro {}
	}
    }
}

# bdd::ExpandedSatLauncher --
#
#	Launches a coroutine for expanding a SAT term
#
# Parameters:
#	varList - list of numeric variable indices
#	term - Term to expand
#
# Results:
#	The immediate result is the name of the coroutine.
#	Further results are the rows in the term's truth table.
#	The final result, once all truth table rows are yielded, is the
#	empty list. Upon returning this result, the coroutine terminates.

proc bdd::ExpandedSatLauncher {varlist term} {
    yield [info coroutine]
    ExpandedSat $varlist $term {}
    return {}
}

# bdd::ExpandedSat --
#
#	Recursively expands a SAT term's truth table.
#
# Parameters:
#	varList - List of variables to include in the truth table
#	term - SAT term to expand
#	prefix - Truth table row for variables already expanded.
#
# Results:
#	Yields rows in the truth table. Returns without a result to the
#	caller once the expansion is done.

proc bdd::ExpandedSat {varlist term prefix} {

    # If no variables remain to expand, yield the current truth table row

    if {[llength $varlist] == 0} {
	yield $prefix
	return
    }

    # Pull off one variable to expand

    set restvars [lassign $varlist var]
    if {[dict exists $term $var]} {

	# The term value depends on the variable: make the truth assignment
	# and expand the remaining variables.

	lappend prefix [dict get $term $var]
	ExpandedSat $restvars $term $prefix
    } else {

	# The term value does not depend on the variable. Generate truth
	# table rows for both values of the variable.

	lappend prefix 0
	ExpandedSat $restvars $term $prefix
	lset prefix end 1
	ExpandedSat $restvars $term $prefix

    }
    return
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

package provide tclbdd 0.1
