# datalog.tcl --
#
#	Datalog compiler, implemented in Tcl for a Tcl-hosted runtime
#	engine based on BDD's.
#
# Copyright (c) 2013 by Kevin B. Kenny
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#------------------------------------------------------------------------------

package require Tcl 8.6
package require tclbdd 0.1
package require tclbdd::fddd 0.1
package require coroutine::corovar 1.0
package require coroutine::iterator 1.0
package require grammar::aycock 1.0

namespace import coroutine::corovar::corovar

namespace eval bdd {
    namespace eval datalog {
	variable gensym 0
	namespace export compileProgram
    }
}

# bdd::datalog::lex --
#
#	Lexical analysis for the Datalog compiler.
#
# Parameters:
#	string - Program text
#
# Results:
#	Returns two lists. The first is a list of token types, drawn from
#	!, (, ,, ), =, ., ~, ?, :-, IDENTIFIER, TCLVAR, STRING and INTEGER.
#	The second is a list of token values. Values of most tokens are
#	their text. Quoted strings are backslash-substituted, and Tcl
#	variables have the $ sigil stripped.

proc bdd::datalog::lex {string} {
    set i 0
    regsub -all {\\\n} $string {} string
    set tokens [regexp -all -inline -expanded -- {
	| \s+			# whitespace - ignore
	| %[^\n]*\n?            # % comment - ignore
	| [!(,)=.~?\"]		# punctuation
        | :-			# implicant
        | \$?[^\s!(,)=:.~?\"%]+	# identifier
	| \"(?:[^\"\\]|\\.)+\"  # quoted string
    } $string]
    set types {}
    set values {}
    foreach token $tokens {
	set sigil [string index $token 0]
	if {$sigil in {% { } \t \n}} {
	    continue
	} elseif {$sigil in {! ( , ) : = . ~ ?}} {
	    lappend types $token
	    lappend values $token
	} elseif {$sigil eq "\$"} {
	    lappend types TCLVAR
	    lappend values [list [string range $token 1 end]]
	} elseif {$sigil eq "\""} {
	    lappend types STRING
	    lappend values [subst -novariables -nocommands \
				[string range $token 1 end-1]]
	} elseif {[string is integer -strict $token]} {
	    lappend types INTEGER
	    lappend values $token
	} else {
	    lappend types IDENTIFIER
	    lappend values $token
	}
    }
    return [list $types $values]
}

# Grammar for the Datalog parser

set bdd::datalog::parser \
    [::grammar::aycock::parser \
	 [regsub -all -- {\#[^\n]*\n} {

    # A program comprises a list of statements followed optionally by
    # a query

    program	::=	statements 		{}
    program	::=	statements query	{}
    statements	::=	statements statement	{}
    statements	::=			    	{}

    # A statement is either an assertion or retraction of a clause.
    # A clause is either a rule or a fact. We refactor this into
    # the four cases, 'ruleAssertion', 'factAssertion', 'ruleRetraction'
    # and 'factRetraction' because this is no more complicated and
    # gives slightly easier data manipulation

    statement	::=	factAssertion		{}
    statement	::=	factRetraction		{}
    statement   ::=	ruleAssertion		{}
    statement	::=	ruleRetraction		{}

    factAssertion ::=	fact .
    {
	$clientData assertFact [lindex $_ 0]
    }
    factRetraction ::= fact ~
    {
	$clientData retractFact [lindex $_ 0]
    }
    ruleAssertion ::= rule .
    {
	$clientData assertRule [lindex $_ 0]
    }
    ruleRetraction ::= rule ~
    {
	$clientData retractRule [lindex $_ 0]
    }

    # A query gives a literal to match

    query	::=	pliteral ?
    {
	$clientData addQuery [lindex $_ 0]
    }

    # A fact is just a non-negated literal

    fact	::=	pliteral		{}

    # A rule comprises a head (a fact to be deduced) and a body
    # (the facts to deduce it from)

    rule	::=	head :- body
    {
	linsert [lindex $_ 2] 0 [lindex $_ 0]
    }

    # The head is a single, non-negated literal

    head	::=	pliteral		{}

    # The body is a set of comma-separated, possibly negated subgoals

    body	::=	subgoal
    {
	set _
    }
    body	::=	body , subgoal
    {
	linsert [lindex $_ 0] end [lindex $_ 2]
    }

    # A subgoal is either a literal, or else an equality constraint

    subgoal	::=	literal			{}
    subgoal	::=	equality		{}

    # A literal is a predicate symbol optionally followed by a list of terms

    literal	::=	pliteral		{}
    literal	::=	! pliteral
    {
	list NOT [lindex $_ 1]
    }
    pliteral	::=	predicate_symbol
    {
	list LITERAL [lindex $_ 0]
    }
    pliteral	::=	predicate_symbol ( termlist )
    {
	linsert [lindex $_ 2] 0 LITERAL [lindex $_ 0]
    }
    termlist	::=	term
    {
	set _
    }
    termlist	::=	termlist , term
    {
	linsert [lindex $_ 0] end [lindex $_ 2]
    }

    equality	::=	variable = variable
    {
	list EQUALITY [list VARIABLE [lindex $_ 0]] \
	    [list VARIABLE [lindex $_ 2]]
    }

    equality	::=	variable ! = variable
    {
	list INEQUALITY [list VARIABLE [lindex $_ 0]] \
	    [list VARIABLE [lindex $_ 3]]
    }

    # A predicate symbol is either a bare identifier or a quoted string

    predicate_symbol ::= IDENTIFIER			{}
    predicate_symbol ::= STRING				{}

    # A term is either a variable or a constant

    term	::=	variable
    {
	linsert $_ 0 VARIABLE
    }
    term	::=	constant
    {
	linsert $_ 0 CONSTANT
    }

    # A variable is an identifier or quoted string

    variable	::=	IDENTIFIER 			{}
    variable	::=	STRING				{}

    # A constant is a reference to a Tcl variable or a number

    constant	::=	TCLVAR
    {
	linsert $_ 0 TCLVAR
    }
    constant	::=	INTEGER
    {
	linsert $_ 0 INTEGER
    }
} {}]]

# bdd::datalog::prettyprint-rule --
#
#	Formats a rule for printing.
#
# Usage:
#	bdd::datalog::prettyprint-rule $rule
#
# Parameters:
#	rule - Rule in the parse tree

proc bdd::datalog::prettyprint-rule {rule} {
    set s [prettyprint-literal [lindex $rule 0]]
    set sep :-
    foreach subgoal [lrange $rule 1 end] {
	append s $sep [prettyprint-subgoal $subgoal]
	set sep ,
    }
    return $s
}
proc bdd::datalog::prettyprint-subgoal {subgoal} {
    switch -exact [lindex $subgoal 0] {
	EQUALITY {
	    set s [prettyprint-variable [lindex $subgoal 1]]
	    append s = [prettyprint-variable [lindex $subgoal 2]]
	}
	INEQUALITY {
	    set s [prettyprint-variable [lindex $subgoal 1]]
	    append s != [prettyprint-variable [lindex $subgoal 2]]
	}
	NOT {
	    set s !
	    append s [prettyprint-literal [lindex $subgoal 1]]
	}
	LITERAL {
	    set s [prettyprint-literal $subgoal]
	}
	default {
	    error "Expected subgoal and got $subgoal"
	}
    }
    return $s
}
proc bdd::datalog::prettyprint-literal {literal} {
    # FIXME: May need to quote s (and backslashify its content)
    set s [lindex $literal 1]
    if {[llength $literal] > 2} {
	set sep \(
	foreach t [lrange $literal 2 end] {
	    append s $sep [prettyprint-term $t]
	    set sep ,
	}
	append s \)
    }
    return $s
}

proc bdd::datalog::prettyprint-term {term} {
    switch -exact [lindex $term 0] {
	VARIABLE {
	    return [prettyprint-variable $term]
	}
	CONSTANT {
	    return [prettyprint-constant $term]
	}
	default {
	    error "expected term and got $term"
	}
    }
}
proc bdd::datalog::prettyprint-constant {constant} {
    switch -exact [lindex $constant 1 0] {
	INTEGER {
	    return [lindex $constant 1 1]
	}
	TCLVAR {
	    return \$[list [lindex $constant 1 1]]
	}
    }
}
proc bdd::datalog::prettyprint-variable {variable} {
    # FIXME: May need to quote and backslashify
    return [lindex $variable 1]
}

# bdd::datalog::program --
#
#	Class that exists to hold a program description under construction
#	from the parser.

oo::class create bdd::datalog::program {

    # 'rules' is a list of all the rules in the program, expressed as
    #         parse trees.
    # 'rulesForPredicate' is a dictionary whose keys are predicate names
    #         and whose values are lists of rules that assign a value to the
    #	      given predicate. The lists consist of integer indices into
    #         the 'rules' list.
    # 'factsForPredicate' is a dictionary whose keys are predicate names
    #         and whose values are lists of facts that assign a value to the
    #         given predicate
    # 'outEdgesForPredicate' is a dictionary whose keys are predicate names
    #         and whose values are edges that describe the rules that depend
    #         on the given predicate. Each edge is a tuple:
    #             [0] The name of the predicate being tracked
    #             [1] The name of the predicate on the left hand side of
    #                 the dependent rule
    #		  [2] 1 if the predicate is negated in the rule, 0 otherwise
    #             [3] The dependent rule, as a parse tree
    #             [4] The index of the predicate being tracked within the
    #                 subgoals on the right hand side of the dependent rule.
    # 'query' is a literal giving the query at the end of the program
    #         (if any)
    # 'executionPlan' gives the eventual order of execution of the facts
    #                 and rules. It is a list of tuples:
    #                     RULE literal subgoal subgoal ...
    #		          FACT literal
    #		          LOOP predicate executionPlan
    #                 possibly having 'QUERY literal' at the end.
    # 'intcode' is the execution plan translated to an intermediate code
    #           that expresses the work to be done in terms of relational
    #	        algebra.

    variable \
	rules \
	rulesForPredicate \
	factsForPredicate \
	outEdgesForPredicate \
	query \
	executionPlan \
	intcode

    # Constructor -
    #
    #	Creates an empty program.

    constructor {} {
	set rules {}
	set rulesForPredicate {}
	set factsForPredicate {}
	set outEdgesForPredicate {}
	set executionPlan {}
	set intcode {}
    }

    # gensym -
    #
    #	Generate a unique symbol
    #
    # Results:
    #	Returns a generated symbol

    method gensym {{prefix G}} {
	return ${prefix}[incr ::bdd::datalog::gensym]
    }

    # assertRule -
    #
    #	Semantic action called from the parser when a rule is being asserted
    #
    # Parameters:
    #	rule - Parse tree of the rule
    #
    # Results:
    #	None
    #
    # Side effects:
    #	Adds the rule to the rule list, and the list of rules that compute
    #   its left-hand side. For each predicate on the right-hand side, adds
    #	an edge linking the dependency to the rule.

    method assertRule {rule} {

	# Put the rule in the rule list and the list of rules for
	# the predicate on the left-hand side

	set ruleIndex [llength $rules]
	set lhPredicate [lindex $rule 0 1]
	lappend rules $rule
	dict lappend rulesForPredicate $lhPredicate $ruleIndex

	# Examine the subgoals on the right hand side

	set i 0
	foreach subgoal [lrange $rule 1 end] {
	    incr i
	    switch -exact -- [lindex $subgoal 0] {
		EQUALITY -
		INEQUALITY { 	# does not create a dependency
		    continue
		}
		LITERAL {
		    set dependency [lindex $subgoal 1]
		    set not 0
		}
		NOT {
		    set dependency [lindex $subgoal 1 1]
		    set not 1
		}
		default {
		    error "[info level 0] - can't happen"
		}
	    }

	    # Put the dependency into the edges for the LHS predicate

	    dict lappend outEdgesForPredicate $dependency \
		[list $dependency $lhPredicate $not $rule $i]
	}

	# Make sure that the predicates of all rules appear in
	# the 'outEdgesForPredicate' dictionary.

	if {![dict exists $outEdgesForPredicate $lhPredicate]} {
	    dict set outEdgesForPredicate $lhPredicate {}
	}

	return
    }

    # Method: retractRule
    #
    #	Retracts a rule
    #
    # NOT IMPLEMENTED

    method retractRule {rule} {
	return -code error "Retractions are not currently supported"
    }

    # Method: assertFact
    #
    #	Semantic action called from the parser when a program asserts
    #   a fact.
    #
    # Parameters:
    #	literal - The fact being asserted, expressed as a parse tree
    #
    # Results:
    #	None.
    #
    # Side effects:
    #	Adds the given fact to the list of facts for its predicate.

    method assertFact {literal} {

	# Add the fact to the list of facts for its predicate
	set predicate [lindex $literal 1]
	dict lappend factsForPredicate $predicate $literal
    }

    # Method: retractFact
    #
    #	Retracts a fact
    #
    # NOT IMPLEMENTED

    method retractFact {literal} {
	return -code error "Retractions are not currently supported"
    }

    # Method: addQuery
    #
    #	Adds a query to a program
    #
    # Parameters:
    #	literal - The literal being queried.
    #
    # Results:
    #	None.
    #
    # Side effects:
    #	Sets the program's final query to the given query.

    method addQuery {literal} {
	set query $literal
    }

    # Method: planExecution
    #
    #	Develops an execution plan for the program
    #
    # Parameters:
    #	None.
    #
    # Results:
    #	Returns the execution plan
    #
    # Errors:
    #	Throws an error if the program is not stratifiable.
    #
    # Notes:
    #	The general approach is that the predicate dependency graph is
    #   broken up into strongly connected components. For each component,
    #	in topologic order, if the component consists of a single predicate, 
    #	code is generated for the facts and rules that assign values
    #	to the predicate. If the component contains multiple predicates,
    #	it contains at least one loop. A loop header is identified
    #	heuristically, and an iteration is constructed to compute the
    #	predicate that corresponds to it. That predicate is removed from
    #	the component, and whatever remains of the component is extracted
    #	as a new program and compiled to become the loop body.

    method planExecution {} {

	set executionPlan {}

	# Partition the program into strongly connected components.

	set components {}
	set i 0
	bdd::datalog::scc c $outEdgesForPredicate {
	    lappend components $c
	}

	# Iterate through the components, in dependency order, and
	# plan their execution individually.
	
	foreach component [lreverse $components] {
	    my planExecutionForComponent $component
	}

	# Tack on the query at the end

	if {[info exists query]} {
	    lappend executionPlan [list QUERY $query]
	}

	return $executionPlan

    }

    # Method: planExecutionForComponent
    #
    #	Plans the execution for one strongly-connected component of a
    #	program.
    #
    # Parameters:
    #	component - List of predicates belonging to the component
    #
    # Results:
    #	None.
    #
    # Side effects:
    #	Appends the execution plan for the component to the plan
    #	under construction for the program.
    #
    # Errors:
    #	Throws an error if the program is not stratifiable.
    
    method planExecutionForComponent {component} {

	set loops {}
	foreach predicate $component {
	    foreach fact [my getFactsForPredicate $predicate] {
		lappend executionPlan [list FACT $fact]
	    }
	    foreach ruleNo [my getRulesForPredicate $predicate] {
		set rule [my getRule $ruleNo]
		switch -exact -- [my ruleDependsOn $rule $component] {
		    2 {
			error "The program is not stratifiable.\
                               Check the rule\n\
                               [::bdd::datalog::prettyprint-rule $rule]"
		    }
		    1 {
			lappend loops $rule
		    }
		    0 {
			lappend executionPlan [list RULE $rule]
		    }
		    default {
			error "in planExecutionForComponent: can't happen"
		    }
		}
	    }
	}
	if {[llength $loops] != 0} {
	    lappend executionPlan [my planIteration $component $loops]
	}
    }

    # Method: planIteration
    #
    #	Plans an iteration pattern once a recursive component has been
    #   identified.
    #
    # Parameters:
    #   component - Set of predicates that need to be resolved.
    #	loops - Set of rules that require iteration. All irrelevant rules
    #           have been removed.
    #
    # Results:
    #	Returns the execution plan for the iteration

    method planIteration {component loops} {
	# As a heuristic, iterate over the predicate whose in-degree
	# most exceeds its out-degree. This is the predicate whose deletion
	# will remove the most edges from the dependency graph

	# Score the predicates according to the degrees of the dependency
	# graph.
	set delta [my rankComponentMembers $component $loops]

	# Find the predicate with the high score
	set maxDelta -Inf
	dict for {pred d} $delta {
	    if {$d > $maxDelta} {
		set maxDelta $d
		set toRemove $pred
	    }
	}

	# Make a loop to iterate over that predicate
	try {
	    # Take all the other component members and compile
	    # their rules recursively.
	    set loopBody [::bdd::datalog::program new]
	    foreach rule $loops {
		if {[lindex $rule 0 1] ne $toRemove} {
		    $loopBody assertRule $rule
		}
	    }
	    set bodyCode [$loopBody planExecution]

	    # Append the rules for deriving the current member at
	    # the bottom of the loop.
	    foreach rule $loops {
		if {[lindex $rule 0 1] eq $toRemove} {
		    lappend bodyCode [list RULE $rule]
		}
	    }
	} finally {
	    $loopBody destroy
	}

	return [list LOOP $toRemove $bodyCode]
		    
    }

    # Method: rankComponentMemebers
    #
    #	Ranks members of a connected component in the predicate dependency
    #   graph for selection of loop headers.
    #
    # Parameters:
    #	components - Set of predicates in the connected component
    #	loops - Set of rules in the connected component that must be iterated.
    #
    # Results:
    #	Returns a dictionary whose keys are predicates and whose values are
    #	scores. The high-scoring predicate is the one that will be removed.
    #
    # The heuristic in play is from TODO: [citation needed]. It is to
    # compare the in-degree and out-degree of the predicate in the
    # dependency graph. The one with the highest (in-out) is the one
    # that will remove the most edges from the component if the
    # loop is broken there, and hence is likely to simplify the graph.
    # (The paper quantifies how close the result is to optimum.)

    method rankComponentMembers {component loops} {
	set delta {}
	foreach rule $loops {
	    set lhPredicate [lindex $rule 0 1]
	    foreach subgoal [lrange $rule 1 end] {
		switch -exact -- [lindex $subgoal 0] {
		    EQUALITY - 
		    INEQUALITY {	# does not introduce a dependency
			continue
		    }
		    NOT {
			set rhPredicate [lindex $subgoal 1 1]
		    }
		    LITERAL {
			set rhPredicate [lindex $subgoal 1]
		    }
		    default {
			error "in [info level 0]: can't happen."
		    }
		}
		if {[lsearch -exact $component $rhPredicate] >= 0} {
		    dict incr delta $lhPredicate 1; # edge into lhPredicate
		    dict incr delta $rhPredicate -1; # edge out of rhPredicate
		}
	    }
	}
	return $delta
    }
    

    # Method: ruleDependsOn
    #
    #	Tests if a rule depends on one or more of a set of predicates.
    #
    # Parameters:
    #	rule - Parse tree of the rule
    #	predicates - List of predicate names
    #
    # Results:
    #	Returns 2 if the rule depends on one of the predicates in negated
    #   form, 1, if the rule depends on one of the predicates only in
    #   non-negated form, 0 if the rule has no dependency on the predicates

    method ruleDependsOn {rule predicates} {
	set result 0
	foreach subgoal [lrange $rule 1 end] {
	    if {[set r [my subgoalDependsOn $subgoal $predicates]]
		> $result} {
		set result $r
	    }
	}
	return $result
    }

    # Method: subgoalDependsOn
    #
    #	Tests if a subgoal depends on one or more of a set of predicates.
    #
    # Parameters:
    #	rule - Parse tree of the subgoal
    #	predicates - List of predicate names
    #
    # Results:
    #	Returns 2 if the rule depends on one of the predicates in negated
    #   form, 1, if the rule depends on one of the predicates only in
    #   non-negated form, 0 if the rule has no dependency on the predicates

    method subgoalDependsOn {subgoal predicates} {
	switch -exact -- [lindex $subgoal 0] {
	    EQUALITY -
	    INEQUALITY {
		return 0
	    }
	    NOT {
		if {[my subgoalDependsOn [lindex $subgoal 1] $predicates]} {
		    return 2
		} else {
		    return 0
		}
	    }
	    LITERAL {
		if {[lsearch -exact $predicates [lindex $subgoal 1]] >= 0} {
		    return 1
		} else {
		    return 0
		}
	    }
	}
    }

    # Method: translateExecutionPlan
    #
    #	Once an execution plan has been constructed, translates it to
    #	three-address code.
    #
    # Parameters:
    #	db - Database on which the plan will be executed. The input and
    #	     output relations, and all columns appearing in the code,
    #	     must be defined.
    #	plan - Execution plan, a list of FACT, RULE, LOOP, and QUERY
    #	       subplans, as returned from 'planExecution'
    #
    # Results:
    #	Returns a list of three-address instructions.

    method translateExecutionPlan {db plan} {
	foreach step $plan {
	    switch -exact -- [lindex $step 0] {
		FACT {
		    my translateFact $db [lindex $step 1]
		}
		LOOP {
		    my translateLoop $db [lindex $step 1] [lindex $step 2]
		} 
		QUERY {
		    my translateQuery $db [lindex $step 1]
		}
		RULE {
		    my translateRule $db [lindex $step 1]
		}
		default {
		    error "in translateExecutionPlan: can't happen"
		}
	    }
	}
	return $intcode
    }

    # Method: translateFact
    #
    #	Translates a fact in the execution plan to three-address code
    #
    # Parameters:
    #	db - Database on which the plan will be executed. The input and
    #	     output relations, and all columns appearing in the code,
    #	     must be defined.
    #	fact - Literal representing the fact to be translated.
    #	cols - If supplied, list of names of the columns of the
    #	       relation representing $fact's predicate.
    #
    # Results:
    #	None.
    #
    # Side effects:
    #	Appends three-addres instructions to 'intcode'

    method translateFact {db fact {cols {}}} {

	set predicate [lindex $fact 1]

	# Retrieve the set of columns in the output relation if not supplied
	# by the caller.

	if {$cols eq {}} {
	    db relationMustExist $predicate
	    set cols [$db columns $predicate]
	    if {[llength $cols] != [llength $fact]-2} {
		set ppfact [bdd::datalog::prettyprint-literal $fact]
		return -code error \
		    -errorCode [list DATALOG wrongColumns $predicate $ppfact] \
		    "$predicate has a different number of columns from $ppfact"
	    }
	}

	# Examine the terms of the literal, and extract the list of
	# columns for which specific vales have been supplied, and the
	# list of columns that have 'don't care' values: unbound variables
	# or _.

	set probeColumns {}
	set probeValues {}
	set dontCareColumns {}
	foreach term [lrange $fact 2 end] col $cols {
	    switch -exact [lindex $term 0] {
		CONSTANT {
		    lappend probeColumns $col
		    lappend probeValues [lindex $term 1]
		}
		VARIABLE {
		    if {[lindex $term 1] ne {_}} {
			set ppfact [bdd::datalog::prettyprint-literal $fact]
			puts stderr "warning: unused variable [lindex $term 1]\
                                     in fact $ppfact."
		    }
		    lappend dontCareColumns $col
		}
	    }
	}

	# Complain if no variables in the literal are bound.

	if {$probeColumns eq {}} {
	    set ppfact [bdd::datalog::prettyprint-literal $fact]
	    puts stderr "warning: fact $ppfact. asserts the universal set"
	    lappend intcode \
		[list SET $predicate _]
	} else {

	    # If there are 'don't cares', then make a relation for the
	    # bound values, a universal relation for the 'don't cares',
	    # join the two, and then union the result into the relation
	    # under construction.

	    if {$dontCareColumns ne {}} {
		set probeRelation [my gensym #T]
		set dontCareRelation [my gensym #T]
		set joinedRelation [my gensym #T]
		lappend intcode \
		    [list RELATION $probeRelation $probeColumns] \
		    [list LOAD $probeRelation $probeValues] \
		    [list RELATION $dontCareRelation $dontCareColumns] \
		    [list SET $dontCareRelation _] \
		    [list RELATION $joinedRelation $cols] \
		    [list JOIN $joinedRelation \
			 $probeRelation $dontCareRelation] \
		    [list UNION $predicate $predicate $joinedRelation]
	    } else {

		# If there are no 'don't cares', then load the literal
		# directly into the relation under construction.

		lappend intcode \
		    [list LOAD $predicate $probeValues]
	    }
	}
    }

    # Method: translateLoop
    #
    #	Generates three-address code for rules with a cyclic dependency,
    #	iterating to a fixed point.
    #
    # Parameters:
    #	db - Database on which the plan will be executed. The input and
    #	     output relations, and all columns appearing in the code,
    #	     must be defined.
    #   predicate - Predicate to test for a fixed point.
    #	body - Execution plan for the loop body.
    #
    # Results:
    #	None.
    #
    # Side effects:
    #	Appends three-address instructions to 'intcode'

    method translateLoop {db predicate body} {

	db relationMustExist $predicate
	set cols [$db columns $predicate]
	set comparison [my gensym #T]

	# Create a temporary relation to record the old value of
	# predicate for convergence testing.
	lappend intcode [list RELATION $comparison $cols]

	# Mark the top of the loop
	set where [llength $intcode]
	lappend intcode BEGINLOOP

	# Save the value of the relation being iterated
	lappend intcode [list SET $comparison $predicate]

	# Translate the loop body
	my translateExecutionPlan $db $body

	# Translate the loop footer.
	lappend intcode [list ENDLOOP $comparison $predicate $where]
    }

    method translateQuery {db query} {
	lassign [my translateSubgoal $db $query {} {}] tempRelation tempColumns
	lappend intcode [list RESULT $tempRelation $tempColumns]
	
    }

    method translateRule {db rule} {
	set tempRelation {}
	set tempColumns {}
	foreach subgoal [lrange $rule 1 end] {
	    lassign [my translateSubgoal \
			 $db $subgoal $tempRelation $tempColumns] \
		tempRelation tempColumns
	}
	my translateRuleHead $db [lindex $rule 0] $tempRelation $tempColumns
    }

    method translateSubgoal {db subgoal dataSoFar columnsSoFar} {
	switch -exact [lindex $subgoal 0] {
	    NOT {
		lassign \
		    [my translateLiteral $db \
			 [lindex $subgoal 1] $dataSoFar $columnsSoFar] \
		    subgoalRelation subgoalColumns
		tailcall my translateSubgoalEnd $db ANTIJOIN \
		    $dataSoFar $columnsSoFar $subgoalRelation $subgoalColumns
	    }
	    EQUALITY -
	    INEQUALITY {
		tailcall my translateEquality $db [lindex $subgoal 0] \
		    [lindex $subgoal 1] [lindex $subgoal 2] \
		    $dataSoFar $columnsSoFar
	    }
	    LITERAL {
		lassign \
		    [my translateLiteral \
			 $db $subgoal $dataSoFar $columnsSoFar] \
		    subgoalRelation subgoalColumns
		tailcall my translateSubgoalEnd $db JOIN \
		    $dataSoFar $columnsSoFar $subgoalRelation $subgoalColumns
	    }
	    default {
		error "in translateSubgoal: can't happen"
	    }
	}
    }

    method translateEquality {db operation var1 var2 dataSoFar columnsSoFar} {
	set col1 [lindex $var1 1]
	set col2 [lindex $var2 1]
	set equality [my gensym #T]
	lappend intcode \
	    [list RELATION $equality [list $col1 $col2]] \
	    [list $operation $equality $col1 $col2]
	if {$columnsSoFar eq {}} {
	    return [list $equality [list $col1 $col2]]
	} else {
	    set joined [my gensym #T]
	    lappend columnsSoFar $col1 $col2
	    set columnsSoFar [lsort -dictionary -unique $columnsSoFar]
	    lappend intcode \
		[list RELATION $joined $columnsSoFar] \
		[list JOIN $joined $dataSoFar $equality]
	    return [list $joined $columnsSoFar]
	}
    }

    method translateLiteral {db literal dataSoFar columnsSoFar} {
	set predicate [lindex $literal 1]
	db relationMustExist $predicate
	set cols [db columns $predicate]
	if {[llength $cols] != [llength $literal]-2} {
	    set pplit [bdd::datalog::prettyprint-literal $literal]
	    return -code error \
		-errorCode [list DATALOG wrongColumns $predicate $pplit] \
		"$predicate has a different number of columns from $pplit"
	}
	set selector [my gensym #T]
	set selectLiteral [list LITERAL $selector]
	set needSelect 0
	set needProject 0
	set projector [my gensym #T]
	set projectColumns {}
	set renamed [my gensym #T]
	set renamedFrom {}
	set renamedTo {}
	foreach term [lrange $literal 2 end] col $cols {
	    switch -exact -- [lindex $term 0] {
		CONSTANT {
		    lappend selectLiteral $term
		    set needSelect 1
		    set needProject 1
		}
		VARIABLE {
		    set varName [lindex $term 1]
		    lappend selectLiteral {VARIABLE _}
		    if {$varName eq {_}} {
			set needProject 1
 		    } else {
			lappend projectColumns $col
			lappend renamedColumns $varName
			if {$varName eq $col} {
			    # no rename needed
			} else {
			    lappend renamedFrom $col
			    lappend renamedTo $varName
			}
		    }
		}
	    }
	}

	if {$needSelect} {
	    lappend intcode [list RELATION $selector $cols]
	    my translateFact $db $selectLiteral $cols
	    lappend intcode [list JOIN $selector $selector $predicate]
	    set projectSource $selector
	} else {
	    set projectSource $predicate
	}
	if {$needProject} {
	    lappend intcode \
		[list RELATION $projector $projectColumns] \
		[list PROJECT $projector $projectSource]
	    set renameSource $projector
	} else {
	    set renameSource $projectSource
	}
	if {[llength $renamedFrom] > 0} {
	    lappend intcode [list RELATION $renamed $renamedColumns]
	    set renameCommand [list RENAME $renamed $renameSource]
	    foreach to $renamedTo from $renamedFrom {
		lappend renameCommand $to $from
	    }
	    lappend intcode $renameCommand
	    set result $renamed
	} else {
	    set result $renameSource
	}
	return [list $result $renamedColumns]
    }

    method translateSubgoalEnd {db operation 
				dataSoFar columnsSoFar
				dataThisOp columnsThisOp} {
	if {$dataSoFar eq {}} {
	    if {$operation eq {ANTIJOIN}} {
		lappend intcode [list NEGATE $dataThisOp $dataThisOp]
	    }
	    set resultRelation $dataThisOp
	    set resultColumns $columnsThisOp
	} else {
	    set resultColumns $columnsSoFar
	    lappend resultColumns {*}$columnsThisOp
	    set resultColumns [lsort -unique -dictionary $resultColumns]
	    set resultRelation [my gensym #T]
	    lappend intcode \
		[list RELATION $resultRelation $resultColumns] \
		[list $operation $resultRelation $dataSoFar $dataThisOp]
	}
	return [list $resultRelation $resultColumns]
    }

    method translateRuleHead {db literal sourceRelation sourceColumns} {
	set predicate [lindex $literal 1]
	db relationMustExist $predicate
	set cols [db columns $predicate]
	if {[llength $cols] != [llength $literal]-2} {
	    set pplit [bdd::datalog::prettyprint-literal $literal]
	    return -code error \
		-errorCode [list DATALOG wrongColumns $predicate $pplit] \
		"$predicate has a different number of columns from $pplit"
	}

	# Analyze the head of the rule
	# Complain about columns in literal that are not in sourceColumns.

	set pplit [bdd::datalog::prettyprint-literal $literal]
	set destColumn {}
	set dontCareColumns {}
	set renamedFrom {}
	set renamedTo {}
	set constant [my gensym #T]
	set constantColumns {}
	set constantLiteral [list LITERAL $constant]
	foreach destTerm [lrange $literal 2 end] col $cols {
	    switch -exact -- [lindex $destTerm 0] {
		CONSTANT {
		    lappend constantColumns $col
		    lappend constantLiteral $destTerm
		}
		VARIABLE {
		    set vname [lindex $destTerm 1]
		    if {$vname eq {_}} {
			lappend dontCareColumns $col
		    } else {
			if {$col ne $vname} {
			    lappend renamedFrom $vname
			    lappend renamedTo $col
			}
			if {[lsearch -exact $sourceColumns $vname] < 0} {
			    return -code error \
				-errorCode \
				[list DATALOG MissingVariable $vname $pplit] \
				"variable $vname appears in the head $pplit\
                                 but not in the body $sourceColumns"
			}
			dict set destColumn $vname {}
			lappend renamedColumns $col
		    }
		}
	    }
	}

	# Project away unused columns in sourceColumns.
	set needProject 0
	set projector [my gensym #T]
	set projectColumns {}
	foreach col $sourceColumns {
	    if {[dict exists $destColumn $col]} {
		lappend projectColumns $col
	    } else {
		set needProject 1
	    }
	}
	if {$needProject} {
	    lappend intcode \
		[list RELATION $projector $projectColumns] \
		[list PROJECT $projector $sourceRelation]
	    set renameSource $projector
	} else {
	    set renameSource $sourceRelation
	}

	# Rename columns from literal to destination.
	if {[llength $renamedFrom] > 0} {
	    set renamed [my gensym \#T]
	    lappend intcode [list RELATION $renamed $renamedColumns]
	    set renameCommand [list RENAME $renamed $renameSource]
	    foreach to $renamedTo from $renamedFrom {
		lappend renameCommand $to $from
	    }
	    lappend intcode $renameCommand
	    set joinSource $renamed
	} else {
	    set joinSource $renameSource
	}

	# Join with any constants

	set joinColumns $renamedColumns
	if {[llength $constantColumns] > 0} {
	    lappend intcode [list RELATION $constant $constantColumns]
	    my translateFact $db $constantLiteral $constantColumns
	    lappend joinColumns {*}$constantColumns
	    set joined [my gensym #T]
	    lappend intcode \
		[list RELATION $joined $joinColumns] \
		[list JOIN $joined $joinSource $constant]
	    set joinSource $joined
	}

	# Join with any don't-cares

	if {[llength $dontCareColumns] > 0} {
	    set dontCareRelation [my gensym #T]
	    lappend intcode \
		[list RELATION $dontCareRelation $dontCareColumns] \
		[list SET $dontCareRelation _]
	    lappend joinColumns {*}$dontCareColumns
	    set joined [my gensym #T]
	    lappend intcode \
		[list RELATION $joined $joinColumns] \
		[list JOIN $joined $joinSource $dontCareRelation]
	    set joinSource $joined

	}

	# Union the result into the destination
	lappend intcode [list UNION $predicate $predicate $joinSource]
	
    }

    method generateCode {db icode args} {

	set loaders {}

	set prologue \n
	set body \n
	set epilogue \n

	set ind0 {    }
	set ind {    }

	foreach instr $icode {
	    switch -exact -- [lindex $instr 0] {
		RELATION {
		    $db relation [lindex $instr 1] {*}[lindex $instr 2]
		    append prologue $ind0 [$db set [lindex $instr 1] {}] \n
		    append epilogue $ind0 [$db set [lindex $instr 1] {}] \n
		}
		
		ANTIJOIN {
		    append body $ind \
			[$db antijoin {*}[lrange $instr 1 end]] \n
		}
		BEGINLOOP {
		    append body $ind "while 1 \{\n"
		    set ind "$ind    "
		}
		ENDLOOP {
		    set command [$db === [lindex $instr 1] [lindex $instr 2]]
		    append body \
			$ind if { } \{ \[ $command \] \} { } break \n
		    set ind [string replace $ind end-3 end]
		    append body $ind "\}" \n
		}
		EQUALITY {
		    append body $ind \
			[$db equate {*}[lrange $instr 1 end]] \n
		}
		INEQUALITY {
		    append body $ind \
			[$db inequality {*}[lrange $instr 1 end]] \n
		}
		JOIN {
		    append body $ind \
			[$db join {*}[lrange $instr 1 end]] \n
		}
		LOAD {
		    # append body $ind # $instr \n
		    set relation [lindex $instr 1]
		    if {![dict exists $loaders $relation]} {
			dict set loaders $relation [$db loader $relation]
		    }
		    append body $ind \
			[dict get $loaders $relation]
		    foreach val [lindex $instr 2] {
			switch -exact -- [lindex $val 0] {
			    INTEGER {
				append body { } [lindex $val 1]
			    }
			    TCLVAR {
				append body { } \$ [lindex $val 1]
			    }
			    default {
				error "in generateCode: can't happen"
			    }
			}
		    }
		    append body \n
		}
		NEGATE {
		    append body $ind \
			[$db negate {*}[lrange $instr 1 end]] \n
		}
		PROJECT {
		    append body $ind \
			[$db project {*}[lrange $instr 1 end]] \n
		}
		RENAME {
		    append body $ind \
			[$db replace {*}[lrange $instr 1 end]] \n
		}
		SET {
		    append body $ind \
			[$db set {*}[lrange $instr 1 end]] \n
		}
		UNION {
		    append body $ind \
			[$db union {*}[lrange $instr 1 end]] \n
		}

		RESULT {
		    if {[llength $args] != 2} {
			error "wrong # args"; # TODO - better reporting
		    }
		    append body \
			[list $db enumerate [lindex $args 0] \
			     [lindex $instr 1] \
			     [lindex $args 1]] \n
		}

		default {
		    error "in generateCode: can't happen"
		}
	    }

	}
	return $prologue$body$epilogue

    }

    method getRule {ruleNo} {
	return [lindex $rules $ruleNo]
    }

    method getRules {} {
	return $rules
    }

    method getRulesFor {} {
	return $rulesForPredicate
    }

    method getRulesForPredicate {predicate} {
	if {[dict exists $rulesForPredicate $predicate]} {
	    return [dict get $rulesForPredicate $predicate]
	} else {
	    return {}
	}
    }

    method getEdges {} {
	return $outEdgesForPredicate
    }

    method getFacts {} {
	return $factsForPredicate
    }

    method getFactsForPredicate {predicate} {
	if {[dict exists $factsForPredicate $predicate]} {
	    return [dict get $factsForPredicate $predicate]
	} else {
	    return {}
	}
    }
}

# bdd::datalog::scc --
#
#	Partiton the predicate dependency graph into strongly connected 
#	components.
#
#
# Usage:
#	bdd::datalog::scc v $outedges script
#
# Parameters:
#	v        - Name of a variable in the caller's scope that will
#		   receive each component in turn. The components are
#		   lists of the predicate names.
#	outedges - Dictionary containing the adjacency lists. The keys
#		   of the dictionary are the names of predicates. The
#		   values are lists of edges. Each edge is a tuple.
#		   The first two elements of the tuple are the from-predicate
#		   and to-predicate. The remaining elements are not used
#		   in this procedure.
#	script   - Script that will be executed once on each strongly
#		   connected component, with 'v' set to the list of 
#		   names of predicates that belong to the component.
#
# Results:
#	None.
#
# Side effects:
#	Partitions the dependency graph into strongly connected components
#	and runs the given script on each, with 'v' set to the list of nodes.

proc bdd::datalog::scc {v outedges script} {
    tailcall coroutine::iterator::foreach $v \
	[list bdd::datalog::SCC_coro $outedges] \
	$script
}

# bdd::datalog::SCC_coro --
#
#	Main procedure of the coroutine that partitions the dependency
#	graph into strongly connected components.
#
# Usage:
#	coroutine $name bdd::datalog::SCC_coro $outedges
#
# Parameters:
#	outedges - Dictionary containing the adjacency lists. The keys
#		   of the dictionary are the names of predicates. The
#		   values are lists of edges. Each edge is a tuple.
#		   The first two elements of the tuple are the from-predicate
#		   and to-predicate. The remaining elements are not used
#		   in this procedure.
#
# Results:
#	Yields the sets of nodes that form strongly connected components.

proc bdd::datalog::SCC_coro {outedges} {
    # outedges is coroutine-global

    # Coroutine-global variables:
    set index 0;		# Coroutine global: Current node's index
    set S {};			# Coroutine global: Stack of nodes on the
    				# path from a root to the current node
    set vindex {};		# Coroutine global: Dictionary whose keys are
    				# node names, and whose values are node indices
    set lowlink {};		# Coroutine global: Dictionary whose keys are
    				# node names, and whose values are node indices
    				# of backward edges in the graph

    # Visit each node and run Tarjan's algorithm recursively on it.
    # When every node is visited, all components will have been listed.
    dict for {v edges} $outedges {
	if {![dict exists $vindex $v]} {
	    SCC_coro_worker $v $edges
	}
    }

    return
}

# bdd::datalog::SCC_coro_worker --
#
#	Visits a single node in Tarjan's algorithm for strongly
#	connected components.
#
# Parameters:
#	v     - Name of the node being visited.
#	edges - List of edges whose origin is the node
#
# Results:
#	None.
#
# This procedure performs a depth-first traversal, identifying back edges
# and strongly connected components.

proc bdd::datalog::SCC_coro_worker {v edges} {
    corovar outedges;		# Adjacency lists
    corovar index;		# Index of next unexamined node
    corovar S;			# Stack of nodes traversed from root to current
    corovar vindex;		# Dictionary mapping node to node index
    corovar lowlink;		# Dictionary mapping node to the other
    				# end of a back edge.


    # Set the index and lowlink of the node to point to itself, and put
    # the node on the stack.
    dict set vindex $v $index
    dict set lowlink $v $index
    incr index
    lappend S $v

    # Examine the successor nodes, testing whether they have yet been visited.
    foreach edge $edges {
	lassign $edge from w 
	if {![dict exists $vindex $w]} {

	    # Successor has not been visited. Visit it now.
	    SCC_coro_worker $w [dict get $outedges $w]
	    dict set lowlink $v \
		[expr {min([dict get $lowlink $v],
			   [dict get $lowlink $w])}]

	} elseif {[lsearch -exact $S $w] >= 0} {

	    # Successor has been visited, is stacked and hence must 
	    # belong to the current component. Keep track of the earliest
	    # visited successor.
	    dict set lowlink $v \
		[expr {min([dict get $lowlink $v],
			   [dict get $vindex $w])}]
	}
    }

    if {[dict get $lowlink $v] == [dict get $vindex $v]} {
	# v is a root node of a strongly connected component.
	# Unstack the component out to the root, and yield it.

	set component {}
	while {1} {
	    set w [lindex $S end]
	    set S [lrange $S[set S {}] 0 end-1]
	    lappend component $w
	    if {$w eq $v} break
	}
	yield $component

    }
    return
}

proc bdd::datalog::compileProgram {db prelude programText args} {

    set postlude [lindex $args end]

    variable parser

    try {

	set program [bdd::datalog::program new]

	# Do lexical analysis of the program
	lassign [lex $programText] tokens values
	
	# Parse the program
	set parseTree [$parser parse $tokens $values $program]

	# Plan the execution
	set plan [$program planExecution]

	# Translate the execution plan to relational algebra
	set intcode [$program translateExecutionPlan $db $plan]

	# Generate code
	append result \
	    $prelude \n \
	    [$program generateCode $db $intcode {*}[lrange $args 0 end-1]] \n \
	    $postlude

    } finally {

	$program destroy

    }
    return $result

}

package provide tclbdd::datalog 0.1
