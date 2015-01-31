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
#
# Results:
#	Returns the formatted string.

proc bdd::datalog::prettyprint-rule {rule} {
    set s [prettyprint-literal [lindex $rule 0]]
    set sep :-
    foreach subgoal [lrange $rule 1 end] {
	append s $sep [prettyprint-subgoal $subgoal]
	set sep ,
    }
    return $s
}

# bdd::datalog::prettyprint-subgoal --
#
#	Formats a subgoal for printing.
#
# Usage:
#	bdd::datalog::prettyprint-subgoal $subgoal
#
# Parameters:
#	subgoal - Subgoal (EQUALITY, INEQUALITY, NOT or LITERAL) to be
#		  printed, expressed as a parse tree.
#
# Results:
#	Returns the formatted string.

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

# bdd::datalog::prettyprint-literal --
#
#	Formats a literal for printing.
#
# Usage:
#	bdd::datalog::prettyprint-literal $literal
#
# Parameters:
#	literal - Literal (LITERAL relation ?term...?) to be printed,
#                 expressed as a parse tree.
#
# Results:
#	Returns the formatted string.

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

# bdd::datalog::prettyprint-term --
#
#	Formats a term for printing.
#
# Usage:
#	bdd::datalog::prettyprint-term $term
#
# Parameters:
#	term - Term (VARIABLE or CONSTANT) expressed as a parse tree.
#
# Results:
#	Returns the formatted string.

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

# bdd::datalog::prettyprint-constant --
#
#	Formats a constant for printing.
#
# Usage:
#	bdd::datalog::prettyprint-constant $term
#
# Parameters:
#	term - Term (CONSTANT {INTEGER value} or CONSTANT {TCLVAR name})
#              to be formatted.
#
# Results:
#	Returns the formatted string.

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

# bdd::datalog::prettyprint-variable --
#
#	Formats a variable for printing.
#
# Usage:
#	bdd::datalog::prettyprint-variable $term
#
# Parameters:
#	term - Term (VARIABLE name) to be formatted.
#
# Results:
#	Returns the formatted string.

proc bdd::datalog::prettyprint-variable {variable} {
    # FIXME: May need to quote and backslashify
    return [lindex $variable 1]
}

# bdd::datalog::program --
#
#	Class that exists to hold a program description under construction
#	from the parser.

oo::class create bdd::datalog::program {

    # 'db' is the name of the database we're compiling against
    #
    # 'rules' is a list of all the rules in the program, expressed as
    #         parse trees.
    #
    # 'rulesForPredicate' is a dictionary whose keys are predicate names
    #         and whose values are lists of rule numbers of
    #	      rules that have the given predicate on the
    #	      left hand side.
    #
    # factsForPredicate' is a dictionary whose keys are predicate names
    #         and whose values are lists of facts that assign a value to the
    #         given predicate
    #
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
    #
    # 'query' is a literal giving the query at the end of the program
    #         (if any)
    #
    # 'executionPlan' gives the eventual order of execution of the facts
    #                 and rules. It is a list of tuples:
    #                     RULE literal subgoal subgoal ...
    #		          FACT literal
    #		          LOOP predicate executionPlan
    #                 possibly having 'QUERY literal' at the end.
    #
    # 'intcode' is the execution plan translated to an intermediate code
    #           that expresses the work to be done in terms of relational
    #	        algebra.
    #
    # The language of the intermediate code is that it is a list of 
    # instructions, each of which is itself a list comprising an operation
    # and arguments.  Instructions that are currently recognized include:
    #
    # RELATION name ?column...?
    #	This is a declaration, rather than an instruction. It describes
    #   that a relation has a given set of columns. As a side effect, the
    #   relation is cleared (set to the empty set of tuples) before and
    #   after the program executes.
    #
    # ANTIJOIN outputRelation inputRelation1 inputRelation2
    #	When executed, this instruction sets the output relation to the
    #   antijoin of the two input relations.
    #
    # BEGINLOOP
    #   Begins a loop. All loops in the generated code are of the "iterate
    #   until convergence" type: they test at the bottom of the loop and
    #   run as long as something changes
    #
    # ENDLOOP relation1 relation2
    #   Closes a loop begun with BEGINLOOP. The loop runs until the contents
    #   of relation1 and relation2 are identical (===).
    #
    # EQUALITY relation column1 column2
    #   Sets the given relation to the set of tuples in which column1 and
    #   column2 have equal values.
    #
    # INEQUALITY relation column1 column2
    #   Sets the given relation to the set of tuples in which column1 and
    #   column2 have distinct values.
    #
    # JOIN outputRelation inputRelation1 inputRelation2
    #   Sets the given output relation to the relational join of the two
    #   input relations.
    #
    # LOAD outputRelation ?value...?
    #   Adds a single tuple to the given output relation. The 'value' arguments
    #   give the column values in order. Each argument is a two-element list:
    #	    INTEGER intval
    #		intval must be an integer at most the same width as the
    #		corresponding column. Its value will be used as the value
    #		in the tuple
    #	    TCLVAR varname
    #		The Tcl variable named 'varname' will be used for the value
    #		in the tuple. It must contain an integer at most the same width
    #		as the corresponding column.
    #
    # NEGATE outputRelation inputRelation
    #	Sets the output relation to the set of all tuples NOT present in the
    #   input relation.
    #
    # PROJECT outputRelation inputRelation
    #	Initializes the output relation, whose columns must be a subset
    #	of the columns of the input relation, by projecting away any unused
    #   columns of the input relation.
    #
    # RENAME outputRelation inputRelation ?outputVar inputVar?...
    #	Sets the output relation's tuples to the tuples of the input relation,
    #	with each variable named by an 'inputVar' replaced with the
    #   variable named by the corresponding 'outputVar'.
    #
    # SET outputRelation inputRelation
    #	Copies the given input relation to the given output relation.
    #
    # UNION outputRelation inputRelation1 inputRelation2
    #	Sets the ouput relation to the union of the two given input relations.
    #
    # RESULT relation
    #	Must be the last instruction in the list. Sets up to enumerate the
    #   tuples in the given relation as the result of a Datalog program.

    variable \
	db \
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
    #
    # Arguments:
    #	db_ - Name of the database being compiled agains

    constructor {db_} {
	set db $db_
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

	# Make sure that the predicate exists in the 'outEdgesForPredicate'
	# dictionary.
	if {![dict exists $outEdgesForPredicate $predicate]} {
	    dict set outEdgesForPredicate $predicate {}
	}
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
	set loopBody [::bdd::datalog::program new $db]
	try {
	    # Take all the other component members and compile
	    # their rules recursively.
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
    #	plan - Execution plan, a list of FACT, RULE, LOOP, and QUERY
    #	       subplans, as returned from 'planExecution'
    #
    # Results:
    #	Returns a list of three-address instructions.

    method translateExecutionPlan {plan} {
	foreach step $plan {
	    switch -exact -- [lindex $step 0] {
		FACT {
		    my translateFact [lindex $step 1]
		}
		LOOP {
		    my translateLoop [lindex $step 1] [lindex $step 2]
		} 
		QUERY {
		    my translateQuery [lindex $step 1]
		}
		RULE {
		    my translateRule [lindex $step 1]
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
    #	fact - Literal representing the fact to be translated.
    #	cols - If supplied, list of names of the columns of the
    #	       relation representing $fact's predicate.
    #
    # Results:
    #	None.
    #
    # Side effects:
    #	Appends three-addres instructions to 'intcode'

    method translateFact {fact {cols {}}} {

	set predicate [lindex $fact 1]

	# Retrieve the set of columns in the output relation if not supplied
	# by the caller.

	if {$cols eq {}} {
	    $db relationMustExist $predicate
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
    #   predicate - Predicate to test for a fixed point.
    #	body - Execution plan for the loop body.
    #
    # Results:
    #	None.
    #
    # Side effects:
    #	Appends three-address instructions to 'intcode'

    method translateLoop {predicate body} {

	$db relationMustExist $predicate
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
	my translateExecutionPlan $body

	# Translate the loop footer.
	lappend intcode [list ENDLOOP $comparison $predicate $where]
    }

    # Method: translateQuery
    #
    #	Generates three-address code to return the result of a Datalog query
    #
    # Parameters:
    #	query - Parse tree of the query
    #
    # Results:
    #	None.
    #
    # Side effects:
    #	Appends three-address instructions to 'intcode'

    method translateQuery {query} {
	lassign [my translateSubgoal $query {} {}] tempRelation tempColumns
	lappend intcode [list RESULT $tempRelation $tempColumns]
	
    }

    # Method: translateRule
    #
    #	Generates three-address code to evaluate a Datalog rule
    #
    # Parameters:
    #	rule - Parse tree of the rule
    #
    # Results:
    #	None.
    #
    # Side effects:
    #	Appends three-address instructions to 'intcode'

    method translateRule {rule} {
	set tempRelation {}
	set tempColumns {}
	foreach subgoal [lrange $rule 1 end] {
	    lassign [my translateSubgoal $subgoal $tempRelation $tempColumns] \
		tempRelation tempColumns
	}
	my translateRuleHead [lindex $rule 0] $tempRelation $tempColumns
    }

    # Method: translateSubgoal
    #
    #	Generates three-address code to evaluate a subgoal within a 
    #   Datalog rule
    #
    # Parameters:
    #	subgoal - Parse tree of the subgoal
    #	dataSoFar - Name of a relation that holds the result of evaluating
    #               the subgoals to the left of this subgoal
    #   columnsSoFar - List of column names present in 'dataSoFar'
    #
    # Results:
    #   Returns a two element list consisting of the name of the relation
    #   representing the partly-translated rule, and the names of the
    #   columns in that relation
    #
    # Side effects:
    #	Appends three-address instructions to 'intcode'

    method translateSubgoal {subgoal dataSoFar columnsSoFar} {

	# Dispatch according to the type of the subgoal
	switch -exact [lindex $subgoal 0] {
	    NOT {
		lassign \
		    [my translateLiteral \
			 [lindex $subgoal 1] $dataSoFar $columnsSoFar] \
		    subgoalRelation subgoalColumns
		tailcall my translateSubgoalEnd ANTIJOIN \
		    $dataSoFar $columnsSoFar $subgoalRelation $subgoalColumns
	    }
	    EQUALITY -
	    INEQUALITY {
		tailcall my translateEquality [lindex $subgoal 0] \
		    [lindex $subgoal 1] [lindex $subgoal 2] \
		    $dataSoFar $columnsSoFar
	    }
	    LITERAL {
		lassign \
		    [my translateLiteral $subgoal $dataSoFar $columnsSoFar] \
		    subgoalRelation subgoalColumns
		tailcall my translateSubgoalEnd JOIN \
		    $dataSoFar $columnsSoFar $subgoalRelation $subgoalColumns
	    }
	    default {
		error "in translateSubgoal: can't happen"
	    }
	}
    }

    # Method: translateEquality
    #
    #	Generates three-address code to evaluate a subgoal of the
    #   form 'a==b' or 'a!=b' within a Datalog rule
    #
    # Parameters:
    #	operation - EQUALITY or INEQUALITY depending on the operator encountered
    #   var1 - {VARIABLE name}, where 'name' is the left hand variable name
    #   var2 - {VARIABLE name}, where 'name' is the right hand variable name
    #	dataSoFar - Name of a relation that holds the result of evaluating
    #               the subgoals to the left of this subgoal
    #   columnsSoFar - List of column names present in 'dataSoFar'
    #
    # Results:
    #   Returns a two element list consisting of the name of the relation
    #   representing the partly-translated rule, and the names of the
    #   columns in that relation
    #
    # Side effects:
    #	Appends three-address instructions to 'intcode'

    method translateEquality {operation var1 var2 dataSoFar columnsSoFar} {
	set col1 [lindex $var1 1]
	set col2 [lindex $var2 1]
	set equality [my gensym #T]
	lappend intcode \
	    [list RELATION $equality [list $col1 $col2]] \
	    [list $operation $equality $col1 $col2]
	
	# If there are no earlier subgoals, just create and return the equality
	if {$columnsSoFar eq {}} {
	    return [list $equality [list $col1 $col2]]
	} else {

	    # There are earlier subgoals. Join the equality relation with them.
	    set joined [my gensym #T]
	    lappend columnsSoFar $col1 $col2
	    set columnsSoFar [lsort -dictionary -unique $columnsSoFar]
	    lappend intcode \
		[list RELATION $joined $columnsSoFar] \
		[list JOIN $joined $dataSoFar $equality]
	    return [list $joined $columnsSoFar]
	}
    }

    # Method: translateLiteral
    #
    #	Generates three-address code to evaluate a literal subgoal of a
    #   Datalog rule
    #
    # Parameters:
    #   literal - Parse tree of the literal
    #	dataSoFar - Name of a relation that holds the result of evaluating
    #               the subgoals to the left of this subgoal
    #   columnsSoFar - List of column names present in 'dataSoFar'
    #
    # Results:
    #   Returns a two element list consisting of the name of the relation
    #   representing the partly-translated rule, and the names of the
    #   columns in that relation
    #
    # Side effects:
    #	Appends three-address instructions to 'intcode'

    method translateLiteral {literal dataSoFar columnsSoFar} {

	# What relation/predicate does the literal refer to?
	set predicate [lindex $literal 1]
	$db relationMustExist $predicate
	set cols [$db columns $predicate]
	if {[llength $cols] != [llength $literal]-2} {
	    set pplit [bdd::datalog::prettyprint-literal $literal]
	    return -code error \
		-errorCode [list DATALOG wrongColumns $predicate $pplit] \
		"$predicate has a different number of columns from $pplit"
	}

	# Make a relation to hold the result of selecting for the tuples
	# that match the literal. The result of the selection may need
	# projection (to eliminate 'don't-care' columns) or renaming
	# (if the domains in the literal don't match the columns in the 
	# relation).
	set selector [my gensym #T]
	set selectLiteral [list LITERAL $selector]
	set needSelect 0
	set needProject 0
	set projector [my gensym #T]
	set projectColumns {}
	set renamed [my gensym #T]
	set renamedFrom {}
	set renamedTo {}

	# Process the terms
	foreach term [lrange $literal 2 end] col $cols {
	    switch -exact -- [lindex $term 0] {
		CONSTANT {

		    # Constant term - make it a selection condition.
		    # The result will require at least a SELECT operation
		    # to choose the tuples, and a projection to get rid
		    # of the constant value.
		    lappend selectLiteral $term
		    set needSelect 1
		    set needProject 1
		}
		VARIABLE {

		    # Variable term. If the variable is '_' (don't care),
		    # then it will need to be projected away. If the variable
		    # is other than the domain of the column, it will need
		    # renaming.
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

	# Generate the selection to bring in any required tuples
	if {$needSelect} {
	    lappend intcode [list RELATION $selector $cols]
	    my translateFact $selectLiteral $cols
	    lappend intcode [list JOIN $selector $selector $predicate]
	    set projectSource $selector
	} else {
	    set projectSource $predicate
	}

	# Project away any constants and don't-cares
	if {$needProject} {
	    lappend intcode \
		[list RELATION $projector $projectColumns] \
		[list PROJECT $projector $projectSource]
	    set renameSource $projector
	} else {
	    set renameSource $projectSource
	}

	# Rename any columns that need it.
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

    # Method: translateSubgoalEnd
    #
    #	Generates three-address code to finish the evaluation of a literal
    #   subgoal of a Datalog rule, after code has been generated for all terms.
    #
    # Parameters:
    #   operation - JOIN or ANTIJOIN according to whether the literal is
    #               negated.
    #	dataSoFar - Name of a relation that holds the result of evaluating
    #               the subgoals to the left of this subgoal
    #   columnsSoFar - List of column names present in 'dataSoFar'
    #   dataThisOp - Name of a relation that holds the result of evaluating
    #                the literal
    #   columnsThisOp - Lisst of column names present in 'dataThisOp'
    #
    # Results:
    #   Returns a two element list consisting of the name of the relation
    #   representing the partly-translated rule, and the names of the
    #   columns in that relation
    #
    # Side effects:
    #	Appends three-address instructions to 'intcode'

    method translateSubgoalEnd {operation 
				dataSoFar columnsSoFar
				dataThisOp columnsThisOp} {
	if {$dataSoFar eq {}} {

	    # This is the first literal in the rule. Negate it if necessary,
	    # and let it be the result
	    if {$operation eq {ANTIJOIN}} {
		lappend intcode [list NEGATE $dataThisOp $dataThisOp]
	    }
	    set resultRelation $dataThisOp
	    set resultColumns $columnsThisOp
	} else {

	    # Join or antijoin the result of the literal to the result of
	    # the subgoals to its left
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

    # Method: translateRuleHead
    #
    #	Generates three-address code to finish the evaluation of a rule
    #   in a Datalog program, after code has been generated for its right
    #   hand side..
    #
    # Parameters:
    #   literal - Literal on the left hand side of the rule
    #	sourceRelation - Relation computed by the right-hand side
    #   sourceColumns - List of column names in 'sourceRelation'
    #
    # Results:
    #	None.
    #
    # Side effects:
    #	Appends three-address instructions to 'intcode'

    method translateRuleHead {literal sourceRelation sourceColumns} {
	set predicate [lindex $literal 1]
	$db relationMustExist $predicate
	set cols [$db columns $predicate]
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
	    my translateFact $constantLiteral $constantColumns
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

    method startMeasure {ind bodyVar instrumentLevel} {
	if {$instrumentLevel >= 1} {
	    upvar 1 $bodyVar body
	    append body $ind {set tock [clock microseconds]} \n
	}
    }
    method endMeasure {ind bodyVar instrumentLevel instr} {
	if {$instrumentLevel >= 1} {
	    upvar 1 $bodyVar body
	    append body $ind {set beadCount 0} \n
	    append body $ind {foreach {col bit n} [sys profile } [lindex $instr 1] {] } \{ \n
	    append body $ind {    incr beadCount $n} \n
	    append body $ind \} \n
	    append body $ind \
		[string map [list @instr $instr] \
		     {puts [format "%10.6f: %6i @instr" [expr {1.0e-6*([clock microseconds] - $tock)}] $beadCount]}] \n
	}
    }

    # Method: generateCode
    #
    #	Generates Tcl code for a Datalog program from the intermediate code 
    #	lists

    method generateCode {instrumentLevel args} {

	set loaders {}

	set prologue \n
	set body \n
	set epilogue \n

	set ind0 {    }
	set ind {    }

	set pc 0
	if {$instrumentLevel >= 1} {
	    puts "Relational algebra for Datalog program:"
	    set pc 0
	}
	foreach instr $intcode {
	    if {$instrumentLevel >= 1} {
		puts [format {%6d: %s} $pc $instr]
		incr pc
	    }
	    switch -exact -- [lindex $instr 0] {
		RELATION {
		    $db relation [lindex $instr 1] {*}[lindex $instr 2]
		    append prologue $ind0 [$db set [lindex $instr 1] {}] \n
		    append epilogue $ind0 [$db set [lindex $instr 1] {}] \n
		}
		
		ANTIJOIN {
		    my startMeasure $ind body $instrumentLevel
		    append body $ind \
			[$db antijoin {*}[lrange $instr 1 end]] \n
		    my endMeasure $ind body $instrumentLevel $instr
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
		    my startMeasure $ind body $instrumentLevel
		    append body $ind \
			[$db equate {*}[lrange $instr 1 end]] \n
		    my endMeasure $ind body $instrumentLevel $instr
		}
		INEQUALITY {
		    my startMeasure $ind body $instrumentLevel
		    append body $ind \
			[$db inequality {*}[lrange $instr 1 end]] \n
		    my endMeasure $ind body $instrumentLevel $instr
		}
		JOIN {
		    my startMeasure $ind body $instrumentLevel
		    append body $ind \
			[$db join {*}[lrange $instr 1 end]] \n
		    my endMeasure $ind body $instrumentLevel $instr
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
		    my startMeasure $ind body $instrumentLevel
		    append body $ind \
			[$db negate {*}[lrange $instr 1 end]] \n
		    my endMeasure $ind body $instrumentLevel $instr
		}
		PROJECT {
		    my startMeasure $ind body $instrumentLevel
		    append body $ind \
			[$db project {*}[lrange $instr 1 end]] \n
		    my endMeasure $ind body $instrumentLevel $instr
		}
		RENAME {
		    my startMeasure $ind body $instrumentLevel
		    append body $ind \
			[$db replace {*}[lrange $instr 1 end]] \n
		    my endMeasure $ind body $instrumentLevel $instr
		}
		SET {
		    my startMeasure $ind body $instrumentLevel
		    append body $ind \
			[$db set {*}[lrange $instr 1 end]] \n
		    my endMeasure $ind body $instrumentLevel $instr
		}
		UNION {
		    my startMeasure $ind body $instrumentLevel
		    append body $ind \
			[$db union {*}[lrange $instr 1 end]] \n
		    my endMeasure $ind body $instrumentLevel $instr
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
	if {$instrumentLevel > 0} {
	    append prologue $ind "puts \"\[info level 0\]:\""
	}
	return $prologue$body$epilogue

    }

    # Method: getRule
    #
    #	Looks up a rule
    #
    # Parameters:
    #	ruleNo - Number of the rule in the order of definition
    #
    # Results:
    #	Returns the parse tree of the rule

    method getRule {ruleNo} {
	return [lindex $rules $ruleNo]
    }

    # Method: getRules
    #
    #	Returns a list of all defined rules
    #
    # Results:
    #	Returns a list of parse trees of all the rules, in order of definition

    method getRules {} {
	return $rules
    }

    # Method: getRulesForPredicate
    #
    #	Returns a list of the rules for a given predicate
    #
    # Parameters;
    #	predicate - Predicate (or name of the relation) being sought
    #
    # Results:
    #	Returns a list of rule nhmbers for the rules having the given
    #	predicate on the left hand side.

    method getRulesForPredicate {predicate} {
	if {[dict exists $rulesForPredicate $predicate]} {
	    return [dict get $rulesForPredicate $predicate]
	} else {
	    return {}
	}
    }

    # Method: getFactsForPredicate
    #
    #	Returns a list of the facts for a given predicate
    #
    # Parameters:
    #	predicate - Name of a predicate (relation)
    #
    # Results:
    #	Returns a list of the facts that assert values for the given predicate.
    #	Each fact is expressed as the parse tree of a literal.

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

# bdd::datalog::compileProgram --
#
#	Compiles a Datalog program into Tcl code
#
# Usage:
#	bdd::datalog::compileProgram $db {
#	    prelude
#	} {
#	    programText
#	} {
#	    postlude
#	}
#
#	-or-
#
#	bdd::datalog::compileProgram $db {
#	    prelude
#	} {
#	    programText
#	} dictVar {
#	    actions
#	} {
#	    postlude
#	}
#
# Parameters:
#	db - Name of the BDD database against which the Datalog program
#	     should operate
#	prelude - A block of Tcl code that should be evaluate before execution
#		  of the Datalog program begins.
#	programText - Text of the program to compile. In the first form,
#		      the text should comprise only assertions of facts
#		      and rules. In the second form, the text may contain
#		      assertions of facts and rules, and must end with
#		      a single query.
#	dictVar - The name of a Tcl variable that will receive, for each
#		  query result, a dictionary whose keys are the names of
#		  terms in the query and whose values are the values of
#		  the terms.
#	actions - A block of Tcl code that will be executed for each query
#		  result, after filling in 'dictVar' with the values
#		  produced by the query.
#	postlude - A block of Tcl code that should execute after the
#		   Datalog program, including all actions, ends.
#
# Results:
#	Returns a block of Tcl code that when evaluated, executes the Datalog
#	program.
#
# Ordinarily, this procedure is used with 'proc' or 'method' to define
# a procedure, with a full example looking like the following:
#
# # create the database, defining 8-bit domains a, b and c
# bdd::fddd::database create db \
#     [bdd::fddd::interleave \
#         [bdd::fddd::domain a 8] \
#         [bdd::fddd::domain b 8] \
#         [bdd::fddd::domain c 8]]]
#
# # create the 'parent' relation and load data into it
# db relation parent a b
# db relation grandparent a b
#
# interp alias {} loadParents {} {*}[db loader parent]
# loadParent 1 0
# loadParent 2 0
# loadParent 3 1
# loadParent 4 1
# loadParent 5 2
# loadParent 6 2
#
# # procedure to create the derived relations from 'parent'
# proc listGrandparents {} [bdd::datalog::compileProgram $db {
#     # no initialization needed
# } {
#     grandparent(a,b) :- parent(a,c), parent(c,b).
# } {
#     return
# }]
#
# # Query the database for the grandparents of an item
# proc grandparent {grandchild} [bdd::datalog::compileProgram $db {
#     set grandparents {}
# } {
#     grandparent(a,$grandchild)?
# } d {
#     lappend grandparents [dict get $d a]
# } {
#     return $grandparents
# }
#
# # Populate the 'grandparent' relation
# listGrandparents
# # What items are the grandparents of item 0?
# puts [grandparents 0]; # prints a list containing 3, 4, 5, and 6

proc bdd::datalog::compileProgram {db args} {

    variable parser

    set instrumentLevel 0
    if {[lindex $args 0] eq {-instrument}} {
	set args [lassign $args - instrumentLevel]
    }
    set args [lassign $args prelude programText]

    switch -exact -- [llength $args] {
	1 {
	    lassign $args postlude
	    set dictAndAction {}
	}
	3 {
	    lassign $args dict action postlude
	    set dictAndAction [list $dict $action]
	}
	default {
	    set methd [lindex [info level 0] 0]
	    return -code error -errorcode {TCL WRONGARGS} \
		"wrong # args: should be $methd db prelude programText ?dictVar action? postlude"
	}
    }

    try {

	set program [bdd::datalog::program new $db]

	# Do lexical analysis of the program
	lassign [lex $programText] tokens values
	
	# Parse the program and feed the parse into $program
	$parser parse $tokens $values $program

	# Plan the execution
	set plan [$program planExecution]

	if {$instrumentLevel >= 1} {
	    puts "Execution plan for Datalog program:"
	    set p 0
	    foreach instr $plan {
		puts [format {%6d: %s} $p $instr]
		incr p
	    }
	}

	# Translate the execution plan to relational algebra
	$program translateExecutionPlan $plan

	# Generate code
	append result \
	    $prelude \n \
	    [$program generateCode $instrumentLevel {*}$dictAndAction] \n \
	    $postlude

    } finally {

	$program destroy

    }
    return $result

}

oo::class create bdd::datalog::database {
    superclass ::bdd::fddd::database

    constructor {args} {
	next {*}$args
    }

    method tclMethod {name arglist body} {
	oo::objdefine [self] method $name $arglist $body
    }
    
    method datalogMethod {name arglist args} {
	oo::objdefine [self] method $name $arglist \
	    [bdd::datalog::compileProgram [self] {*}$args]
    }
    
}

package provide tclbdd::datalog 0.1
