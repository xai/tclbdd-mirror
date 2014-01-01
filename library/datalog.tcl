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

source [file dirname [info script]]/coroutine_iterator.tcl; # TEMP
source [file dirname [info script]]/coroutine_corovar.tcl; # TEMP


package require Tcl 8.6
package require coroutine::corovar 1.0
package require coroutine::iterator 1.0
package require grammar::aycock 1.0

namespace import coroutine::corovar::corovar

namespace eval bdd {
    namespace eval datalog {
	namespace export lex parse compile
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

    # The body is a set of comma-separated, possibly negated conditions

    body	::=	condition
    {
	set _
    }
    body	::=	body , condition
    {
	linsert [lindex $_ 0] end [lindex $_ 2]
    }

    # A condition is either a literal, or else an equality constraint

    condition	::=	literal			{}
    condition	::=	equality		{}

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
    foreach condition [lrange $rule 1 end] {
	append s $sep [prettyprint-condition $condition]
	set sep ,
    }
    return $s
}
proc bdd::datalog::prettyprint-condition {condition} {
    switch -exact [lindex $condition 0] {
	EQUALITY {
	    set s [prettyprint-variable [lindex $condition 1]]
	    append s = [prettyprint-variable [lindex $condition 2]]
	}
	NOT {
	    set s !
	    append s [prettyprint-literal [lindex $condition 1]]
	}
	LITERAL {
	    set s [prettyprint-literal $condition]
	}
	default {
	    error "Expected condition and got $condition"
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
    #                 conditions on the right hand side of the dependent rule.
    # 'query' is a literal giving the query at the end of the program
    #         (if any)

    variable \
	rules \
	rulesForPredicate \
	factsForPredicate \
	outEdgesForPredicate \
	query

    # Constructor -
    #
    #	Creates an empty program.

    constructor {} {
	set rules {}
	set rulesForPredicate {}
	set factsForPredicate {}
	set outEdgesForPredicate {}
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

	# Examine the conditions on the right hand side

	set i 0
	foreach condition [lrange $rule 1 end] {
	    incr i
	    switch -exact -- [lindex $condition 0] {
		EQUALITY { 	# does not create a dependency
		    continue
		}
		LITERAL {
		    set dependency [lindex $condition 1]
		    set not 0
		}
		NOT {
		    set dependency [lindex $condition 1 1]
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
    #	TBD
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
    #	TBD
    #
    # Errors:
    #	Throws an error if the program is not stratifiable.
    
    method planExecutionForComponent {component} {

	set loops {}
	foreach predicate $component {
	    foreach fact [my getFactsForPredicate $predicate] {
		# TODO - Implement this
		puts "Compile fact: [::bdd::datalog::prettyprint-literal $fact]"
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
			# TODO - Implement this
			puts "Compile rule:\
                              [::bdd::datalog::prettyprint-rule $rule]"
		    }
		}
	    }
	}
	if {[llength $loops] != 0} {
	    my planIteration $component $loops
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
    #	TBD

    method planIteration {component loops} {
	# As a heuristic, iterate over the predicate whose in-degree
	# most exceeds its out-degree. This is the predicate whose deletion
	# will remove the most edges from the dependency graph

	foreach rule $loops {
	    set lhPredicate [lindex $rule 0 1]
	    foreach condition [lrange $rule 1 end] {
		switch -exact -- [lindex $condition 0] {
		    EQUALITY {	# does not introduce a dependency
			continue
		    }
		    NOT {
			set rhPredicate [lindex $condition 1 1]
		    }
		    LITERAL {
			set rhPredicate [lindex $condition 1]
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
	set maxDelta -Inf
	dict for {pred d} $delta {
	    if {$d > $maxDelta} {
		set maxDelta $d
		set toRemove $pred
	    }
	}
	# TODO - Implement this
	puts "Generate a loop header for testing convergence of $toRemove"
	try {
	    set loopBody [::bdd::datalog::program new]
	    foreach rule $loops {
		if {[lindex $rule 0 1] ne $toRemove} {
		    $loopBody assertRule $rule
		}
	    }
	    $loopBody planExecution
	    foreach rule $loops {
		if {[lindex $rule 0 1] eq $toRemove} {
		    puts "Compile rule:\
                              [::bdd::datalog::prettyprint-rule $rule]"
		}
	    }
	    # TODO - Implement this
	    puts "Close the loop on $toRemove"
		    
	} finally {
	    $loopBody destroy
	}
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
	foreach condition [lrange $rule 1 end] {
	    if {[set r [my conditionDependsOn $condition $predicates]]
		> $result} {
		set result $r
	    }
	}
	return $result
    }

    # Method: conditionDependsOn
    #
    #	Tests if a condition depends on one or more of a set of predicates.
    #
    # Parameters:
    #	rule - Parse tree of the condition
    #	predicates - List of predicate names
    #
    # Results:
    #	Returns 2 if the rule depends on one of the predicates in negated
    #   form, 1, if the rule depends on one of the predicates only in
    #   non-negated form, 0 if the rule has no dependency on the predicates

    method conditionDependsOn {condition predicates} {
	switch -exact -- [lindex $condition 0] {
	    EQUALITY {
		return false
	    }
	    NOT {
		if {[my conditionDependsOn [lindex $condition 1] $predicates]} {
		    return 2
		} else {
		    return 0
		}
	    }
	    LITERAL {
		if {[lsearch -exact $predicates [lindex $condition 1]] >= 0} {
		    return 1
		} else {
		    return 0
		}
	    }
	}
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

proc bdd::datalog::compileProgram {programText} {

    variable parser

    try {

	set program [bdd::datalog::program new]

	# Do lexical analysis of the program
	lassign [lex $programText] tokens values
	
	# Parse the program
	set parseTree [$parser parse $tokens $values $program]
	
	# Extract the facts, rules, and edges joining the rules from the parse
	set facts [$program getFacts]
	set rules [$program getRules]
	set outedges [$program getEdges]
	
	$program planExecution

    } finally {

	$program destroy

    }
    return

}

package provide tclbdd::datalog 0.1

##############################################################################

if {![info exists ::argv0] || [string compare $::argv0 [info script]]} return

# TEMP - lexer stuff - maybe work out better unit tests!
if 0 {
namespace import bdd::datalog::lex

lassign [lex {
    flowspast(V, St, St2) :- seq(St, St2).
    flowspast(V, St, St2) :- flowspast(V, St, St3),
                             !writes(St3, V),
                             flowspast(V, St3, St).
    reaches(V, St, St2) :- writes(St, V), flowspast(V, St, St2), reads(St2, V).
}] types values
foreach t $types v $values {
    puts "$t: $v"
}

lassign [lex {
    reaches(V, $i, St2)?
}] types values
foreach t $types v $values {
    puts "$t: $v"
}
}

if 0 {
# TEMP parser stuff - need to do better unit testing!

set parseTree [$::bdd::datalog::parser parse {*}[bdd::datalog::lex {
 
    % flowspast(v,st,st2) means that control passes from the exit of st
    % to the entry of st2 without altering the value of v

    flowspast(v, st, st2) :- seq(st, st2).
    flowspast(v, st, st2) :- flowspast(v, st, st3),
                             !writes(st3, v),
                             flowspast(v, st3, st).

    % reaches(v,st,st2) means that st assigns a value to v, which
    % reaches st2, which reads the value of v : that is, st is a
    % reaching definition for the use of v at st2.

    reaches(v, st, st2) :- writes(st, v), flowspast(v, st, st2), reads(st2, v).
}]]

puts $parseTree

set parseTree2 [$bdd::datalog::parser parse {*}[bdd::datalog::lex {
    reaches(v, $i, st2)?
}]]

puts $parseTree2


# Parse tree structure
# PROGRAM statements
# statement:
#   ASSERTION clause
#   RETRACTION clause
#   QUERY literal
# clause:
#   FACT literal
#   RULE Name conditions
# condition:
#   literal
#   EQUALITY variable variable
# literal:
#   NOT literal
#   LITERAL Name terms
# term:
#   CONSTANT const
#   variable
# variable:
#   VARIABLE name

}

# Try compiling a program

bdd::datalog::compileProgram {
 
    % A false entry node (node 0) sets every variable and flows
    % to node 1. If any of its variables are reachable, those are
    % variables possibly used uninitialized in the program.

    writes(0, _).
    writes(st,v) :- writes0(st,v).
    seq(0, 1).
    seq(st,st2) :- seq0(st,st2).

    % flowspast(v,st,st2) means that control passes from the exit of st
    % to the entry of st2 without altering the value of v

    flowspast(v, st, st2) :- seq(st, st2).
    flowspast(v, st, st2) :- flowspast(v, st, st3),
                             !writes(st3, v),
                             flowspast(v, st3, st).

    % reaches(v,st,st2) means that st assigns a value to v, which
    % reaches st2, which reads the value of v : that is, st is a
    % reaching definition for the use of v at st2.

    reaches(v, st, st2) :- writes(st, v), flowspast(v, st, st2), reads(st2, v).

    % A variable read that is reachable from the entry is a read of a
    % possibly uninitialized variable

    uninitRead(st, v) :- reaches(v, 0, st).

    % A variable write that reaches nowhere else is dead code

    deadWrite(st, v) :- writes(st, v), !reaches(v, st, _).

    % Also do the bddbddb example. Only 1 stratum, but 2 loops in the larger SCC

    vP(v, h) :- vP0(v,h).
    vP(v1,h) :- assign(v1,v2), vP(v2,h).
    hP(h1,f,h2) :- store(v1,f,v2), vP(v1,h1), vP(v2,h2).
    vP(v2,h2) :- load(v1,f,v2), vP(v1,h1), hP(h1,f,h2).

}
