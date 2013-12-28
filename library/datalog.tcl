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

package require Tcl 8.6
package require coroutine::iterator 1.0
package require grammar::aycock 1.0

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

    program	::=	statements 
    {
	linsert [lindex $_ 0] 0 PROGRAM
    }
    program	::=	statements query
    {
	linsert [linsert [lindex $_ 0] end [lindex $_ 1]] 0 PROGRAM
    }
    statements	::=	statements statement
    {
	linsert [lindex $_ 0] end [lindex $_ 1]
    }
    statements	::=
    {
	concat
    }

    # A statement is either an assertion or retraction of a clause

    statement	::=	assertion		{}
    statement	::=	retraction		{}
    assertion	::=	clause .
    {
	list ASSERTION [lindex $_ 0]
    }
    retraction	::=	clause ~
    {
	list RETRACTION [lindex $_ 0]
    }

    # A query gives a literal to match

    query	::=	pliteral ?
    {
	list QUERY [lindex $_ 0]
    }

    # A clause asserts a fact or gives a rule for deducing a fact

    clause	::=	equivalence		{}
    clause	::=	fact			{}
    clause	::=	rule			{}

    # A fact is just a non-negated literal

    fact	::=	pliteral
    {
	list FACT [lindex $_ 0]
    }

    # A rule comprises a head (a fact to be deduced) and a body
    # (the facts to deduce it from)

    rule	::=	head :- body
    {
	linsert [lindex $_ 2] 0 RULE [lindex $_ 0]
    }
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

# bdd::datalog::getrules --
#
#	Walk the parse tree of a Datalog program and extract only the rules
#
# Usage:
#	bdd::datalog::getrules $parseTree
#
# Parameters:
#	parseTree - Parse tree resulting from running the Datalog parser.
#
# Results:
#	Returns a list of the rules, with the RULE flag stripped.
#	Each rule is therefore a list of literals and equalities, with
#	the first element representing the head and the rest representing
#	the body.

proc bdd::datalog::getrules {parseTree} {
    switch -exact -- [lindex $parseTree 0] {
	PROGRAM -
	ASSERTION {
	    set results {}
	    foreach part [lrange $parseTree 1 end] {
		set sub [getrules $part]
		lappend results {*}$sub
	    }
	    return $results
	}
	RULE {
	    return [list [lrange $parseTree 1 end]]
	}
	RETRACTION {
	    error "Retractions are not currently supported."
	}
	default {		# Nothing else may contain a rule
	    return {}
	}
    }
}

# bdd::datalog::pdg --
#
#	Forms the predicate dependency graph of a Datalog program,
#	expressed as a list of edges whose head and tail labels are
#	predicate names.
#
# Parameters:
#	rules - Rules contained in the program
#
# Results:
#	Returns a list of tuples: each tuple consists of
#	  {dependent dependency negated rule} 
#	where
#	  * dependent is the name of the dependent predicate
#	  * dependency is the name of the dependency
#	  * negated is 1 if the dependency appears in the rule in negated form
#	  * rule is a copy of the input rule

proc bdd::datalog::pdg {rules} {
    # extract the predicate dependency graph as an edge list from
    # the rules of a Datalog program
    set results {}
    foreach rule $rules {
	set body [lassign $rule head]
	set dependent [lindex $head 1]; # LITERAL name term term ...
	foreach condition $body {
	    switch -exact -- [lindex $condition 0] {
		EQUALITY {		# does not create a dependency
		}
		LITERAL {
		    lappend result \
			[list $dependent [lindex $condition 1] 0 $rule]
		}		
		NOT {		# {NOT {LITERAL foo ...}}
		    lappend result \
			[list $dependent [lindex $condition 1 1] 1 $rule]
		}
	    }
	}
    }
    return $result
}

# bdd::datalog::pdg-dicts
#
#	Inverts the edge set of the predicate dependency graph of a
#	Datalog program into individual adjacency lists
#
# Parameters:
#	pdg - Edge list of the predicate dependency graph
#
# Results:
#
#	Returns a list two dictionaries. The first is keyed by the
#	dependent, and its values are lists of the edges that join it
#	to its dependencies. The second is keyed by dependency, and
#	its values are lists of edges joining the dependnts to it.

proc bdd::datalog::pdg-dicts {pdg} {
    set outedges {}
    set inedges {}
    foreach edge $pdg {
	lassign $edge from to not
	if {![dict exists $inedges $from]} {
	    dict set inedges $from {}
	}
	dict lappend outedges $from $edge
	if {![dict exists $outedges $to]} {
	    dict set outedges $to {}
	}
	dict lappend inedges $to $edge
    }
    return [list $outedges $inedges]
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
    upvar #1 outedges outedges;	# Coroutine global: adjacency lists
    upvar #1 index index;	# Coroutine global: index for the next
    				# unexamined node
    upvar #1 S S;		# Coroutine global: stack of nodes traversed
    				# from a root
    upvar #1 vindex vindex;	# Coroutine global: dictionary whose keys are
				# names of nodes and whose values are node
    				# indices
    upvar #1 lowlink lowlink;	# Coroutine global: dictionary whose keys are
				# names of nodes and whose values are the
    				# indices of the other end of back edges

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

# bdd::datalog::indexSCCs --
#
#	Partitions the dependency graph of a Datalog program into strongly
#	connected components, and partitions the adjacency lists by
#	dependent component.
#
# Parameters:
#	outedges - Dictionary containing the adjacency lists. The keys
#		   of the dictionary are the names of predicates. The
#		   values are lists of edges. Each edge is a tuple.
#		   The first two elements of the tuple are the dependent
#		   predicate and the dependency predicate. The remaining 
#		   elements are not used in this procedure.
#
# Results:
#	Returns two lists. The first is a list of the strongly connected
#	components, each of which is represented as a sublist containing the
#	names of predicates in the component. The second is a dictionary
#	which for each predicate gives the predicate's position in the list
#	of components.

proc bdd::datalog::indexSCCs {outedges} {
    # make the index of predicate->component index: output is componentList and
    # componentIndex.
    
    set i 0
    set componentIndex {}
    set componentList {}
    bdd::datalog::scc c $outedges {
	foreach predicate $c {
	    dict set componentIndex $predicate $i
	}
	puts "component $i: $c"
	lappend componentList $c
	incr i
    }
    return [list $componentList $componentIndex]
}

# bdd::datalog::makeComponentEdges --
#
#	Given the partition of the predicate dependency graph into strongly
#	connected components, makes adjacency lists between the components,
#	each of which gives the predicate dependencies that relate
#	the component pair.
#
# Parameters:
#	componentList - List of the components, each of which is a list of
#		        predicate names.
#	componentIndex - Dictionary whose keys are predicate names and whose
#		         values are the positions of the predicates' components
#			 in componentList
#	outEdges - Edge list for the predicate dependency graph.
#
# Results:
#	Returns a list of two items, componentEdges and componentEdgeNegated.
#	componentEdges is a list whose positions are component numbers,
#	and whose values are dictionaries whose keys dependent component
#	numbers and whose values are lists of the edges that introduce
#	the dependency. componentEdgeNegated is a two-level dictionary
#	whose keys are the component numbers of dependent and dependency,
#	and whose values exist if at least one dependency predicate
#	appears in a dependent rule in negated form.
#
# Side effects:
#	Throws an error if the Datalog program is not stratifiable.

proc bdd::datalog::makeComponentEdges {componentList componentIndex outedges} {

    set componentEdges {}
    set componentEdgeNegated {}
    set i 0
    foreach component $componentList {
	set curComponentEdges {}
	foreach predicate $component {
	    foreach edge [dict get $outedges $predicate] {
		lassign $edge from to not rule
		set destComponent [dict get $componentIndex $to]
		if {$not && ($from == $to)} {
		    # TODO - better error reporting
		    error "The program is not stratifiable\
                           because of rule $rule"
		}
		dict lappend curComponentEdges $destComponent $edge
		if {$not} {
		    dict set componentEdgeNegated $i $destComponent 1
		}
	    }
	}
	lappend componentEdges $curComponentEdges
	incr i
    }
    return [list $componentEdges $componentEdgeNegated]
}

# bdd::datalog::stratify --
#
#	Given the strongly connected components of a Datalog program
#	and the dependency relations between components, stratifies the
#	program.
#
# Parameters:
#	componentEdges - List whose positions are component numbers,
#			 and whose values are dictionaries whose keys are the
#                        component numbers of dependencies and whose
#			 values are lists of edges that introduce the
#			 dependency
#	componentEdgeNegated - Two level dictionary whose values are dependent
#			       component index and dependency component index,
#			       whose values exist if at least one predicate
#			       of the dependency appears in the dependent in
#			       negated form.
#
# Results:
#	Returns a dictionary whose keys are component indices and
#	whose values are the stratum numbers to which they belong.

proc bdd::datalog::stratify {componentEdges componentEdgeNegated} {
    set stratum {}
    set c 0
    foreach e $componentEdges {
	stratify1 stratum $componentEdges $componentEdgeNegated $c
	incr c
    }
    return $stratum
}

# bdd::datalog::stratify1 --
#
#	Service procedure for the stratification pass
#
# Parameters:
#	stratumVar - Name of the dictionary in caller's scope where the
#		     stratum information is being accumulated. Keys
#		     are component indices; values are strata.
#	componentEdges - List whose positions are component numbers,
#			 and whose values are dictionaries whose keys are the
#                        component numbers of dependencies and whose
#			 values are lists of edges that introduce the
#			 dependency
#	componentEdgeNegated - Two level dictionary whose values are dependent
#			       component index and dependency component index,
#			       whose values exist if at least one predicate
#			       of the dependency appears in the dependent in
#			       negated form.
#	c - Component index being examined
#
# Results:
#	Returns the stratum of component 'c'
#
# Side effects:
#	Computes strata for the component and its descendants. Utilizes
#	'stratumVar' as a cache so that each component is visited only
#	once.
#
# If component A depends on component B, then stratum[A] >= stratum[B].
# If at least one rule introducing the dependency has the dependent
# predicate appearing in negated form, then stratum[A] > stratum[B].
# The resulting strata are the ones that give stratum[X]>=0 for all X,
# for which the values of stratum[X] are minimized for all X subject to
# the constraints above.

proc bdd::datalog::stratify1 {stratumVar 
			      componentEdges
			      componentEdgeNegated
			      c} {
    upvar 1 $stratumVar stratum
    if {[dict exists $stratum $c]} {
	return [dict get $stratum $c]
    } else {
	set edgeSet [lindex $componentEdges $c]
	if {[dict size $edgeSet] == 0} {
	    set s 0
	} else {
	    set s 1
	    dict set stratum $c 1
	    dict for {next edges} $edgeSet {
		if {$next == $c} continue
		set t [stratify1 stratum \
			   $componentEdges $componentEdgeNegated $next]
		incr t [dict exists $componentEdgeNegated $c $next]
		if {$t > $s} {
		    set s $t
		}
	    }
	}
	dict set stratum $c $s
	return $s
    }
}

# bdd::datalog::compsByStratum --
#
#	Distributes the strongly connected components of the predicate
#	dependency graph by stratum after the program is stratified.
#
# Parameters:
#	stratum - Dictionary whose keys are component numbers and whose
#	          values are stratum numbers for the components
#
# Results:
#	Returns a list with one element per stratum, in order by stratum
#	number. The elements are the lists of components at each of the
#	strata.

proc bdd::datalog::compsByStratum {stratum} {
    set bystratum {}
    dict for {c s} $stratum {
	while {$s >= [llength $bystratum]} {
	    lappend bystratum {}
	}
	set comps [lindex $bystratum $s]; lset bystratum $s {}
	lappend comps $c; lset bystratum $s $comps
    }
    return $bystratum
}

# bdd::datalog::sortComponents --
#
#	Topologically sorts the components at each stratum of the predicate 
#	dependency graph to give the components' order of evaluation in
#	the final generated code.
#
# Parameters:
#	stratum - Dictionary whose keys are component numbers and whose
#	          values are the stratum numbers for the components
#	bystratum - List of lists. The sublists are the component indices
#	            at each stratum.
#	componentEdges - List of dictionaries. The positions in the
#		         list are component indices. The keys of the
#			 dictionaries are dependency component indices,
#			 and the values are lists of edges inducing the
#			 dependencies.
#
# Results:
#	Returns 'bystratum' with the component numbers reordered into
#	topologic numbering.

proc bdd::datalog::sortComponents {stratum bystratum componentEdges} {
    set s 0
    foreach clist $bystratum {
	set e {}
	foreach comp $clist {
	    dict for {comp2 edges} [lindex $componentEdges $comp] {
		if {($comp2 != $comp) && [dict get $stratum $comp2] == $s} {
		    puts "same-stratum edge $comp->$comp2"
		    dict lappend e $comp2 [list $comp2 $comp]
		}
	    }
	}
	lset bystratum $s [bdd::datalog::topsort $clist $e]
	incr s
    }
    return $bystratum
}

# bdd::datalog::topsort --
#
#	Topologic sort.
#
# Parameters:
#	v - List of vertices in a DAG.
#	e - Dictionary of edges in the graph. The keys are origin nodes.
#	    The values are lists of edges. The edges are tuples, the first
#	    two elements of which are the origin and destination vertices.
#
# Results:
#	Returns a topologic ordering of v. If (v,w) is in E, then v
#	precedes w in the ordering.
#
# This procedure separates the DAG into connected commponents, and
# for each component, adds the vertices of the component to the
# output in reverse postorder of the component's minimum spanning tree.

proc bdd::datalog::topsort {v e} {
    set result {}
    set visited {}
    foreach vertex $v {
	topsort1 result visited $e $vertex
    }
    return [lreverse $result]
}

# bdd::datalog::topsort1 --
#
#	Service procedure for topologic sort.
#
# Parameters:
#	resultVar - Name of a variable in the caller's scope where
#		    the topologic order is being accumulated.
#	visitedVar - Name of a dictionary in the caller's scope whose
#		     keys are the names of vertices already visited.
#	e - Dictionary of edges in the graph. The keys are origin nodes.
#	    The values are lists of edges. The edges are tuples, the first
#	    two elements of which are the origin and destination vertices.
#	v - Name of the current vertex being visited.
#
# Results:
#	None
#
# This procedure is called at least once for every vertex. It walks
# the vertex's outbound edges recursively until it comes to either an
# vertex already visited or to a sink. The vertices that it visits are
# accumulated in postorder. (The calling procedure will reverse them).

proc bdd::datalog::topsort1 {resultVar visitedVar e v} {
    upvar 1 $resultVar result
    upvar 1 $visitedVar visited
    if {[dict exists $visited $v]} {
	return
    }
    dict set visited $v {}
    if {[dict exists $e $v]} {
	foreach edge [dict get $e $v] {
	    topsort1 result visited $e [lindex $edge 1]
	}
    }
    lappend result $v
    return
}

proc bdd::datalog::compile {program} {
    # Do lexical analysis of the program
    lassign [::bdd::datalog::lex $program] tokens values
    
    # Parse the program
    set parseTree [$::bdd::datalog::parser parse $tokens $values]

    # Extract the rules from the parse tree
    set rules [bdd::datalog::getrules $parseTree]
    
    # Form the predicate dependency graph of the rules
    set pdg [bdd::datalog::pdg $rules]
    
    # Distribute the edges of the graph into separate adjacency lists
    lassign [bdd::datalog::pdg-dicts $pdg] outedges inedges
    
    # Find the stongly connected components of the predicate dependency graph
    lassign [bdd::datalog::indexSCCs $outedges] componentList componentIndex
    
    # Determine whether the predicate dependency graph is stratifiable, and 
    # distribute the edges of the component graph into separate adjacency lists.
    lassign [bdd::datalog::makeComponentEdges \
		 $componentList $componentIndex $outedges] \
	componentEdges componentEdgeNegated
    
    # TEMP - report component-component edges
    set i 0
    foreach edgeSet $componentEdges {
	dict for {j edges} $edgeSet {
	    puts "$i -> $j ([dict exists $componentEdgeNegated $i $j])"
	}
	incr i
    }

    # Stratify the predicate dependency graph
    set stratum [bdd::datalog::stratify $componentEdges $componentEdgeNegated]
    
    # Distribute the components by stratum
    set bystratum [bdd::datalog::compsByStratum $stratum]
    
    # Within each stratum, topologically sort the components so that
    # dependencies precede their dependents
    
    set bystratum [bdd::datalog::sortComponents \
		       $stratum $bystratum $componentEdges]

    # TEMP - Report the components in final order of processing
    set s 0
    foreach clist $bystratum {
	puts "stratum $s"
	foreach c $clist {
	    puts "component $c: [lindex $componentList $c]"
	}
	incr s
    }
    
    # TEMP - Report the intra-component dependencies for the components 
    if 0 {
	set s 0
	foreach clist $bystratum {
	    foreach c $clist {
		puts "component $c at stratum $s"
		set edges [lindex $componentEdges $c]
		puts "edges to [dict keys $edges]"
		if {[dict exists $edges $c]} {
		    foreach edge [dict get $edges $c] {
			puts $edge
		    }
		}
	    }
	    incr s
	}
    }
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

bdd::datalog::compile {
 
    % A false entry node (node 0) sets every variable and flows
    % to node 1. If any of its variables are reachable, those are
    % variables possibly used uninitialized in the program.

    writes(0, _).
    seq(0, 1).

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
    vP(v1,h) :- assign(v1,v2).
    hP(h1,f,h2) :- store(v1,f,v2), vP(v1,h1), vP(v2,h2).
    vP(v2,h2) :- load(v1,f,v2), vP(v1,h1), hP(h1,f,h2).

}

