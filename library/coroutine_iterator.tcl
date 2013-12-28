# coroutine_iterator.tcl --
#
#	Implements a 'foreach' loop that uses a coroutine to manage the
#	iteration, and cleans up properly on unusual terminations.
#
# Copyright (c) 2013 by Kevin B. Kenny
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#------------------------------------------------------------------------------

package require Tcl 8.6

namespace eval coroutine {
    namespace eval iterator {
	variable gensym 0;	# Sequence number for generated symbols
	namespace export foreach
    }
}

# coroutine::iterator::foreach --
#
#	Iterate over the results of a coroutine
#
# Usage:
#	coroutine::iterator::foreach var initCommand script
#
# Parameters:
#	var         - Name of the variable in caller's scope that
#		      will hold the values the procedure is iterating over.
#	initCommand - Command and arguments that will be the main
#		      procedure of the coroutine. The procedure is
#		      expected to yield each of the iteration results
#		      in turn, and then return to indicate the end of
#		      the loop.
#	script      - Script to execute for each [yield]ed value, with
#		      the [yield]ed value in $var.
#
# Results:
#	None.
#
# Side effects:
#	Launches a coroutine with the given 'initCommand' and runs
#	it to completion, executing the given script one on each
#	[yield]ed result.

proc coroutine::iterator::foreach {var initCommand script} {
    variable gensym
    set coro [namespace current]::coro[incr gensym]
    upvar 1 $var value
    try {
	for {set value [coroutine $coro {*}$initCommand]} \
	    {[namespace which $coro] ne {}} \
	    {set value [$coro]} {
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
		}
	    }
    } finally {
	catch {rename $coro {}}
    }
}

package provide coroutine::iterator 1.0

if {![info exists ::argv0] || $::argv0 ne [info script]} {
    return
}

# Example:

proc doit {n {i 0}} {
    if {$n == 0} {
	return
    }
    yield $i
    incr n -1
    doit $n [expr {2*$i + 1}]
    doit $n [expr {2*$i + 2}]
    return
}

coroutine::iterator::foreach x {doit 4} {puts $x}
