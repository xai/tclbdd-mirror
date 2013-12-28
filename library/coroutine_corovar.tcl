# coroutine_corovar.tcl --
#
#	Implements a 'corovar' command that declares a set of variables
#	to be coroutine-global
#
# Copyright (c) 2013 by Kevin B. Kenny
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#------------------------------------------------------------------------------

package require Tcl 8.6

namespace eval coroutine {
    namespace eval corovar {
	namespace export corovar
    }
}

# coroutine::corovar::corovar --
#
#	Declares a set of variables to be coroutine-global
#
# Usage:
#	coroutine::corovar::corovar ?var?...
#
# Parameters:
#	var - Name of a variable to make coroutine-global
#
# Results:
#	None
#
# Side effects:
#	Imports the given coroutine-global vars into the caller's scope.

proc coroutine::corovar::corovar {args} {
    foreach var $args {
	uplevel 1 [list upvar #1 $var $var]
    }
    return
}

package provide coroutine::corovar 1.0