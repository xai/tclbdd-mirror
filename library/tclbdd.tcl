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

namespace eval bdd {
    namespace export system
}

oo::class create bdd::system {
    method === {exprName1 exprName2} {
	expr {[my beadindex $exprName1] == [my beadindex exprName2]}
    }
    method satisfiable {exprName} {
	expr {[my beadindex $exprName] != 0}
    }
    method tautology {exprName} {
	expr {[my beadindex $exprname] == 1}
    }
}
