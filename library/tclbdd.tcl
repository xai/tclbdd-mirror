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

oo::class create bdd::SystemInt {
    variable beadseq
    constructor args {
    }
}
oo::class create bdd::system {
    superclass ::bdd::SystemInt
}
