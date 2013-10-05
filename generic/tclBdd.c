
#include <tcl.h>

int
Bdd_Init(Tcl_Interp* interp)
{
    if (Tcl_InitStubs(interp, TCL_VERSION, 0) == TCL_ERROR) {
	return TCL_ERROR;
    }

    /* Create provided commands */

    /* Provide the package */

    if (Tcl_PkgProvideEx(interp, PACKAGE_NAME, PACKAGE_VERSION,
			 (ClientData) NULL) != TCL_ERROR) {
	return TCL_ERROR;
    }

    return TCL_OK;
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * End:
 */
