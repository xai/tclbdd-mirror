/*
 * tclBdd.c --
 *
 *	Binary decision diagram package for Tcl
 *
 * Copyright (c) 2013 by Kevin B. Kenny.
 *
 * Please refer to the file, 'license.terms' for the conditions on
 * redistribution of this file and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *-----------------------------------------------------------------------------
 */

#include <tcl.h>
#include <tclOO.h>

#include "tclBddInt.h"

/*
 * Objects to create within the literal pool
 */

const char* LiteralValues[] = {
    "0",
    "1",
    "::bdd::system",
    NULL
};
enum LiteralIndex {
    LIT_0,
    LIT_1,
    LIT_BDD_SYSTEM,
    LIT__END
};

/*
 * Structure that holds per-interpreter data for the ODBC package.
 */

typedef struct PerInterpData {
    int refCount;		 /* Reference count */
    Tcl_Obj* literals[LIT__END]; /* Literal pool */
} PerInterpData;
#define IncrPerInterpRefCount(x)  \
    do {			  \
	++((x)->refCount);	  \
    } while(0)
#define DecrPerInterpRefCount(x)		\
    do {					\
	PerInterpData* _pidata = x;		\
	if ((--(_pidata->refCount)) <= 0) {	\
	    DeletePerInterpData(_pidata);	\
	}					\
    } while(0)

/*
 * Structure that represents a BDD_System to TclOO
 */

typedef struct BddSystemData {
    unsigned int refCount;	/* Reference count */
    PerInterpData* pidata;	/* Per-interpreter data */
    BDD_System* system;		/* Pointer to the BDD system */
} BddSystemData;
#define IncrBddSystemDataRefCount(x) \
    do {			  \
	++((x)->refCount);	  \
    } while(0)
#define DecrBddSystemDataRefCount(x)		\
    do {					\
	BddSystemData* sys = x;		\
	if ((--(sys->refCount)) <= 0) {	\
	    DeleteBddSystemData(sys);		\
	}					\
    } while(0)

/*
 * Static functions defined within this file
 */

static void DeletePerInterpData(PerInterpData*);

static int BddSystemConstructor(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				int, Tcl_Obj* const[]);
static int CloneBddSystemObject(Tcl_Interp*, ClientData, ClientData*);
static int CloneMethod(Tcl_Interp*, ClientData, ClientData*);
static void DeleteBddSystemData(BddSystemData*);
static void DeleteBddSystemObject(ClientData);
static void DeleteMethod(ClientData);

/*
 * TclOO data types defined within this file
 */

const static Tcl_ObjectMetadataType BddSystemDataType = {
    TCL_OO_METADATA_VERSION_CURRENT, /* version */
    "BddSystemData",		     /* name */
    DeleteBddSystemObject,	     /* deleteProc */
    CloneBddSystemObject	     /* cloneProc */
};

/*
 * TclOO methods defined within this file
 */

const static Tcl_MethodType BddSystemConstructorType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "CONSTRUCTOR",		   /* name */
    BddSystemConstructor,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};

/*
 *-----------------------------------------------------------------------------
 *
 * Bdd_Init --
 *
 *	Initialize the 'bdd' package
 *
 * Results:
 *	Returns a standard Tcl result
 *
 * Side effects:
 *	Commands are created to manage BDD's.
 *
 *-----------------------------------------------------------------------------
 */

int
Bdd_Init(Tcl_Interp* interp)
{

    PerInterpData* pidata;	/* Per-interpreter data for the package */
    Tcl_Obj** literals;		/* Literal pool */
    Tcl_Object curClassObject;	/* Tcl_Object representing a class being
				   initialized */
    Tcl_Class curClass;		/* Tcl_Class representing a class being
				   initialized */
    int i;

    if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {
	return TCL_ERROR;
    }
    if (Tcl_OOInitStubs(interp) == NULL) {
	return TCL_ERROR;
    }

    /* Install per-interpreter data */

    pidata = (PerInterpData*) ckalloc(sizeof(PerInterpData));
    pidata->refCount = 0;
    literals = pidata->literals;
    for (i = 0; i < LIT__END; ++i) {
	literals[i] = Tcl_NewStringObj(LiteralValues[i], -1);
	Tcl_IncrRefCount(literals[i]);
    }
    IncrPerInterpRefCount(pidata);

    /* 
     * Attach constructor, destructor and methods to the 'bdd::system'
     * class.
     */

    curClassObject = Tcl_GetObjectFromObj(interp, literals[LIT_BDD_SYSTEM]);
    if (curClassObject == NULL) {
	Tcl_SetObjResult(interp,
			 Tcl_NewStringObj("bdd::system is not an object", -1));
	DecrPerInterpRefCount(pidata);
	return TCL_ERROR;
    }
    curClass = Tcl_GetObjectAsClass(curClassObject);
    if (curClass == NULL) {
	Tcl_SetObjResult(interp,
			 Tcl_NewStringObj("bdd::system is not a class", -1));
	DecrPerInterpRefCount(pidata);
	return TCL_ERROR;
    }
    Tcl_ClassSetConstructor(interp, curClass, 
			    Tcl_NewMethod(interp, curClass, NULL, 0,
					  &BddSystemConstructorType,
					  (ClientData) pidata));
    
    /* Provide the package */

    if (Tcl_PkgProvideEx(interp, PACKAGE_NAME, PACKAGE_VERSION,
			 (ClientData) NULL) == TCL_ERROR) {
	DecrPerInterpRefCount(pidata);
	return TCL_ERROR;
    }

    DecrPerInterpRefCount(pidata);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemConstructor --
 *
 *	Constructs a system of Binary Decision Diagrams (BDD's)
 *
 * Parameters:
 *	Accepts a single optional parameter, which is the initial size 
 *	of the system in beads (default is 1024).
 *
 * Results:
 *	Returns a standard Tcl result.
 *
 *-----------------------------------------------------------------------------
 */

static int
BddSystemConstructor(
    ClientData clientData,	     /* Pointer to the per-interp data */
    Tcl_Interp* interp,		     /* Tcl interpreter */
    Tcl_ObjectContext objectContext, /* Object context */
    int objc,			     /* Parameter count */
    Tcl_Obj *const objv[]	     /* Parameter vector */
) {
    PerInterpData* pidata = (PerInterpData*) clientData;
				/* Per-interp data for the BDD package */
    Tcl_Object thisObject = Tcl_ObjectContextObject(objectContext);
				/* Current object */
    int skip = Tcl_ObjectContextSkippedArgs(objectContext);
				/* Number of leading args to skip */
    int size = 1024;		/* Size of the BDD system to create */
    BddSystemData* sdata;
    
    /* Check arguments */

    if (objc != skip && objc != skip+1) {
	Tcl_WrongNumArgs(interp, skip, objv, "?size?");
	return TCL_ERROR;
    }
    if (objc > skip && Tcl_GetIntFromObj(interp, objv[skip], &size) != TCL_OK) {
	return TCL_ERROR;
    }
    if (size <= 4) {
	Tcl_SetObjResult(interp,
			 Tcl_NewStringObj("BDD initial size must be at least 4",
					  -1));
	Tcl_SetErrorCode(interp, "BDD", "InitialSize<4",
			 Tcl_GetString(objv[skip]), NULL);
	return TCL_ERROR;
    }

    /* Create the BDD system */

    sdata = (BddSystemData*) ckalloc(sizeof(BddSystemData));
    sdata->refCount = 1;
    sdata->pidata = pidata;
    IncrPerInterpRefCount(pidata);
    sdata->system = BDD_NewSystem(size);
    
    Tcl_ObjectSetMetadata(thisObject, &BddSystemDataType, (ClientData) sdata);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * CloneBddSystemObject --
 *
 *	Tries to clone a system of BDD's.
 *
 * Results:
 *	Returns TCL_OK to permit the cloning, or TCL_ERROR if the system
 *	cannot be cloned.
 *
 * Notes:
 *	It would be possible to clone all the beads, but reference counts
 *	would be all wrong unless there's some way to clone the BDD objects
 *	within the system. Until and unless this is figured out, just disable
 *	cloning.	
 *
 *-----------------------------------------------------------------------------
 */

static int
CloneBddSystemObject(Tcl_Interp* interp,
		     ClientData oldClientData,
		     ClientData* newClientData)
{
    Tcl_SetObjResult(interp,
		     Tcl_NewStringObj("bdd::system objects may not be cloned",
				      -1));
    Tcl_SetErrorCode(interp, "BDD", "noclone");
    return TCL_ERROR;
}

/*
 *-----------------------------------------------------------------------------
 *
 * CloneMethod --
 *
 *	Callback executed when a BDD system method is cloned.
 *
 * Results:
 *	Returns TCL_OK to permit the cloning.
 *
 * Side effects:
 *	Since nothing is in the client data for methods, nothing need be done.
 *
 *-----------------------------------------------------------------------------
 */

static int
CloneMethod(Tcl_Interp* interp,	/* Tcl interpreter */
	    ClientData oldClientData,
	    ClientData* newClientDataPtr)
{
    /*
     * Copy the client data pointer, just to silence any complaints
     * about unuinitialized memory.
     */
    *newClientDataPtr = oldClientData;
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * DeleteBddSystemData --
 *
 *	Final cleanup when a system of BDD's is deleted
 *
 *-----------------------------------------------------------------------------
 */

static void
DeleteBddSystemData(BddSystemData* sdata)
{
    BDD_DeleteSystem(sdata->system);
    sdata->system = NULL;
    DecrPerInterpRefCount(sdata->pidata);
    ckfree(sdata);
}

/*
 *-----------------------------------------------------------------------------
 *
 * DeleteBddSystemObject --
 *
 *	Cleans up when a system of BDD's is deleted.
 *
 *-----------------------------------------------------------------------------
 */

void
DeleteBddSystemObject(
    ClientData clientData
) {
    DecrBddSystemDataRefCount((BddSystemData*) clientData);
}

/*
 *-----------------------------------------------------------------------------
 *
 * DeleteMethod --
 *
 *	Callback executed when a BDD system method is deleted.
 *
 * Since the client data is unused, there's nothing to be done.
 *
 *-----------------------------------------------------------------------------
 */

static void
DeleteMethod(ClientData clientData)
{
    /* do nothing */
}

/*
 *-----------------------------------------------------------------------------
 *
 * DeletePerInterpData --
 *
 *	Delete per-interpreter data when the ODBC package is finalized
 *
 * Side effects:
 *	Releases the (presumably last) reference on the environment handle,
 *	cleans up the literal pool, and deletes the per-interp data structure.
 *
 *-----------------------------------------------------------------------------
 */

static void
DeletePerInterpData(
    PerInterpData* pidata	/* Data structure to clean up */
) {
    int i;
    for (i = 0; i < LIT__END; ++i) {
	Tcl_DecrRefCount(pidata->literals[i]);
    }
    ckfree((char *) pidata);
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * End:
 */
