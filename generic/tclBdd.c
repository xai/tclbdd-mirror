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
#include <tclTomMath.h>

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
    Tcl_HashTable* expressions;	/* Hash table of named expressions */
} BddSystemData;
#define IncrBddSystemDataRefCount(x) \
    do {			  \
	++((x)->refCount);	  \
    } while(0)
#define DecrBddSystemDataRefCount(x)		\
    do {					\
	BddSystemData* sys = (x);		\
	if ((--(sys->refCount)) <= 0) {		\
	    DeleteBddSystemData(sys);		\
	}					\
    } while(0)

/*
 * Row in a method table
 */
typedef struct MethodTableRow {
    const char* name;		/* Name of the method */
    const Tcl_MethodType* type;	/* Type of the method */
    ClientData clientData;	/* Client data */
} MethodTableRow;

/*
 * Static functions defined within this file
 */

static void SetNamedExpression(BddSystemData*, Tcl_Obj*, BDD_BeadIndex);
static Tcl_HashEntry* FindHashEntryForNamedExpression(Tcl_Interp*,
						      BddSystemData*,
						      Tcl_Obj*);
static int FindNamedExpression(Tcl_Interp*, BddSystemData*, Tcl_Obj*,
			       BDD_BeadIndex*);
static int UnsetNamedExpression(Tcl_Interp*, BddSystemData*, Tcl_Obj*);
static void DeletePerInterpData(PerInterpData*);
static int BddSystemConstructor(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				int, Tcl_Obj* const[]);
static int BddSystemBeadindexMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				    int, Tcl_Obj* const[]);
static int BddSystemBinopMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				int, Tcl_Obj* const[]);
static int BddSystemConstantMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				   int, Tcl_Obj* const[]);
static int BddSystemCopyMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
			       int, Tcl_Obj* const[]);
static int BddSystemDumpMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
			       int, Tcl_Obj* const[]);
/* not yet there
static int BddSystemNegateMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				 int, Tcl_Obj* const[]);
*/
static int BddSystemNotnthvarMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				    int, Tcl_Obj* const[]);
static int BddSystemNthvarMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				 int, Tcl_Obj* const[]);
/* not yet there
static int BddSystemRestrictMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				   int, Tcl_Obj* const[]);
*/
static int BddSystemSatcountMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				   int, Tcl_Obj* const[]);
static int BddSystemUnsetMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
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

const static Tcl_MethodType BddSystemBeadindexMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "beadindex",		   /* name */
    BddSystemBeadindexMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
const static Tcl_MethodType BddSystemBinopMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "binop",			   /* name */
    BddSystemBinopMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};				   /* common to all ten binary operators */
const static Tcl_MethodType BddSystemConstantMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "constant",			   /* name */
    BddSystemConstantMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
const static Tcl_MethodType BddSystemCopyMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "copy",			   /* name */
    BddSystemCopyMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
const static Tcl_MethodType BddSystemDumpMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "copy",			   /* name */
    BddSystemDumpMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
#if 0
/* not yet there */
const static Tcl_MethodType BddSystemNegateMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "negate",			   /* name */
    BddSystemNegateMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
#endif
const static Tcl_MethodType BddSystemNotnthvarMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "notnthvar",		   /* name */
    BddSystemNotnthvarMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
const static Tcl_MethodType BddSystemNthvarMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "nthvar",			   /* name */
    BddSystemNthvarMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
#if 0
/* not yet there */
const static Tcl_MethodType BddSystemRestrictMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "restrict",			   /* name */
    BddSystemRestrictMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
#endif
const static Tcl_MethodType BddSystemSatcountMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "satcount",			   /* name */
    BddSystemSatcountMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
const static Tcl_MethodType BddSystemUnsetMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "unset",			   /* name */
    BddSystemUnsetMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};

/*
 * Method table for the BDD system object
 */

MethodTableRow systemMethodTable[] = {
    { "!=",        &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_NE },
    { "&",         &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_AND },
    { "<=",        &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_LE },
    { "<",         &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_LT },
    { "==",        &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_EQ },
    { ">",         &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_GT },
    { ">=",        &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_GE },
    { "^",         &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_XOR },
    { "beadindex", &BddSystemBeadindexMethodType, NULL },
    { "constant",  &BddSystemConstantMethodType,  NULL },
    { "copy",      &BddSystemCopyMethodType,      NULL },
    { "dump",      &BddSystemDumpMethodType,      NULL },
    { "nand",      &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_NAND },
    { "nor",       &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_NOR },
    { "notnthvar", &BddSystemNotnthvarMethodType, NULL },
    { "nthvar",    &BddSystemNthvarMethodType,    NULL },
    /* not yet there
    { "restrict",  &BddSystemRestrictMethodType,  NULL },
    */
    { "satcount",  &BddSystemSatcountMethodType,  NULL },
    { "unset",     &BddSystemUnsetMethodType,     NULL },
    { "|",         &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_OR },
    /* not yet there
    { "~",         &BddSystemNegateMethodType,    NULL },
    */
    { NULL,	   NULL,                         NULL }
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
    MethodTableRow* methodPtr;	/* Current row in the method table */
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
    IncrPerInterpRefCount(pidata);
    for (methodPtr = systemMethodTable; methodPtr->name != NULL; ++methodPtr) {
	Tcl_NewMethod(interp, curClass, Tcl_NewStringObj(methodPtr->name, -1),
		      1, methodPtr->type, methodPtr->clientData);
    }
    /* Provide the package */

    if (Tcl_PkgProvideEx(interp, PACKAGE_NAME, PACKAGE_VERSION,
			 ( ClientData) NULL) == TCL_ERROR) {
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
    BddSystemData* sdata;	/* Pointer to the data describing the new 
				 * system */
    Tcl_HashEntry* entryPtr;	/* Pointer to a hash entry for a named
				 * expression */
    int newFlag;	        /* Flag for whether a hash entry is new */
    
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
    sdata->expressions = ckalloc(sizeof(Tcl_HashTable));
    Tcl_InitObjHashTable(sdata->expressions);
    entryPtr = Tcl_CreateHashEntry(sdata->expressions, pidata->literals[LIT_0],
				   &newFlag);
    Tcl_SetHashValue(entryPtr, 0);
    entryPtr = Tcl_CreateHashEntry(sdata->expressions, pidata->literals[LIT_1],
				   &newFlag);
    Tcl_SetHashValue(entryPtr, 1);
    
    Tcl_ObjectSetMetadata(thisObject, &BddSystemDataType, (ClientData) sdata);

    Tcl_ObjectContextInvokeNext(interp, objectContext, objc, objv, skip);

    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemBeadindexMethod --
 *
 *	Returns the index of a bead given an expression name
 *
 * Usage:
 *	$system beadindex name
 *
 * Parameters:
 *	name - Name of the expression
 *
 * Results:
 *	Returns the index of the bead; throws an error if no expression
 *	of the given name is defined.
 *
 *-----------------------------------------------------------------------------
 */

static int
BddSystemBeadindexMethod(
    ClientData clientData,	/* unused */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_ObjectContext ctx,	/* Object context */
    int objc,			/* Parameter count */
    Tcl_Obj *const objv[])	/* Parameter vector */
{
    Tcl_Object thisObject = Tcl_ObjectContextObject(ctx);
				/* The current object */
    BddSystemData* sdata = (BddSystemData*)
	Tcl_ObjectGetMetadata(thisObject, &BddSystemDataType);
				/* The current system of expressions */
    int skipped = Tcl_ObjectContextSkippedArgs(ctx);
				/* The number of args used in method dispatch */
    BDD_BeadIndex beadIndex;	/* The bead index */

    /* Check syntax */

    if (objc != skipped+1) {
	Tcl_WrongNumArgs(interp, skipped, objv, "name");
	return TCL_ERROR;
    }
    if (FindNamedExpression(interp, sdata, objv[skipped],
			    &beadIndex) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_SetObjResult(interp, Tcl_NewWideIntObj(beadIndex));
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemBinopMethod --
 *
 *	Computes a binary operation between two expressions in a BDD system
 *	and assigns it a name
 *
 * Usage:
 *	$system OP a b c
 *
 * Parameters:
 *	OP - One of the binary operators nor, <, >, !=, ^, nand, &,
 *           ==, <=, >=, |
 *	a - Name of the result expression
 *	b - Name of the left-hand operand
 *	c - Name of the right-hand operand expression
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Assigns the given name to the result expression
 *
 *-----------------------------------------------------------------------------
 */

static int
BddSystemBinopMethod(
    ClientData clientData,	/* Operation to perform */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_ObjectContext ctx,	/* Object context */
    int objc,			/* Parameter count */
    Tcl_Obj *const objv[])	/* Parameter vector */
{
    Tcl_Object thisObject = Tcl_ObjectContextObject(ctx);
				/* The current object */
    BddSystemData* sdata = (BddSystemData*)
	Tcl_ObjectGetMetadata(thisObject, &BddSystemDataType);
				/* The current system of expressions */
    int skipped = Tcl_ObjectContextSkippedArgs(ctx);
				/* The number of args used in method dispatch */
    BDD_BeadIndex beadIndexOpd1; /* The bead index of the first operand */
    BDD_BeadIndex beadIndexOpd2; /* The bead index of the second operand */
    BDD_BeadIndex beadIndexResult; /* The bead index of the result */

    /* Check syntax */

    if (objc != skipped+3) {
	Tcl_WrongNumArgs(interp, skipped, objv, "result operand1 operand2");
	return TCL_ERROR;
    }
    if (FindNamedExpression(interp, sdata, objv[skipped+1],
			    &beadIndexOpd1) != TCL_OK
	|| FindNamedExpression(interp, sdata, objv[skipped+2],
			       &beadIndexOpd2) != TCL_OK) {
	return TCL_ERROR;
    }
    beadIndexResult = BDD_Apply(sdata->system, (BDD_BinOp) (size_t)clientData,
				beadIndexOpd1, beadIndexOpd2);
    SetNamedExpression(sdata, objv[skipped], beadIndexResult);
    BDD_UnrefBead(sdata->system, beadIndexResult);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemConstantMethod --
 *
 *	Computes a constant expression in a system of BDD's and assigns
 *	it a name
 *
 * Usage:
 *	$system constant name value
 *
 * Parameters:
 *	name - The name to assign
 *	value - The Boolean value to give it
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Assigns the given value to the name
 *
 *-----------------------------------------------------------------------------
 */

static int
BddSystemConstantMethod(
    ClientData clientData,	/* Operation to perform */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_ObjectContext ctx,	/* Object context */
    int objc,			/* Parameter count */
    Tcl_Obj *const objv[])	/* Parameter vector */
{
    Tcl_Object thisObject = Tcl_ObjectContextObject(ctx);
				/* The current object */
    BddSystemData* sdata = (BddSystemData*)
	Tcl_ObjectGetMetadata(thisObject, &BddSystemDataType);
				/* The current system of expressions */
    int skipped = Tcl_ObjectContextSkippedArgs(ctx);
				/* The number of args used in method dispatch */
    int boolval;		/* Boolean value */

    /* Check syntax */

    if (objc != skipped+2) {
	Tcl_WrongNumArgs(interp, skipped, objv, "name value");
	return TCL_ERROR;
    }
    if (Tcl_GetBooleanFromObj(interp, objv[skipped+1], &boolval) != TCL_OK) {
	return TCL_ERROR;
    }
    SetNamedExpression(sdata, objv[skipped], (BDD_BeadIndex) boolval);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemCopyMethod --
 *
 *	Copies a named expression in a system of BDD's and assigns
 *	it a new name
 *
 * Usage:
 *	$system copy new old
 *
 * Parameters:
 *	new - The name to assign
 *	old - The name already assigned to its value
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Copies the given value to the new name
 *
 *-----------------------------------------------------------------------------
 */

static int
BddSystemCopyMethod(
    ClientData clientData,	/* Operation to perform */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_ObjectContext ctx,	/* Object context */
    int objc,			/* Parameter count */
    Tcl_Obj *const objv[])	/* Parameter vector */
{
    Tcl_Object thisObject = Tcl_ObjectContextObject(ctx);
				/* The current object */
    BddSystemData* sdata = (BddSystemData*)
	Tcl_ObjectGetMetadata(thisObject, &BddSystemDataType);
				/* The current system of expressions */
    int skipped = Tcl_ObjectContextSkippedArgs(ctx);
				/* The number of args used in method dispatch */
    BDD_BeadIndex beadIndex;	/* The bead index for the old expression */

    /* Check syntax */

    if (objc != skipped+2) {
	Tcl_WrongNumArgs(interp, skipped, objv, "out in");
	return TCL_ERROR;
    }
    if (FindNamedExpression(interp, sdata, objv[skipped+1],
			    &beadIndex) != TCL_OK) {
	return TCL_ERROR;
    }
    SetNamedExpression(sdata, objv[skipped], beadIndex);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemDumpMethod --
 *
 *	Dumps the internal representation of a named expression
 *
 * Usage:
 *	$system dump name
 *
 * Parameters:
 *	name - Name of the expression
 *
 * Results:
 *	Returns a dictionary whose keys are states and whose values are
 *	triples {variable# nextStateIfFalse nextStateIfTrue}. The triples
 *	are placed in the list in depth-first order.
 *
 *-----------------------------------------------------------------------------
 */

static int
BddSystemDumpMethod(
    ClientData clientData,	/* unused */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_ObjectContext ctx,	/* Object context */
    int objc,			/* Parameter count */
    Tcl_Obj *const objv[])	/* Parameter vector */
{
    Tcl_Object thisObject = Tcl_ObjectContextObject(ctx);
				/* The current object */
    BddSystemData* sdata = (BddSystemData*)
	Tcl_ObjectGetMetadata(thisObject, &BddSystemDataType);
				/* The current system of expressions */
    int skipped = Tcl_ObjectContextSkippedArgs(ctx);
				/* The number of args used in method dispatch */
    BDD_BeadIndex beadIndex;	/* The bead index */
    Tcl_Obj* result;		/* The result dictionary */
    int status;			/* Tcl status return */

    /* Check syntax */

    if (objc != skipped+1) {
	Tcl_WrongNumArgs(interp, skipped, objv, "name");
	return TCL_ERROR;
    }
    if (FindNamedExpression(interp, sdata, objv[skipped],
			    &beadIndex) != TCL_OK) {
	return TCL_ERROR;
    }
    result = Tcl_NewObj();
    status = BDD_Dump(interp, result, sdata->system, beadIndex);
    if (status == TCL_OK) {
	Tcl_SetObjResult(interp, result);
    }
    return status;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemNegateMethod --
 *
 *	Computes the negation of a named expression in a system of
 *	BDD's and assigns it a name
 *
 * Usage:
 *	$system negate result operand
 *
 * Parameters:
 *	result  - The name to assign
 *	operand - The name of the expression to negate
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Assigns the negation of the operand to the result
 *
 *-----------------------------------------------------------------------------
 */

/* not yet there */
#if 0
static int
BddSystemNegateMethod(
    ClientData clientData,	/* Operation to perform */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_ObjectContext ctx,	/* Object context */
    int objc,			/* Parameter count */
    Tcl_Obj *const objv[])	/* Parameter vector */
{
    Tcl_Object thisObject = Tcl_ObjectContextObject(ctx);
				/* The current object */
    BddSystemData* sdata = (BddSystemData*)
	Tcl_ObjectGetMetadata(thisObject, &BddSystemDataType);
				/* The current system of expressions */
    int skipped = Tcl_ObjectContextSkippedArgs(ctx);
				/* The number of args used in method dispatch */
    BDD_BeadIndex operandIndex;	/* The bead index for the operand expression */
    BDD_BeadIndex resultIndex;	/* The bead index for the result expression */

    /* Check syntax */

    if (objc != skipped+2) {
	Tcl_WrongNumArgs(interp, skipped, objv, "result operand");
	return TCL_ERROR;
    }
    if (FindNamedExpression(interp, sdata, objv[skipped+1],
			    &operandIndex) != TCL_OK) {
	return TCL_ERROR;
    }
    resultIndex = BDD_Negate(sdata->system, operandIndex);
    SetNamedExpression(sdata, objv[skipped], resultIndex);
    return TCL_OK;
}
#endif

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemNthvarMethod --
 *
 *	Assigns a name to the nth variable in a BDD system
 *
 * Usage:
 *	$system nthvar name n
 *
 * Parameters:
 *	name - The name to assign
 *	n    - The variable index
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Assigns the given variable to the new name
 *
 *-----------------------------------------------------------------------------
 */

static int
BddSystemNthvarMethod(
    ClientData clientData,	/* Operation to perform */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_ObjectContext ctx,	/* Object context */
    int objc,			/* Parameter count */
    Tcl_Obj *const objv[])	/* Parameter vector */
{
    Tcl_Object thisObject = Tcl_ObjectContextObject(ctx);
				/* The current object */
    BddSystemData* sdata = (BddSystemData*)
	Tcl_ObjectGetMetadata(thisObject, &BddSystemDataType);
				/* The current system of expressions */
    int skipped = Tcl_ObjectContextSkippedArgs(ctx);
				/* The number of args used in method dispatch */
    int varNum;			/* The variable number */
    BDD_BeadIndex beadIndex;	/* The bead index for the old expression */

    /* Check syntax */

    if (objc != skipped+2) {
	Tcl_WrongNumArgs(interp, skipped, objv, "name n");
	return TCL_ERROR;
    }
    if (Tcl_GetIntFromObj(interp, objv[skipped+1], &varNum) != TCL_OK) {
	return TCL_ERROR;
    }
    beadIndex = BDD_NthVariable(sdata->system, (BDD_VariableIndex) varNum);
    SetNamedExpression(sdata, objv[skipped], beadIndex);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemNotnthvarMethod --
 *
 *	Assigns a name to the negation of the nth variable in a BDD system
 *
 * Usage:
 *	$system notnthvar name n
 *
 * Parameters:
 *	name - The name to assign
 *	n    - The variable index
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Assigns the negation of the given variable to the new name
 *
 *-----------------------------------------------------------------------------
 */

static int
BddSystemNotnthvarMethod(
    ClientData clientData,	/* Operation to perform */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_ObjectContext ctx,	/* Object context */
    int objc,			/* Parameter count */
    Tcl_Obj *const objv[])	/* Parameter vector */
{
    Tcl_Object thisObject = Tcl_ObjectContextObject(ctx);
				/* The current object */
    BddSystemData* sdata = (BddSystemData*)
	Tcl_ObjectGetMetadata(thisObject, &BddSystemDataType);
				/* The current system of expressions */
    int skipped = Tcl_ObjectContextSkippedArgs(ctx);
				/* The number of args used in method dispatch */
    int varNum;			/* The variable number */
    BDD_BeadIndex beadIndex;	/* The bead index for the old expression */

    /* Check syntax */

    if (objc != skipped+2) {
	Tcl_WrongNumArgs(interp, skipped, objv, "name n");
	return TCL_ERROR;
    }
    if (Tcl_GetIntFromObj(interp, objv[skipped+1], &varNum) != TCL_OK) {
	return TCL_ERROR;
    }
    beadIndex = BDD_NotNthVariable(sdata->system, (BDD_VariableIndex) varNum);
    SetNamedExpression(sdata, objv[skipped], beadIndex);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemSatcountMethod --
 *
 *	Computes and returns the number of satisfying assignments for
 *      a named expression in a system of BDD's
 *
 * Usage:
 *	$system satcount name
 *
 * Parameters:
 *	name - The name of the expression to analyze
 *
 * Results:
 *	Returns the count of satifsying assignments as a Tcl result
 *
 *-----------------------------------------------------------------------------
 */

static int
BddSystemSatcountMethod(
    ClientData clientData,	/* Operation to perform */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_ObjectContext ctx,	/* Object context */
    int objc,			/* Parameter count */
    Tcl_Obj *const objv[])	/* Parameter vector */
{
    Tcl_Object thisObject = Tcl_ObjectContextObject(ctx);
				/* The current object */
    BddSystemData* sdata = (BddSystemData*)
	Tcl_ObjectGetMetadata(thisObject, &BddSystemDataType);
				/* The current system of expressions */
    int skipped = Tcl_ObjectContextSkippedArgs(ctx);
				/* The number of args used in method dispatch */
    BDD_BeadIndex beadIndex;	/* The bead index for the old expression */
    mp_int satCount;

    /* Check syntax */

    if (objc != skipped+1) {
	Tcl_WrongNumArgs(interp, skipped, objv, "name");
	return TCL_ERROR;
    }
    if (FindNamedExpression(interp, sdata, objv[skipped],
			    &beadIndex) != TCL_OK) {
	return TCL_ERROR;
    }
    mp_init(&satCount);
    BDD_SatCount(sdata->system, beadIndex, &satCount);
    Tcl_SetObjResult(interp, Tcl_NewBignumObj(&satCount));

    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemUnsetMethod --
 *
 *	Forgets a named expression
 *
 * Usage:
 *	$system unset name
 *
 * Parameters:
 *	name - Name of the expression
 *
 * Results:
 *	None. Attempting to unset an expression that does not exist is
 *	ignored silently
 *
 *-----------------------------------------------------------------------------
 */

static int
BddSystemUnsetMethod(
    ClientData clientData,	/* unused */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_ObjectContext ctx,	/* Object context */
    int objc,			/* Parameter count */
    Tcl_Obj *const objv[])	/* Parameter vector */
{
    Tcl_Object thisObject = Tcl_ObjectContextObject(ctx);
				/* The current object */
    BddSystemData* sdata = (BddSystemData*)
	Tcl_ObjectGetMetadata(thisObject, &BddSystemDataType);
				/* The current system of expressions */
    int skipped = Tcl_ObjectContextSkippedArgs(ctx);
				/* The number of args used in method dispatch */

    /* Check syntax */

    if (objc != skipped+1) {
	Tcl_WrongNumArgs(interp, skipped, objv, "name");
	return TCL_ERROR;
    }
    UnsetNamedExpression(interp, sdata, objv[skipped]);
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
    Tcl_SetErrorCode(interp, "BDD", "noclone", NULL);
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
 * SetNamedExpression --
 *
 *	Sets a given name in a BDD system to refer to a specific bead.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The name is adjusted to refer to the given bead. The ref count of
 *	the bead is incremented.
 *
 *-----------------------------------------------------------------------------
 */

static void
SetNamedExpression(
    BddSystemData* sdata,	/* System of BDD's */
    Tcl_Obj* name,		/* Name of the expression */
    BDD_BeadIndex beadIndex)	/* Bead index */
{
    BDD_BeadIndex oldBeadIndex = 0;
				/* Previous bead index */
    int newFlag;		/* Flag == 1 if this is the first use of
				 * the name */
    Tcl_HashEntry* entryPtr = Tcl_CreateHashEntry(sdata->expressions, name,
						  &newFlag);
    if (!newFlag) {
	oldBeadIndex = (BDD_BeadIndex) (size_t) Tcl_GetHashValue(entryPtr);
    }
    BDD_IncrBeadRefCount(sdata->system, beadIndex);
    Tcl_SetHashValue(entryPtr, (ClientData) (size_t)beadIndex);
    if (!newFlag) {
	BDD_UnrefBead(sdata->system, oldBeadIndex);
    }
}

/*
 *-----------------------------------------------------------------------------
 *
 * FindHashEntryForNamedExpression --
 *
 *	Finds the hash entry corresponding to a named expression.
 *
 * Results:
 *	Returns a pointer to the hash entry, or NULL if no entry is found.
 *	If no entry is found and interp is not NULL, stores an appropriate
 *	error message in the interpreter result.
 *
 *-----------------------------------------------------------------------------
 */

static Tcl_HashEntry*
FindHashEntryForNamedExpression(
    Tcl_Interp* interp,		/* Tcl interpreter */
    BddSystemData* sdata,	/* System data for the BDD system */
    Tcl_Obj* name)		/* Name of the expression */
{
    Tcl_HashEntry* entryPtr = Tcl_FindHashEntry(sdata->expressions, name);
    Tcl_Obj* errorMessage;	/* Error message */

    if (entryPtr == NULL) {
	if (interp != NULL) {
	    errorMessage = Tcl_NewStringObj("expression named \"", -1);
	    Tcl_AppendToObj(errorMessage, Tcl_GetString(name), -1);
	    Tcl_AppendToObj(errorMessage, "\" not found", -1);
	    Tcl_SetObjResult(interp, errorMessage);
	    Tcl_SetErrorCode(interp, "BDD", "ExprNotFound", 
			     Tcl_GetString(name), NULL);
	}
    }
    return entryPtr;
}

/*
 *-----------------------------------------------------------------------------
 *
 * FindNamedExpression --
 *
 *	Finds the bead index corresponding to a named expression.
 *
 * Results:
 *	Returns a standard Tcl result
 *
 * Side effects:
 *	Sets *beadIndexPtr to the bead index if the name is found. If
 *	the name is not found, and 'interp' is not NULL, reports the
 *	error in the interpreter result.
 *
 *-----------------------------------------------------------------------------
 */

static int
FindNamedExpression(
    Tcl_Interp* interp,		/* Tcl interpreter */
    BddSystemData* sdata,	/* Data about the BDD system */
    Tcl_Obj* name,		/* Name of the expression */
    BDD_BeadIndex* beadIndexPtr)
				/* OUTPUT: Index of the bead */
{
    Tcl_HashEntry* entryPtr = 
	FindHashEntryForNamedExpression(interp, sdata, name);
    if (entryPtr == NULL) {
	return TCL_ERROR;
    } else {
	*beadIndexPtr = (BDD_BeadIndex) (size_t) Tcl_GetHashValue(entryPtr);
	return TCL_OK;
    }
}

/*
 *-----------------------------------------------------------------------------
 *
 * UnsetNamedExpression --
 *
 *	Unsets an expression given its name
 *
 * Results:
 *	Returns a standard Tcl result
 *
 * Side effects:
 *	If the given name designates an expression, unsets the expression and
 *	returns TCL_OK. If no such named expression is found, returns TCL_ERROR,
 *	and if interp is not NULL, places an appropriate error message in the
 *	interpreter result.
 *
 *-----------------------------------------------------------------------------
 */

static int
UnsetNamedExpression(
    Tcl_Interp* interp,		/* Tcl interpreter */
    BddSystemData* sdata,	/* Data about the BDD system */
    Tcl_Obj* name)		/* Name of the expression */
{
    Tcl_HashEntry* entryPtr = 
	FindHashEntryForNamedExpression(interp, sdata, name);
    if (entryPtr == NULL) {
	return TCL_ERROR;
    } else {
	BDD_UnrefBead(sdata->system,
		      (BDD_BeadIndex) (size_t)Tcl_GetHashValue(entryPtr));
	Tcl_DeleteHashEntry(entryPtr);
	return TCL_OK;
    }
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
    Tcl_DeleteHashTable(sdata->expressions);
    ckfree(sdata->expressions);
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
