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
    LIT_0 = 0,
    LIT_1 = 1,
    LIT_BDD_SYSTEM,
    LIT__END
};

/*
 * Structure that holds per-interpreter data for the tclbdd package.
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
static int CompareValueAssignments(const void* a, const void* b);
static int CompareVariableIndices(const void* a, const void* b);
static void DeletePerInterpData(PerInterpData*);
static int BddSystemConstructor(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				int, Tcl_Obj* const[]);
static int BddSystemBeadindexMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				    int, Tcl_Obj* const[]);
static int BddSystemBinopMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				int, Tcl_Obj* const[]);
static int BddSystemCopyMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
			       int, Tcl_Obj* const[]);
static int BddSystemDumpMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
			       int, Tcl_Obj* const[]);
static int BddSystemForeachSatMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				     int, Tcl_Obj *const[]);
static int ForeachSatPre(Tcl_Interp* interp, BddSystemData*,
			 Tcl_Obj* varName, Tcl_Obj* script,
			 BDD_AllSatState* stateVector);
static int ForeachSatPost(ClientData data[4], Tcl_Interp*, int);
static int BddSystemNegateMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				 int, Tcl_Obj* const[]);
static int BddSystemNotnthvarMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				    int, Tcl_Obj* const[]);
static int BddSystemNthvarMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				 int, Tcl_Obj* const[]);
static int BddSystemQuantifyMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				   int, Tcl_Obj* const[]);
static int BddSystemRestrictMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				   int, Tcl_Obj* const[]);
static int BddSystemSatcountMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
				   int, Tcl_Obj* const[]);
static int BddSystemTernopMethod(ClientData, Tcl_Interp*, Tcl_ObjectContext,
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
const static Tcl_MethodType BddSystemCopyMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "copy",			   /* name */
    BddSystemCopyMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
const static Tcl_MethodType BddSystemDumpMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "dump",			   /* name */
    BddSystemDumpMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
const static Tcl_MethodType BddSystemForeachSatMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "foreach_sat",		   /* name */
    BddSystemForeachSatMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
const static Tcl_MethodType BddSystemNegateMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "negate",			   /* name */
    BddSystemNegateMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
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
const static Tcl_MethodType BddSystemQuantifyMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "quantify",			   /* name */
    BddSystemQuantifyMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
const static Tcl_MethodType BddSystemRestrictMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "restrict",			   /* name */
    BddSystemRestrictMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
const static Tcl_MethodType BddSystemSatcountMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "satcount",			   /* name */
    BddSystemSatcountMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};
const static Tcl_MethodType BddSystemTernopMethodType = {
    TCL_OO_METHOD_VERSION_CURRENT, /* version */
    "ternop",			   /* name */
    BddSystemTernopMethod,	   /* callProc */
    DeleteMethod,		   /* method delete proc */
    CloneMethod			   /* method clone proc */
};				   /* common to all ternary operators */
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
    { "&3",        &BddSystemTernopMethodType,    (ClientData) BDD_TERNOP_AND },
    { ":=",        &BddSystemCopyMethodType,      NULL },
    { "<",         &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_LT },
    { "<=",        &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_LE },
    { "==",        &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_EQ },
    { ">",         &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_GT },
    { ">=",        &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_GE },
    { "^",         &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_XOR },
    { "^3",        &BddSystemTernopMethodType,    (ClientData) BDD_TERNOP_XOR },
    { "?:",        &BddSystemTernopMethodType,    
                                           (ClientData) BDD_TERNOP_IFTHENELSE },
    { "beadindex", &BddSystemBeadindexMethodType, NULL },
    { "borrow",    &BddSystemTernopMethodType, (ClientData) BDD_TERNOP_BORROW },
    { "concur3",   &BddSystemTernopMethodType, (ClientData) BDD_TERNOP_CONCUR },
    { "differ3",   &BddSystemTernopMethodType, (ClientData) BDD_TERNOP_DIFFER },
    { "dump",      &BddSystemDumpMethodType,      NULL },
    { "even3",     &BddSystemTernopMethodType,   (ClientData) BDD_TERNOP_EVEN },
    { "exists",	   &BddSystemQuantifyMethodType,  
      					        (ClientData) BDD_QUANT_EXISTS },
    { "forall",	   &BddSystemQuantifyMethodType,  
      					        (ClientData) BDD_QUANT_FORALL },
    { "foreach_sat",
                   &BddSystemForeachSatMethodType,NULL },
    { "median" ,   &BddSystemTernopMethodType, (ClientData) BDD_TERNOP_MEDIAN },
    { "nand",      &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_NAND },
    { "nand3",     &BddSystemTernopMethodType,   (ClientData) BDD_TERNOP_NAND },
    { "nor",       &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_NOR },
    { "nor3",      &BddSystemTernopMethodType,    (ClientData) BDD_TERNOP_NOR },
    { "notnthvar", &BddSystemNotnthvarMethodType, NULL },
    { "nthvar",    &BddSystemNthvarMethodType,    NULL },
    { "oneof3",    &BddSystemTernopMethodType,  (ClientData) BDD_TERNOP_ONEOF },
    { "restrict",  &BddSystemRestrictMethodType,  NULL },
    { "satcount",  &BddSystemSatcountMethodType,  NULL },
    { "twoof3",    &BddSystemTernopMethodType,  (ClientData) BDD_TERNOP_TWOOF },
    { "unset",     &BddSystemUnsetMethodType,     NULL },
    { "|",         &BddSystemBinopMethodType,     (ClientData) BDD_BINOP_OR },
    { "|3",        &BddSystemTernopMethodType,    (ClientData) BDD_TERNOP_OR },
    { "~",         &BddSystemNegateMethodType,    NULL },
    { NULL,	   NULL,                          NULL }
};

/*
 *-----------------------------------------------------------------------------
 *
 * Tclbdd_Init --
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
Tclbdd_Init(Tcl_Interp* interp)
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
    if (Tcl_TomMath_InitStubs(interp, TCL_VERSION) == NULL) {
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
 * BddSystemForeachSatMethod --
 *
 *	Iterates over all satisfying variable assignments of a BDD.
 *
 * Usage:
 *	$system foreach_sat $var $expr $script
 *
 * Parameters:
 *	system - System of BDD's (this object)
 *	var - Name of a Tcl variable that will hold an assignment
 *	expr - Name of the expression whose variable assignments are to
 *	       be enumerated
 *	script - Tcl script to execute once per satisfying assignment
 *
 * Results:
 *	Returns a standard Tcl result.
 *
 * Side effects:
 *	Whatever the script does.
 *
 * The script, like any Tcl loop body, may return any Tcl status code.
 * A status code of OK or CONTINUE advances to the next satisfying assignment,
 * or causes the loop to return OK if no more satisfying assignments exist.
 * A status code of BREAK causes the loop to return OK immediately.
 * A status code of RETURN, or any other return code, is reported as
 * the status of the 'foreach_sat' construct.
 *
 *-----------------------------------------------------------------------------
 */

static int
BddSystemForeachSatMethod(
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
    BDD_BeadIndex exprIndex;	/* Bead index of the named expression */
    BDD_AllSatState* stateVector;
				/* State vector for the enumeration */

    if (objc != skipped+3) {
	Tcl_WrongNumArgs(interp, skipped, objv, "varName exprName script");
	return TCL_ERROR;
    }
    if (FindNamedExpression(interp, sdata, objv[skipped+1],
			    &exprIndex) != TCL_OK) {
	return TCL_ERROR;
    }
    stateVector = BDD_AllSatStart(sdata->system, exprIndex);
    IncrBddSystemDataRefCount(sdata);
    return ForeachSatPre(interp, sdata, objv[skipped], objv[skipped+2],
			 stateVector);
}

/*
 *-----------------------------------------------------------------------------
 *
 * ForeachSatPre --
 *
 *	Sets up a call to a 'foreach_sat' loop body.
 *
 * Results:
 *	Returns a standard Tcl result.
 *
 * Side effects:
 *	If the enumeration has further assignments to process, gets the next
 *	one and sets up to invoke the loop body through Tcl_NREvalObj. 
 *	Otherwise, destroys the enumeration and returns TCL_OK to finish the
 *	loop.
 *
 *	Invoking the loop body, of course, has arbitrary side effects.
 *
 *-----------------------------------------------------------------------------
 */

static int
ForeachSatPre(
    Tcl_Interp* interp,		/* Tcl interpreter */
    BddSystemData* sdata,	/* System of BDD's */
    Tcl_Obj* varName,		/* Name of the loop variable */
    Tcl_Obj* script,		/* Loop body */
    BDD_AllSatState* stateVector) /* State vector for the enumeration */
{
    PerInterpData* pidata = sdata->pidata;
				/* Per-interpreter data */
    Tcl_Obj** literals = pidata->literals;
				/* Literal pool */
    BDD_ValueAssignment* v;	/* Value assignments for the current
				 * satisfier */
    BDD_VariableIndex n;	/* Count of value assignments */
    Tcl_Obj* sat;		/* Satisfying values */
    BDD_VariableIndex i;

    /*
     * Time to terminate the loop?
     */
    if (!BDD_AllSatNext(stateVector, &v, &n)) {
	BDD_AllSatFinish(stateVector);
	DecrBddSystemDataRefCount(sdata);
	return TCL_OK;
    }

    /*
     * Make a satisfying assignment
     */
    sat = Tcl_NewObj();
    for (i = 0; i < n; ++i) {
	Tcl_ListObjAppendElement(NULL, sat,
				 Tcl_NewWideIntObj((Tcl_WideInt) v[i].var));
	Tcl_ListObjAppendElement(NULL, sat, literals[v[i].value]);
    }
    Tcl_IncrRefCount(sat);

    /*
     * Stash the satisfying assignment in the loop variable
     */
    if (Tcl_ObjSetVar2(interp, varName, NULL, sat, TCL_LEAVE_ERR_MSG) == NULL) {
	Tcl_DecrRefCount(sat);
	BDD_AllSatFinish(stateVector);
	DecrBddSystemDataRefCount(sdata);
	return TCL_ERROR;
    }
    Tcl_DecrRefCount(sat);

    Tcl_NRAddCallback(interp, ForeachSatPost, 
		      (ClientData) sdata,
		      (ClientData) varName,
		      (ClientData) script,
		      (ClientData) stateVector);
    if (Tcl_NREvalObj(interp, script, 0) != TCL_OK) {
	BDD_AllSatFinish(stateVector);
	DecrBddSystemDataRefCount(sdata);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * ForeachSatPost --
 *
 *	Handle return from the loop body in a 'foreach_sat' construct.
 *
 * Results:
 *	Returns a standard Tcl return code.
 *
 * Side effects:
 *	Schedules the next iteration of the loop body.
 *
 * If the script returns TCL_OK or TCL_CONTINUE, this procedure schedules the
 * next iteration and returns TCL_OK.
 * If the script returns TCL_BREAK, this procedure returns TCL_OK without
 * scheduling the next iteration.
 * If the script returns TCL_RETURN, this procedure returns TCL_RETURN.
 * If the script returns TCL_ERROR or a nonstandard return code, this
 * procedure returns the same error or return code.
 *
 * The client data layout is:
 *	0 - Pointer to the BDD system data
 *	1 - Loop variable name
 *	2 - Loop body
 *	3 - State vector for the enumeration.
 *
 *-----------------------------------------------------------------------------
 */

static int
ForeachSatPost(
    ClientData clientData[4],	/* Client data */
    Tcl_Interp* interp,		/* Tcl interpreter */
    int result)			/* Result code from the loop body */
{
    BddSystemData* sdata = (BddSystemData*) clientData[0];
    Tcl_Obj* var = (Tcl_Obj*) clientData[1];
    Tcl_Obj* script = (Tcl_Obj*) clientData[2];
    BDD_AllSatState* stateVector = (BDD_AllSatState*) clientData[3];

    /* Handle the result and continue iterating if appropriate */

    switch(result) {
    case TCL_OK:
    case TCL_CONTINUE:
	/*
	 * We need to reset the result, so that an error message will not
	 * be appended to the result of the last evaluation.
	 */
	Tcl_ResetResult(interp);
	return ForeachSatPre(interp, sdata, var, script, stateVector);
    case TCL_BREAK:
	result = TCL_OK;
	Tcl_ResetResult(interp);
	break;
    case TCL_ERROR:
	Tcl_AppendObjToErrorInfo(interp,
				 Tcl_ObjPrintf("\n   ('foreach_sat' body, "
					       "line %d)",
					       Tcl_GetErrorLine(interp)));
	/* add to backtrace */
	break;
    default:
	break;
    }

    /* Clean up memory */
    BDD_AllSatFinish(stateVector);
    DecrBddSystemDataRefCount(sdata);
    return result;
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
    BDD_UnrefBead(sdata->system, resultIndex);
    return TCL_OK;
}

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
    BDD_UnrefBead(sdata->system, beadIndex);
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
    BDD_UnrefBead(sdata->system, beadIndex);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemQuantifyMethod --
 *
 *	Quantifies a BDD with respect to a particular set of variables.
 *
 * Usage:
 *	$system exists $name $vars $expr
 *	$system forall $name $vars $expr
 *	$system unique $name $vars $expr  (NOT YET IMPLEMENTED)
 *
 * Parameters:
 *	system - System of BDD's
 *	name - Name to assign to the resulting expression.
 *	vars - List of literals that must be un-negated variable names
 *	expr - Expression to quantify.
 *
 * Results:
 *	Returns a standard Tcl result
 *
 * Side effects:
 *	Creates the named expression if successful
 *
 *-----------------------------------------------------------------------------
 */
static int
BddSystemQuantifyMethod(
    ClientData clientData,	/* Operation to perform */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_ObjectContext ctx,	/* Object context */
    int objc,			/* Parameter count */
    Tcl_Obj *const objv[])	/* Parameter vector */
{
    BDD_Quantifier q = (BDD_Quantifier) clientData;
				/* The quantifier to apply */
    Tcl_Object thisObject = Tcl_ObjectContextObject(ctx);
				/* The current object */
    BddSystemData* sdata = (BddSystemData*)
	Tcl_ObjectGetMetadata(thisObject, &BddSystemDataType);
				/* The current system of expressions */
    int skipped = Tcl_ObjectContextSkippedArgs(ctx);
				/* The number of args used in method dispatch */
    BDD_BeadIndex u;		/* Expression to quantify */
    int varc;			/* Count of variable names supplied */
    Tcl_Obj** varv;		/* List of variable names supplied */
    BDD_BeadIndex literalIndex;	/* Index of the current variable as a BDD */
    BDD_VariableIndex* v;	/* Variables to quantify */
    BDD_ValueAssignment a;	/* Current quantified variable */
    Tcl_Obj* errorMessage;	/* Error message of a failed execution */
    BDD_BeadIndex result;	/* Result of the quantification */
    BDD_VariableIndex i;

    /* Check syntax */

    if (objc != skipped+3) {
	Tcl_WrongNumArgs(interp, skipped, objv, "name vars expr");
	return TCL_ERROR;
    }
    if (FindNamedExpression(interp, sdata, objv[skipped+2],
			    &u) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Tcl_ListObjGetElements(interp, objv[skipped+1],
			       &varc, &varv) != TCL_OK) {
	return TCL_ERROR;
    }
    v = ckalloc(varc);
    for (i = 0; i < varc; ++i) {
	if (FindNamedExpression(interp, sdata, varv[i],
				&literalIndex) != TCL_OK) {
	    ckfree(v);
	    return TCL_ERROR;
	}
	if (!BDD_Literal(sdata->system, literalIndex, &a)
	    || (a.value == 0)) {
	    errorMessage = Tcl_ObjPrintf("%s is not a variable",
					 Tcl_GetString(varv[i]));
	    Tcl_SetObjResult(interp, errorMessage);
	    Tcl_SetErrorCode(interp, "BDD", "NotVariable",
			     Tcl_GetString(varv[i]), NULL);
	    ckfree(v);
	    return TCL_ERROR;
	}	    
	v[i] = a.var;
    }

    /*
     * Order the literals
     */
    qsort(v, varc, sizeof(BDD_VariableIndex), CompareVariableIndices);

    /*
     * Quantify the formula
     */
    result = BDD_Quantify(sdata->system, q, v, varc, u);

    ckfree(v);
    SetNamedExpression(sdata, objv[skipped], result);
    BDD_UnrefBead(sdata->system, result);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BddSystemRestrictMethod --
 *
 *	Computes a BDD restricted to a particular set of variable values
 *
 * Usage:
 *	$system restrict name expr r1 r2 ...
 *
 * Parameters:
 *	name - The name to assign
 *	expr - The expression to restrict
 *	r1 r2 ... - Restrictions. The restrictions must be named expressions
 *                  resulting from 'nthvar' or 'notnthvar' (or other
 *                  expressions representing a single, possibly negated,
 *		    literal.
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
BddSystemRestrictMethod(
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
    BDD_BeadIndex expr;		/* The expression to restrict */
    BDD_VariableIndex nRestrictions = 0;
				/* How many restrictions? */
    BDD_ValueAssignment* restrictions = NULL;
    int i;
    BDD_VariableIndex j;
    BDD_BeadIndex literalIndex;
    Tcl_Obj* errorMessage;
    BDD_BeadIndex result;
    
    /* Check syntax */

    if (objc < skipped+2) {
	Tcl_WrongNumArgs(interp, skipped, objv, "name expr ?restriction?...");
	return TCL_ERROR;
    }
    if (FindNamedExpression(interp, sdata, objv[skipped+1],
			    &expr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (objc >= skipped+3) {
	nRestrictions = objc-skipped-2;
	restrictions = (BDD_ValueAssignment*)
	    ckalloc(nRestrictions * sizeof(BDD_ValueAssignment));;
	for (i = skipped+2, j = 0; i < objc; ++i, ++j) {
	    /* TODO - Refactor into FindLiteral and FindLiteralList */
	    if (FindNamedExpression(interp, sdata, objv[i],
				    &literalIndex) != TCL_OK) {
		ckfree(restrictions);
		return TCL_ERROR;
	    }
	    if (!BDD_Literal(sdata->system, literalIndex, restrictions+j)) {
		errorMessage = Tcl_ObjPrintf("%s is not a literal",
					     Tcl_GetString(objv[i]));
		Tcl_SetObjResult(interp, errorMessage);
		Tcl_SetErrorCode(interp, "BDD", "NotLiteral",
				 Tcl_GetString(objv[i]), NULL);
		ckfree(restrictions);
		return TCL_ERROR;
	    }
	}
    }
    
    /*
     * Order the literals
     */
    qsort(restrictions, nRestrictions, sizeof(BDD_ValueAssignment),
	  CompareValueAssignments);

    result = BDD_Restrict(sdata->system, expr, restrictions, nRestrictions);
    SetNamedExpression(sdata, objv[skipped], result);
    BDD_UnrefBead(sdata->system, result);
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
 * BddSystemTernopMethod --
 *
 *	Computes a ternary operation among three expressions in a BDD system
 *	and assigns it a name
 *
 * Usage:
 *	$system OP a b c d
 *
 * Parameters:
 *	OP - One of the ternary operators
 *	a - Name of the result expression
 *	b - Name of the first operand
 *	c - Name of the second operand
 *	d - Name of the third operand
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
BddSystemTernopMethod(
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
    BDD_BeadIndex beadIndexOpd3; /* The bead index of the third operand */
    BDD_BeadIndex beadIndexResult; /* The bead index of the result */

    /* Check syntax */

    if (objc != skipped+4) {
	Tcl_WrongNumArgs(interp, skipped, objv,
			 "result operand1 operand2 operand3");
	return TCL_ERROR;
    }
    if (FindNamedExpression(interp, sdata, objv[skipped+1],
			    &beadIndexOpd1) != TCL_OK
	|| FindNamedExpression(interp, sdata, objv[skipped+2],
			       &beadIndexOpd2) != TCL_OK
	|| FindNamedExpression(interp, sdata, objv[skipped+3],
			       &beadIndexOpd3) != TCL_OK) {
	return TCL_ERROR;
    }
    beadIndexResult = BDD_Apply3(sdata->system, (BDD_TernOp) (size_t)clientData,
				 beadIndexOpd1, beadIndexOpd2, beadIndexOpd3);
    SetNamedExpression(sdata, objv[skipped], beadIndexResult);
    BDD_UnrefBead(sdata->system, beadIndexResult);
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
 * CompareValueAssignments --
 *
 *	Callback for 'qsort' to compare value assignments based on their
 *	variable indices.
 *
 * Results:
 *	Returns -1 if a < b; 0 if a == b, 1 if a > b.
 *
 *-----------------------------------------------------------------------------
 */

static int
CompareValueAssignments(
    const void* a,		/* Pointer to first assignment */
    const void* b)		/* Pointer to second assignment */
{
    const BDD_ValueAssignment* aPtr = (const BDD_ValueAssignment*) a;
    const BDD_ValueAssignment* bPtr = (const BDD_ValueAssignment*) b;
    if (aPtr->var < bPtr->var) return -1;
    else if (aPtr->var > bPtr->var) return 1;
    else return 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 * CompareVariableIndices --
 *
 *	Callback for 'qsort' to compare	variable indices.
 *
 * Results:
 *	Returns -1 if a < b; 0 if a == b, 1 if a > b.
 *
 *-----------------------------------------------------------------------------
 */

static int
CompareVariableIndices(
    const void* a,		/* Pointer to first assignment */
    const void* b)		/* Pointer to second assignment */
{
    const BDD_VariableIndex* aPtr = (const BDD_VariableIndex*) a;
    const BDD_VariableIndex* bPtr = (const BDD_VariableIndex*) b;
    if (*aPtr<*bPtr) return -1;
    else if (*aPtr>*bPtr) return 1;
    else return 0;
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
