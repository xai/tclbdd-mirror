#pragma once

#ifndef BDD_H_INCLUDED
#define BDD_H_INCLUDED 1

#include <stdlib.h>
#include <tclTomMath.h>

/*
 * Binary operators for use with BDD_Apply
 *
 * The ten truth tables that depend on both operands are included in the
 * set of operators.  The six trivial truth tables (always true, always
 * false, a, !a, b, !b) are not.
 */
typedef enum {			/* !a!b !ab a!b ab */

    /* BDD_BINOP_FALSE = 0x0, */

    BDD_BINOP_NOR = 0x1,	/* 1    0   0   0  */ /* a NOR b */

    BDD_BINOP_NOTINVIMP = 0x2,	/* 0    1   0   0  */ /* b does not imply a */
    BDD_BINOP_LT = 0x2,		                      /* a < b */

    /* BDD_BINOP_NOTFIRSTARG = 0x3, */

    BDD_BINOP_NOTIMP = 0x4,     /* 0    0   1   0  */ /* a does not imply b */
    BDD_BINOP_GT = 0x4,                               /* a > b */

    /* BDD_BINOP_NOTSECONDARG = 0x5, */

    BDD_BINOP_XOR = 0x6,	/* 0    1   1   0  */ /* a ^ b */
    BDD_BINOP_NE = 0x6,                               /* a != b */

    BDD_BINOP_NAND = 0x7,	/* 1    1   1   0  */ /* a NAND b */

    BDD_BINOP_AND = 0x8,	/* 0    0   0   1  */ /* a & b */

    BDD_BINOP_IFF = 0x9,	/* 1    0   0   1  */ /* a <-> b */
    BDD_BINOP_EQ = 0x9,                               /* a == b */

    /* BDD_BINOP_SECONDARG = 0xa, */

    BDD_BINOP_IMP = 0xb,	/* 1    1   0   1  */ /* a -> b */
    BDD_BINOP_LE = 0xb,                               /* a <= b */

    /* BDD_BINOP_FIRSTARG = 0xc, */

    BDD_BINOP_INVIMP = 0xd,	/* 1    0   1   1  */ /* a <- b */
    BDD_BINOP_GE = 0xd,                               /* a >= b */

    BDD_BINOP_OR = 0xe,         /* 0    1   1   1  */ /* a | b */

    /* BDD_BINOP_TRUE = 0xf */

} BDD_BinOp;

typedef struct BDD_System BDD_System;
typedef struct BDD_AllSatState BDD_AllSatState;

/* Type of a bead index */

typedef size_t BDD_BeadIndex;	/* Making this 'unsigned int'
				 * limits the size of a BDD system to
				 * 2**32 nodes but cuts the memory
				 * usage (and bus bandwidth) roughly in 
				 * half */
typedef unsigned int BDD_VariableIndex;
				/* Since the size of BDD's often is
				 * exponential in the number of variables,
				 * it's hard to imagine more than 
				 * an 'unsigned int' worth. */

/*
 * Value assignment: used in making conjunctions and restrictions,
 * and in reporting out satisfying expressions.
 */
typedef struct BDD_ValueAssignment {
    BDD_VariableIndex var;	/* Variable */
    int value;			/* Boolean value */
} BDD_ValueAssignment;

#define BDDAPI /* TODO: work out the export gubbins */

extern BDDAPI BDD_System* BDD_NewSystem(BDD_BeadIndex n);
extern BDDAPI void BDD_SetVariableCount(BDD_System* sysPtr,
					BDD_VariableIndex n);
extern BDDAPI BDD_VariableIndex BDD_GetVariableCount(BDD_System* sysPtr);
extern BDDAPI BDD_BeadIndex BDD_MakeBead(BDD_System* sysPtr,
					 BDD_VariableIndex level,
					 BDD_BeadIndex low,
					 BDD_BeadIndex high);
extern BDDAPI void BDD_IncrBeadRefCount(BDD_System* sysPtr, 
					BDD_BeadIndex bead);
extern BDDAPI void BDD_UnrefBead(BDD_System* sysPtr, BDD_BeadIndex bead);
extern BDDAPI BDD_BeadIndex BDD_NthVariable(BDD_System* sysPtr,
					    BDD_VariableIndex n);
extern BDDAPI BDD_BeadIndex BDD_NotNthVariable(BDD_System* sysPtr,
					       BDD_VariableIndex n);
extern BDDAPI int BDD_Literal(BDD_System* sysPtr, BDD_BeadIndex expr,
			      BDD_ValueAssignment* assignPtr);
extern BDDAPI BDD_BeadIndex BDD_Negate(BDD_System* sysPtr, BDD_BeadIndex u);
extern BDDAPI BDD_BeadIndex BDD_Apply(BDD_System* sysPtr, BDD_BinOp op,
				      BDD_BeadIndex u1, BDD_BeadIndex u2);
extern BDDAPI BDD_BeadIndex BDD_Restrict(BDD_System* sysPtr,
					 BDD_BeadIndex u,
					 const BDD_ValueAssignment r[],
					 BDD_VariableIndex n);
extern BDDAPI int BDD_SatCount(BDD_System* sysPtr, BDD_BeadIndex x,
			       mp_int* count);
extern BDDAPI BDD_AllSatState* BDD_AllSatStart(BDD_System* sysPtr,
					       BDD_BeadIndex u);
extern BDDAPI int BDD_AllSatNext(BDD_AllSatState* state,
				 BDD_ValueAssignment** vPtr,
				 BDD_VariableIndex* nPtr);
extern BDDAPI void BDD_AllSatFinish(BDD_AllSatState*);
extern BDDAPI int BDD_Dump(Tcl_Interp*, Tcl_Obj*, BDD_System*, BDD_BeadIndex);
extern BDDAPI void BDD_DeleteSystem(BDD_System* sysPtr);


#endif /* not BDD_H_INCLUDED */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * End:
 */
