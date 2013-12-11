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
typedef enum BDD_BinOp {	/* !a!b !ab a!b ab */

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

/*
 * Ternary operations for use with BDD_Apply3
 *
 *	There are 2**2**3 = 256 possible operations. This is merely
 * a selection of the most useful ones.
 */
typedef enum BDD_TernOp {
    BDD_TERNOP_NOR = 0x01,	/* Logical NOR of three variables */
    BDD_TERNOP_ONEOF = 0x16,	/* Exactly one of three variables TRUE */
    BDD_TERNOP_TWOOF = 0x68,	/* Exactly two of three variables TRUE */
    BDD_TERNOP_EVEN = 0x69,	/* Exclusive NOR: zero or two variables TRUE */
    BDD_TERNOP_DIFFER = 0x7E,	/* 1 or 2 variables TRUE - disagreement. */
    BDD_TERNOP_NAND = 0x7F,	/* Logical NAND of three variables */
    BDD_TERNOP_AND = 0x80,	/* Logical AND of three variables */
    BDD_TERNOP_CONCUR = 0x81,	/* All three variables agree */
    BDD_TERNOP_BORROW = 0x8E,	/* Borrow bit of a full subtractor */
                                /* -(a-b-c) < 0 */
    BDD_TERNOP_XOR = 0x96,	/* Exclusive OR: one or three variables TRUE */
    BDD_TERNOP_IFTHENELSE = 0xCA, /* a ? b : c */
    BDD_TERNOP_MEDIAN = 0xE8,	/* Majority rule: at least two of three. */
				/* This function is the median of the three */
				/* variables. This function is also the */
				/* carry bit of a full adder: (a+b+c) > 1 */
    BDD_TERNOP_OR = 0xFE,	/* Logical OR of three variables */
} BDD_TernOp;

/*
 * Quantifiers for use with BDD's
 */
typedef enum BDD_Quantifier {
    BDD_QUANT_FORALL = BDD_BINOP_AND, /* Universal quantification */
    /* BDD_QUANT_UNIQUE - BDD_BINOP_XOR, Unique quantification is not working */
    BDD_QUANT_EXISTS = BDD_BINOP_OR,  /* Existential quantification */
} BDD_Quantifier;

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
 * Value assignment: used in making conjunctions, compositions and restrictions,
 * and in reporting out satisfying expressions.
 */
typedef struct BDD_ValueAssignment {
    BDD_VariableIndex var;	/* Variable */
    BDD_BeadIndex value;	/* Boolean value. Must be constant for
				 * conjunctions and restrictions.
				 * Will be constant for satisfying assignments.
				 * May be an arbitrary bead for compositions. */
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
extern BDDAPI BDD_BeadIndex BDD_GarbageCollect(BDD_System* sysPtr);
extern BDDAPI BDD_BeadIndex BDD_NthVariable(BDD_System* sysPtr,
					    BDD_VariableIndex n);
extern BDDAPI BDD_BeadIndex BDD_NotNthVariable(BDD_System* sysPtr,
					       BDD_VariableIndex n);
extern BDDAPI int BDD_Literal(BDD_System* sysPtr, BDD_BeadIndex expr,
			      BDD_ValueAssignment* assignPtr);
extern BDDAPI BDD_BeadIndex BDD_Negate(BDD_System* sysPtr, BDD_BeadIndex u);
extern BDDAPI BDD_BeadIndex BDD_Apply(BDD_System* sysPtr, BDD_BinOp op,
				      BDD_BeadIndex u1, BDD_BeadIndex u2);
extern BDDAPI BDD_BeadIndex BDD_Apply3(BDD_System* sysPtr, BDD_TernOp op,
				       BDD_BeadIndex u1, BDD_BeadIndex u2,
				       BDD_BeadIndex u3);
extern BDDAPI BDD_BeadIndex BDD_Restrict(BDD_System* sysPtr,
					 BDD_BeadIndex u,
					 const BDD_ValueAssignment r[],
					 BDD_VariableIndex n);
extern BDDAPI BDD_BeadIndex BDD_Quantify(BDD_System* sysPtr,
					 BDD_Quantifier q,
					 BDD_VariableIndex n,
					 const BDD_VariableIndex v[],
					 BDD_BeadIndex u);
extern BDDAPI BDD_BeadIndex BDD_Compose(BDD_System* sysPtr,
					BDD_BeadIndex u,
					BDD_VariableIndex nVars,
					BDD_BeadIndex replacements[]);
extern BDDAPI BDD_BeadIndex BDD_ApplyAndQuantify(BDD_System* sysPtr, 
						 BDD_Quantifier q,
						 BDD_VariableIndex n,
						 const BDD_VariableIndex v[],
						 BDD_BinOp op,
						 BDD_BeadIndex u1, 
						 BDD_BeadIndex u2);
extern BDDAPI int BDD_SatCount(BDD_System* sysPtr, BDD_BeadIndex x,
			       mp_int* count);
extern BDDAPI BDD_AllSatState* BDD_AllSatStart(BDD_System* sysPtr,
					       BDD_BeadIndex u);
extern BDDAPI int BDD_AllSatNext(BDD_AllSatState* state,
				 BDD_ValueAssignment** vPtr,
				 BDD_VariableIndex* nPtr);
extern BDDAPI void BDD_AllSatFinish(BDD_AllSatState*);
extern BDDAPI void BDD_Profile(BDD_System* sysPtr, BDD_BeadIndex u,
			       BDD_VariableIndex n, BDD_BeadIndex counts[]);
extern BDDAPI int BDD_Dump(Tcl_Interp*, Tcl_Obj*, BDD_System*, BDD_BeadIndex);
extern BDDAPI void BDD_DeleteSystem(BDD_System* sysPtr);


#endif /* not BDD_H_INCLUDED */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * End:
 */
