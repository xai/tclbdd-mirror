#pragma once

#ifndef BDD_H_INCLUDED
#define BDD_H_INCLUDED 1

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

#define BDDAPI /* TODO: work out the export gubbins */

extern BDDAPI BDD_System* BDD_NewSystem(unsigned int n);
extern BDDAPI void BDD_SetVariableCount(BDD_System* sysPtr, unsigned int n);
extern BDDAPI unsigned int BDD_GetVariableCount(BDD_System* sysPtr);
extern BDDAPI unsigned int BDD_NthVariable(BDD_System* sysPtr, unsigned int n);
extern BDDAPI unsigned int BDD_NotNthVariable(BDD_System* sysPtr,
					      unsigned int n);
extern BDDAPI unsigned int BDD_MakeBead(BDD_System* sysPtr, unsigned int level,
					unsigned int low, unsigned int high);
extern BDDAPI unsigned int BDD_Apply(BDD_System* sysPtr, BDD_BinOp op,
				     unsigned int u1, unsigned int u2);
extern BDDAPI int BDD_SatCount(BDD_System* sysPtr, unsigned int x,
			       mp_int* count);
extern BDDAPI void BDD_Dotify(BDD_System* sysPtr, unsigned int b);
extern BDDAPI void BDD_UnrefBead(BDD_System* sysPtr, unsigned int bead);
extern BDDAPI void BDD_DeleteSystem(BDD_System* sysPtr);

#endif

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * End:
 */
