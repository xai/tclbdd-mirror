#pragma once
#ifndef BDDINT_H_INCLUDED
#define BDDINT_H_INCLUDED 1

#include "bdd.h"

typedef struct Bead Bead;
typedef struct UIHash2 UIHash3;
typedef struct UIEntry2 UIEntry3;

/*
 * Structure that holds all the data for a system of Binary Decision Diagrams
 * (BDD's).
 *
 * The 'hashes' table is a special hash table whose implementation is in
 * the file 'bdd.c'.  It is an intrusive hash table: each Bead structure
 * is also a hash entry. The hash buckets are singly linked lists with
 * the 'next' entry designating, for each bucket, the next element in the
 * bucket.
 *
 * The reason that the 'hashes' table is handled specially is that it saves
 * an extra pointer per bead in an external hash table. Since every bead
 * is in the hash at all times, this turns out to be an important saving
 * in memory and time.
 */
struct BDD_System {
    Bead* beads;		/* Array of Bead structures holding the
				 * individual beads (shared among all BDD's). */
    BDD_BeadIndex beadsAlloc;	/* Number of elements allocated for the
				 * 'beads' array. */
    BDD_BeadIndex unusedBead;	/* Pointer to the most-recently used 
				 * free bead in the 'beads' array. This
				 * bead points to the least-recently-used,
				 * which in turn points to the next-least
				 * recently-used, and so on in a circular
				 * queue. */
    BDD_BeadIndex* hashes;	/* Array of indices for the first bead in each
				 * hash bucket. The hash table looks up beads
				 * by 'level', 'low' and 'high.*/
    BDD_BeadIndex hashSize;	/* Number of buckets in the hash table */

    Tcl_HashTable applyCache;	/* Cache of partial results for BDD_Apply */
    BDD_BinOp applyOp;		/* Operator for BDD_Apply */
    Tcl_HashTable apply3Cache;	/* Cache of partial results for BDD_Apply3 */
    BDD_TernOp apply3Op;	/* Operator for BDD_Apply3 */
    Tcl_HashTable composeCache;	/* Cache of partial results for BDD_Compose */
    BDD_VariableIndex composeCount;
				/* Number of variables being substituted
				 * in BDD_Compose */
    BDD_BeadIndex* composeReplacements;
				/* Replacement expressions for variables
				 * being substituted in BDD_Compose */
    Tcl_HashTable dumpCache;	/* Cache of visited nodes for BDD_Dump */
    Tcl_Interp* dumpInterp;	/* Tcl interpreter where BDD_Dump stores
				 * results */
    Tcl_Obj* dumpOutput;        /* Tcl object where BDD_Dump returns the dump */
    Tcl_HashTable negateCache;	/* Cache of partial results for BDD_Negate */
    BDD_Quantifier quantifier;	/* Quantifier for BDD_Quantify */
    Tcl_HashTable quantifyCache;
				/* Cache of partial results for BDD_Quantify */
    Tcl_HashTable restrictCache; 
				/* Cache of partial results for BDD_Restrict */
    Tcl_HashTable satCountCache;
				/* Cache of partial results for BDD_SatCount */
};

/*
 * State of an AllSat computation
 */
typedef enum BDD_AllSatSEnum {
    BDD_ALLSAT_START,		/* Initial entry */
    BDD_ALLSAT_SECONDTRY,	/* Did the low transition at this
				 * stack level, now try the high transition */
    BDD_ALLSAT_RETURN,		/* Did both transitions, retreat to the
				 * next earlier node */
} BDD_AllSatSEnum;

/*
 * Structure that manages the state of an AllSat computation
 */
struct BDD_AllSatState {
    BDD_System* sysPtr;		/* System of BDD's */
    BDD_BeadIndex* uStack;	/* Stack of beads from the root to the current
				 * point */
    unsigned char* sStack;	/* Stack of state at each bead */
    BDD_ValueAssignment* v;	/* Incomplete satisfying assignment.
				 * This array is of the same size as the
				 * stack depth and traces the choice of
				 * variable and value at each level. */
    BDD_VariableIndex depth;	/* Stack depth of the three stacks */
};

/*
 * Structure that represents a single bead in a BDD 
 */
struct Bead {
    BDD_BeadIndex low;		/* Index of the next bead if the variable is
				 * false */
    BDD_BeadIndex high;		/* Index of the next bead if the variable is
				 * true */
    BDD_BeadIndex next;		/* Index of the next bead in the same
				 * hash bucket as this bead. */
    BDD_BeadIndex refCount;	/* Reference count of this bead */
    BDD_VariableIndex level;	/* Level in the diagram (in other words,
				 * the index of the variable being examined) */
};

#endif 


/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * End:
 */
