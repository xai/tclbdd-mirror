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
