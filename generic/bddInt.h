#pragma once
#ifndef BDDINT_H_INCLUDED
#define BDDINT_H_INCLUDED 1

#include "bdd.h"

typedef struct Bead Bead;
typedef struct UIHash1 UIHash1;
typedef struct UIEntry1 UIEntry1;
typedef struct UIHash1MP UIHash1MP;
typedef struct UIEntry1MP UIEntry1MP;
typedef struct UIHash2 UIHash2;
typedef struct UIEntry2 UIEntry2;
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
    BDD_ValueAssignment* v;	/* Incomplete satisfying assignment */
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

/*
 * Structure that represents a hash table with a key comprising a single
 * unsigned integer and a value consisting of a single unsigned integer
 */
struct UIHash1 {
    UIEntry1MP* entries;	/* Array of hashtable entries */
    BDD_BeadIndex* hashes;	/* Array of heads of hash buckets */
    BDD_BeadIndex unusedEntry;	/* Index of the last unused entry */
    BDD_BeadIndex entriesAlloc;	/* Count of allocated entries */
    BDD_BeadIndex hashSize;	/* Count of allocated buckets */
};

/*
 * Structure that represents a single hash entry in a UIHash1
 */
struct UIEntry1 {
    BDD_BeadIndex key;		/* Key */
    BDD_BeadIndex next;		/* Next hash entry in the same bucket */
    BDD_BeadIndex value;	/* Value */
};

/*
 * Structure that represents a hash table with a key comprising a single
 * unsigned integer and a value consisting of a single mp_int
 */
struct UIHash1MP {
    UIEntry1MP* entries;	/* Array of hashtable entries */
    BDD_BeadIndex* hashes;	/* Array of heads of hash buckets */
    BDD_BeadIndex unusedEntry;	/* Index of the last unused entry */
    BDD_BeadIndex entriesAlloc;	/* Count of allocated entries */
    BDD_BeadIndex hashSize;	/* Count of allocated buckets */
};

/*
 * Structure that represents a single hash entry in a UIHash1MP
 */
struct UIEntry1MP {
    BDD_BeadIndex key;		/* Key */
    BDD_BeadIndex next;		/* Next hash entry in the same bucket */
    mp_int value;		/* Value */
};

/*
 * Structure that represents a hash table with a key comprising two
 * unsigned integers and a value consisting of a single one.
 */
struct UIHash2 {
    UIEntry2* entries;		/* Array of hashtable entries */
    BDD_BeadIndex* hashes;	/* Array of heads of hash buckets */
    BDD_BeadIndex unusedEntry;	/* Index of the last unused entry */
    BDD_BeadIndex entriesAlloc;	/* Count of allocated entries */
    BDD_BeadIndex hashSize;	/* Count of allocated buckets */
};

/*
 * Structure that represents a single hash entry in a UIHash2 
 */
struct UIEntry2 {
    BDD_BeadIndex key1;		/* First component of key */
    BDD_BeadIndex key2;		/* Second component of key */
    BDD_BeadIndex next;		/* Next hash entry in the same bucket */
    BDD_BeadIndex value;	/* Value */
};

#endif 


/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * End:
 */
