/*
 * bdd.c --
 *
 *	C routines for manipulating Binary Decision Diagrams (BDD's)
 *	at a low level
 *
 * Copyright (c) 2013 by Kevin B. Kenny
 *
 *-----------------------------------------------------------------------------
 */

#include "bddInt.h"

#include <memory.h>

/*
 * Modulus to use when computing hash functions
 */
#define MODULUS 0xfffffffb

/*
 * Desirable fraction to fill hash tables
 */
#define HASHFRACTION 2

/*
 * Static functions defined in this file
 */
static unsigned int HashTableSize(unsigned int);
				/* Number of bits to use in a hash bucket
				 * index */


/*
 *-----------------------------------------------------------------------------
 *
 * HashTableSize --
 *
 *	Computes the number of bits to use in a hash bucket index
 *	when the index is expected to vary between 0 and n-1
 *
 * Results:
 *	Returns the number of bits, which is ceil(log2(n)).
 *
 *-----------------------------------------------------------------------------
 */

static inline unsigned int
HashTableSize(unsigned int n)	/* Number whose logarithm is needed. */
{
    unsigned int c = 0;		/* Return value */

    /*
     * Number of bits needed to represent the numbers 0x0-0xf
     */
    static const unsigned int logs[] = {
	1, 1, 2, 2,
	3, 3, 3, 3,
	4, 4, 4, 4,
	4, 4, 4, 4
    };

    n -= 1;

    /* Cast out 16-bit group */
    while (n & (~0x0ffff)) {
	c += 16;
	n >>= 16;
    }

    /* Cast out 8-bit group */
    if (n & (~0xff)) {
	c += 8;
	n >>= 8;
    }

    /* Cast out 4-bit group */
    if (n & (~0xf)) {
	c += 4;
	n >>= 4;
    }

    /* Add in log of most significant 4 bits */
    return c + logs[n];

}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_NewSystem --
 *
 *	Initializes a system of BDD's.
 *
 * Parameters:
 *	n - Initial number of beads to be shared among all BDD's in the system.
 *
 * Results:
 *	Returns an opaque pointer to the system that will hold the BDD's.
 *
 *-----------------------------------------------------------------------------
 */

BDD_System*
BDD_NewSystem(unsigned int n)
{
    unsigned int hashSize = 1 << HashTableSize((n + HASHFRACTION - 1)
					       / HASHFRACTION);
				/* Required size of the hash table */

    unsigned int i;

    /* Fail if the initial space does not request the two constant beads. */
    if (n < 3) return NULL;

    /* Allocate space for the system data structure. */
    BDD_System* sysPtr = (BDD_System*) ckalloc(sizeof(BDD_System));

    /* Allocate space for the bead array and the hash table */
    sysPtr->beads = (Bead*) ckalloc(n * sizeof(Bead));
    sysPtr->beadsAlloc = n;
    memset(sysPtr->beads, 0, n * sizeof(Bead));

    sysPtr->hashes = (unsigned int*) ckalloc(hashSize * sizeof(unsigned int));
    sysPtr->hashSize = hashSize;
    memset(sysPtr->hashes, 0, hashSize * sizeof(unsigned int));

    /* Initialize the constant beads. */

    sysPtr->beads[0].level = ~(unsigned int) 0;
    sysPtr->beads[0].refCount = 2;
    sysPtr->beads[1].level = ~(unsigned int) 0;
    sysPtr->beads[1].low = 1;
    sysPtr->beads[1].high = 1;
    sysPtr->beads[1].refCount = 2;

    /* Link the non-constant beads on the free list */

    for (i = 3; i < n; ++i) {
	sysPtr->beads[i-1].level = ~(unsigned int) 0;
	sysPtr->beads[i-1].next = i;
    }
    sysPtr->beads[n-1].level = ~(unsigned int) 0;
    sysPtr->beads[n-1].next = 2;
    sysPtr->unusedBead = n-1;

    return sysPtr;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_DeleteSystem --
 *
 *	Frees a system of Binary Decision Diagrams (BDD's) allocated by
 *	BDD_NewSystem.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	All BDD's in the system are deleted forcibly; all external
 *	references to them become invalid.
 *
 *-----------------------------------------------------------------------------
 */

void
BDD_DeleteSystem(
    BDD_System* sysPtr)		/* Pointer to system to delete */
{
    ckfree(sysPtr->hashes); 
    ckfree(sysPtr->beads);
    ckfree(sysPtr);
}
