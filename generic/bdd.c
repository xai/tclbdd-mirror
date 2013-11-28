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
static void RemoveFromHash(BDD_System* sysPtr,
				/* System of BDD's */
			   unsigned int bead);
				/* Bead to remove */

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
 * HashPair --
 *
 *	Combines two integers to make a hashcode for BDD management
 *
 * Results:
 *	Returns an integer that depends on the two input numbers
 *
 * ----------------------------------------------------------------------
 */

static inline unsigned int
HashPair(unsigned int i,	/* First integer to hash */
	 unsigned int j)	/* Second integer to hash */
{
    Tcl_WideUInt iw = i;
    iw += j;			/* i + j */
    iw *= (iw + 1);		/* (i + j)*(i + j + 1) */
    return (unsigned int)((iw >> 1) + i) % MODULUS;
				/* ((i + j)*(i + j + 1))/2 + i */
}

/*
 *-----------------------------------------------------------------------------
 *
 * HashTriple --
 *
 *	Computes a hash code for three integers in BDD management
 *
 * Results:
 *	Returns the hash code.
 *
 *-----------------------------------------------------------------------------
 */

static inline unsigned int
HashTriple(unsigned int level, unsigned int low, unsigned int high)
{
    return HashPair(level, HashPair(low, high));
}

/*
 *-----------------------------------------------------------------------------
 *
 * HashBead --
 *
 *	Computes the hash code for a bead in a BDD.
 *
 * Results:
 *	Returns a hash code based on 'level', 'low' and 'high'.
 *
 *-----------------------------------------------------------------------------
 */

static inline unsigned int
HashBead(Bead* b)		/* Bead whose hash code is needed */
{
    return HashTriple(b->level, b->low, b->high);
}

/*
 *-----------------------------------------------------------------------------
 *
 * RemoveFromHash --
 *
 *	Remove a deallocated bead from the hash table.
 *
 * Side effects:
 *
 *	The bead is unlinked from its hash bucket
 *
 * Notes:
 *	Hash statistics could be kept here.
 *
 *-----------------------------------------------------------------------------
 */

static inline void
RemoveFromHash(
    BDD_System* sysPtr,		/* System of BDDs in play */
    unsigned int bead)		/* Bead to unlink */
{
    Bead* beads = sysPtr->beads;
				/* Pointer to the deleted bead */
    unsigned int hash = HashBead(beads+bead) & (sysPtr->hashSize - 1);
				/* Hash code of the deleted bead */
    unsigned int p;		/* Index of the bead prior to this one */
    unsigned int next;		/* Index of the bead after p while searching */

    /* Delete the bead from the bucket */
    if (sysPtr->hashes[hash] == bead) {

	/* First bead in the bucket */
	sysPtr->hashes[hash] = beads[bead].next;
	return;

    } else {

	/* Search for the bead prior to this  bead. */
	for (p = sysPtr->hashes[hash]; p != 0; p = next) {
	    next = beads[p].next;
	    if (bead == next) {

		/* Found the bead; unlink this one from the chain. */
		beads[p].next = beads[bead].next;
		return;
	    }
	}
    }
    
    /* The bead wasn't in its hash bucket??? */

    Tcl_Panic("couldn't find delete bead(%d,%d,%d) in hashtable",
	      beads[bead].level, beads[bead].low, beads[bead].high);
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

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_IncrBeadRefCount --
 *
 *	Increments the reference count of a bead.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Advances the reference count.
 *
 *-----------------------------------------------------------------------------
 */

void
BDD_IncrBeadRefCount(BDD_System *sysPtr,
				/* System of BDD's */
		     unsigned int bead)
  				/* Bead to adjust  */
{
    if (bead > 1) {
	++(sysPtr->beads[bead].refCount);
    }
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_UnrefBead --
 *
 *	Decrements the reference count of a bead.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	If the reference count hits zero, removes the bead from the hash table,
 *	marks the bead as 'unused' by setting 'level' to all ones,
 *	and adds the bead to the free list.
 *
 *-----------------------------------------------------------------------------
 */

void
BDD_UnrefBead(
    BDD_System* sysPtr,		/* Pointer to the system of BDDs in play */
    unsigned int bead)		/* Index of the bead being unreferened */
{
    Bead* beads = sysPtr->beads;
				/* Bead table */

    if (bead <= 1) return;

    if (--(beads[bead].refCount) == 0) {

	/* Remove the bead from the hash table */
	RemoveFromHash(sysPtr, bead);
	
	/* Mark the bead as freed. */
	beads[bead].level = ~(unsigned int) 0;
	
	/* Add the bead to the free list as most recently used */
	if (sysPtr->unusedBead == 0) {
	    beads[bead].next = bead;
	} else {
	    beads[bead].next = beads[sysPtr->unusedBead].next;
	    beads[sysPtr->unusedBead].next = bead;
	}
	sysPtr->unusedBead = bead;
    }
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * End:
 */
