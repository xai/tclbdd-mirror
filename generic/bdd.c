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
#define HASHFRACTION 4

/*
 * Static functions defined in this file
 */
static void AddToHash(BDD_System*, BDD_BeadIndex);
				/* Add a bead to the system hash */
static unsigned int Bead2HashKeyProc(Tcl_HashTable*, void*);
static int Bead2HashKeyComparator(void*, Tcl_HashEntry*);
static Tcl_HashEntry* Bead2HashEntryAlloc(Tcl_HashTable*, void*);
static int FindInHash(BDD_System*, BDD_VariableIndex, BDD_BeadIndex,
		      BDD_BeadIndex high);
				/* Find a bead in the system hash */
static unsigned int HashPair(BDD_BeadIndex, BDD_BeadIndex);
				/* Hashcode for a pair of integers */
static unsigned int HashTriple(BDD_VariableIndex, BDD_BeadIndex,
				BDD_BeadIndex);
				/* Hashcode for a triple of integers */
static unsigned int HashBead(const Bead*);
				/* Hashcode for a bead */
static int Log2HashTableSize(BDD_BeadIndex);
				/* Number of bits to use in a hash bucket
				 * index */
static void RemoveFromHash(BDD_System* sysPtr, BDD_BeadIndex bead);
				/* Remove a bead from the main hash */

/*
 * Hash entry type for an object indexed by two bead indices
 */
typedef struct Bead2HashEntry {
    Tcl_HashEntry parent;	/* Based on the Tcl_HashEntry */
    BDD_BeadIndex beads[2];	/* The two bead indices */
} Bead2HashEntry;

/*
 * Hash key type for an object indexed by two bead indices
 */

Tcl_HashKeyType Bead2KeyType = {
    TCL_HASH_KEY_TYPE_VERSION,	/* version */
    0,				/* flags */
    Bead2HashKeyProc,		/* hash procedure */
    Bead2HashKeyComparator,	/* comparator */
    Bead2HashEntryAlloc,	/* allocator */
    NULL			/* standard deallocator */
};
/*
 *-----------------------------------------------------------------------------
 *
 * Log2HashTableSize --
 *
 *	Computes the number of bits to use in a hash bucket index
 *	when the index is expected to vary between 0 and n-1
 *
 * Results:
 *	Returns the number of bits, which is ceil(log2(n)).
 *
 *-----------------------------------------------------------------------------
 */

static inline int
Log2HashTableSize(BDD_BeadIndex n)	/* Number whose logarithm is needed. */
{
    BDD_BeadIndex c = 0;		/* Return value */

    /*
     * Number of bits needed to represent the numbers 0x0-0xf
     */
    static const BDD_BeadIndex logs[] = {
	1, 1, 2, 2,
	3, 3, 3, 3,
	4, 4, 4, 4,
	4, 4, 4, 4
    };

    n -= 1;

    /* Cast out 16-bit group */
    while (n & (~(BDD_BeadIndex) 0x0ffff)) {
	c += 16;
	n >>= 16;
    }

    /* Cast out 8-bit group */
    if (n & (~ (BDD_BeadIndex) 0xff)) {
	c += 8;
	n >>= 8;
    }

    /* Cast out 4-bit group */
    if (n & (~ (BDD_BeadIndex) 0xf)) {
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
HashPair(BDD_BeadIndex i,	/* First integer to hash */
	 BDD_BeadIndex j)	/* Second integer to hash */
{
    /* 
     * Tcl_WideUInt is the widest type that we can presume is available
     * without resorting to mp_ints. We presume that its maximum value
     * is at least MODULUS**2. We also presume that the total number
     * of entries in a hash table is MODULUS * HASHFRACTION. On a 64-bit
     * machine, a Bead is 36 bytes, MODULUS is nearly 2**32, and HASHFRACTION
     * is 4, limiting the total memory consumed by the bead array in
     * a BDD_System to about half a terabyte. Presume that's OK for now.
     */
    Tcl_WideUInt iw = i % MODULUS;
    iw += j % MODULUS;		/* i + j */
    iw *= (iw + 1);		/* (i + j)*(i + j + 1) */
    return (BDD_BeadIndex)((iw >> 1) + i) % MODULUS;
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
HashTriple(BDD_VariableIndex level, BDD_BeadIndex low, BDD_BeadIndex high)
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
HashBead(const Bead* b)		/* Bead whose hash code is needed */
{
    return HashTriple(b->level, b->low, b->high);
}

/*
 *-----------------------------------------------------------------------------
 *
 * Bead2HashKeyProc --
 *
 *	Compute a hashcode from a pair of bead indices
 *
 * Results:
 *	Returns the computed hashcode.
 */

static unsigned int
Bead2HashKeyProc(
    Tcl_HashTable* hashTable,	/* Hash table being processed */
    void* key)			/* Key */
{
    BDD_BeadIndex* beads = (BDD_BeadIndex*) key;
    return HashPair(beads[0], beads[1]);
}

/*
 *-----------------------------------------------------------------------------
 *
 * Bead2HashKeyComparator --
 *
 *	Compares the keys in a hash table indexed by two bead indices
 *
 * Results:
 *	Returns 1 if the keys are equal, 0 otherwise.
 *
 *-----------------------------------------------------------------------------
 */

static int
Bead2HashKeyComparator(
    void* key1VPtr,		/* Pointer to the first key */
    Tcl_HashEntry* entryPtr)
				/* Pointer to a hash entry containing the
				 * second key */
{
    BDD_BeadIndex* key1Ptr = (BDD_BeadIndex*) key1VPtr;
    BDD_BeadIndex* key2Ptr = (BDD_BeadIndex*) &(entryPtr->key.oneWordValue);
;
    return (key1Ptr[0] == key2Ptr[0] && key1Ptr[1] == key2Ptr[1]);
}

/*
 *-----------------------------------------------------------------------------
 *
 * Bead2HashEntryAlloc --
 *
 *	Allocates a hash entry containing two BDD_BeadIndex keys
 *
 * Results:
 *	Returns the allocated entry
 *
 *-----------------------------------------------------------------------------
 */
static Tcl_HashEntry*
Bead2HashEntryAlloc(
    Tcl_HashTable* tablePtr,	/* Hash table */
    void* keyPtr		/* Key */
) {
    BDD_BeadIndex* beads = (BDD_BeadIndex*) keyPtr;
    Tcl_HashEntry* entryPtr =
	(Tcl_HashEntry*) ckalloc(sizeof(Tcl_HashEntry)
				 - sizeof(entryPtr->key)
				 + 2 * sizeof(BDD_BeadIndex));
    BDD_BeadIndex* outBeads = (BDD_BeadIndex*) &(entryPtr->key.oneWordValue);
    outBeads[0] = beads[0];
    outBeads[1] = beads[1];
    return entryPtr;
}

/*
 *-----------------------------------------------------------------------------
 *
 * AddToHash --
 *
 *	Adds a bead to the hash table.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Links the bead into the appropriate hash bucket.
 *
 * Notes:
 *	Might want to profile what fraction of times a node was added to
 *	a nonempty bucket.
 *
 *-----------------------------------------------------------------------------
 */

static inline void
AddToHash(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BeadIndex bead)		/* Index of the bead being added */
{
    Bead* beadPtr = sysPtr->beads+bead;
				/* Pointer to the bead being added */
    BDD_BeadIndex bucket = HashBead(beadPtr) & (sysPtr->hashSize - 1);
				/* Hash bucket contaiing the bead */
    beadPtr->next = sysPtr->hashes[bucket];
    sysPtr->hashes[bucket]=bead;
}

/*
 *-----------------------------------------------------------------------------
 *
 * FindInHash --
 *
 *	Hash table lookup for a bead in a set of BDD's.
 *
 * Results:
 *	Returns the bead's index if found, 0 otherwise
 *
 * Notes:
 *	Might want to profile total searches, successful searches,
 *	probes required per successful and unsuccessful search.
 *
 *-----------------------------------------------------------------------------
 */

static inline int
FindInHash(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_VariableIndex level,	/* Level (variable index) of the desired bead */
    BDD_BeadIndex low,		/* 'false' transition of the desired bead */
    BDD_BeadIndex high)		/* 'true' transition of the desired bead */
{
    unsigned int bucket = HashTriple(level, low, high)
	& (sysPtr->hashSize - 1);
				/* Hash bucket holding the bead. */
    Bead* beads = sysPtr->beads;
    BDD_BeadIndex p;

    /* 
     * Walk the linked list starting from the hash bucket, looking for
     * the bead in question.
     */

    for (p = sysPtr->hashes[bucket]; p; p = beads[p].next) {
	Bead* beadPtr = sysPtr->beads+p;
	if (beadPtr->level == level
	    && beadPtr->low == low
	    && beadPtr->high == high) {
	  return p;
	}
    }

    /*
     * The bead in question is not in the hash
     */

    return 0;
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
    BDD_BeadIndex bead)		/* Bead to unlink */
{
    Bead* beads = sysPtr->beads;
				/* Pointer to the deleted bead */
    BDD_BeadIndex hash = HashBead(beads+bead) & (sysPtr->hashSize - 1);
				/* Hash code of the deleted bead */
    BDD_BeadIndex p;		/* Index of the bead prior to this one */
    BDD_BeadIndex next;		/* Index of the bead after p while searching */

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

    Tcl_Panic("couldn't find delete bead(%llu,%llu,%llu) in hashtable",
	      (unsigned long long) beads[bead].level,
	      (unsigned long long) beads[bead].low,
	      (unsigned long long) beads[bead].high);
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
BDD_NewSystem(BDD_BeadIndex n)
{
    BDD_BeadIndex hashSize = 
	(BDD_BeadIndex) 1 << Log2HashTableSize((n + HASHFRACTION - 1)
					       / HASHFRACTION);
				/* Required size of the hash table */

    BDD_BeadIndex i;

    /* Fail if the initial space does not request the two constant beads. */
    if (n < 3) return NULL;

    /* Allocate space for the system data structure. */
    BDD_System* sysPtr = (BDD_System*) ckalloc(sizeof(BDD_System));

    /* Allocate space for the bead array and the hash table */
    sysPtr->beads = (Bead*) ckalloc(n * sizeof(Bead));
    sysPtr->beadsAlloc = n;
    memset(sysPtr->beads, 0, n * sizeof(Bead));

    sysPtr->hashes = (BDD_BeadIndex*) ckalloc(hashSize * sizeof(BDD_BeadIndex));
    sysPtr->hashSize = hashSize;
    memset(sysPtr->hashes, 0, hashSize * sizeof(BDD_BeadIndex));
    sysPtr->varCount = 0;

    /* Initialize the constant beads. */

    sysPtr->beads[0].level = ~(BDD_VariableIndex) 0;
    sysPtr->beads[0].refCount = 2;
    sysPtr->beads[1].level = ~(BDD_VariableIndex) 0;
    sysPtr->beads[1].low = 1;
    sysPtr->beads[1].high = 1;
    sysPtr->beads[1].refCount = 2;

    /* Link the non-constant beads on the free list */

    for (i = 3; i < n; ++i) {
	sysPtr->beads[i-1].level = ~(BDD_VariableIndex) 0;
	sysPtr->beads[i-1].next = i;
    }
    sysPtr->beads[n-1].level = ~(BDD_VariableIndex) 0;
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
 * BDD_GetVariableCount --
 *
 *	Returns the number of variables that a BDD system supports
 *
 * Results:
 *	Returns the number of variables that a BDD system supports
 *
 * Side effects:
 *	None.
 *
 *-----------------------------------------------------------------------------
 */

BDD_VariableIndex
BDD_GetVariableCount(BDD_System* sysPtr)
{
    return sysPtr->varCount;
}

/*
 *-----------------------------------------------------------------------------
 *
 * GrowSystemAndRehash --
 *
 *	Expands the space allocated to the bead table, and rebuilds the
 *	hash table.
 *
 * Results:
 *	Returns 0 if successful, 1 if there should be an attempt to
 *	reorder variables.
 *
 * Side effects:
 *	Hash table and bead table both double in size.
 *
 * Notes:
 *	This procedure would be the correct place to check whether the system
 *	has grown excessive and attempt dynamic reordering if it has.
 *	Note that if it happens here, there needs to be some work to
 *	unwind and restart operations in process -- ideally without
 *	unbridled longjmp'ing. For the moment, we ignore the problem.
 *
 *-----------------------------------------------------------------------------
 */

static inline int
GrowSystemAndRehash(
    BDD_System* sysPtr)
{
    BDD_BeadIndex oldAlloc = sysPtr->beadsAlloc;
				/* Former size of the bead table */
    BDD_BeadIndex newAlloc = 2 * oldAlloc;
				/* New size of the bead table */
    Bead* beads;		/* New bead table */
    BDD_BeadIndex hashSize = sysPtr->hashSize * 2;
				/* New size of the hash table */
    BDD_BeadIndex i;

    /* Reallocate the beads */
    beads = (Bead*) ckrealloc((char*) (sysPtr->beads), newAlloc * sizeof(Bead));
    memset(beads+oldAlloc, 0, (newAlloc-oldAlloc) * sizeof(Bead));
    sysPtr->beads = beads;
    sysPtr->beadsAlloc = newAlloc;

    /* Link the beads together to splice them into the free list */
    for (i = oldAlloc + 1; i < newAlloc; ++i) {
	beads[i-1].level = ~(BDD_VariableIndex) 0;
	beads[i-1].next = i;
    }
    beads[newAlloc-1].level = ~(BDD_VariableIndex) 0;

    /* Splice the beads into the free list as the least recently used. */
    if (sysPtr->unusedBead != 0) {
	beads[newAlloc-1].next = beads[sysPtr->unusedBead].next;
	beads[sysPtr->unusedBead].next = oldAlloc;
    } else {
	beads[newAlloc-1].next = oldAlloc;
	sysPtr->unusedBead = newAlloc-1;
    }

    /* Destroy the old hash table */
    ckfree(sysPtr->hashes);

    /* Make a new hash table */
    sysPtr->hashes = (BDD_BeadIndex*) ckalloc(hashSize * sizeof(BDD_BeadIndex));
    memset(sysPtr->hashes, 0, hashSize * sizeof(BDD_BeadIndex));
    sysPtr->hashSize = hashSize;
    
    /* Add allocated beads to the hash table. */
    for (i = 2; i < oldAlloc; ++i) {
	if (beads[i].level != ~(BDD_VariableIndex) 0) {
	    AddToHash(sysPtr, i);
	}
    }

    /* For now, no automatic reordering. */
    return 0;
}    

/*
 *-----------------------------------------------------------------------------
 *
 * AddBead --
 *
 *	Adds a bead to a system of BDD's under construction
 *
 * Results:
 *	Returns the index of the bead
 *
 * Side effects:
 *	Adds the bead to the table of beads in play.  Increments the
 *	reference count of the two successor beads.
 *
 *-----------------------------------------------------------------------------
 */

static inline BDD_BeadIndex
AddBead(
    BDD_System* sysPtr,		/* System of BDDs in play */
    BDD_VariableIndex level,	/* Variable index of the bead to add */
    BDD_BeadIndex low,		/* Low successor */
    BDD_BeadIndex high)		/* High successor */
{
    BDD_BeadIndex bead;		/* Index of the added bead */
    Bead* beadPtr;		/* Memory address of the added bead */
    Bead* beads;

    /* If there is no free space, grow the tables. */
    if (sysPtr->unusedBead == 0) {
	GrowSystemAndRehash(sysPtr);
    }

    /*
     * If the bead designates a variable not seen before, advance the
     * system variable count
     */
    if (level >= sysPtr->varCount) {
	sysPtr->varCount = level+1;
    }

    /* 
     * Allocate a bead from the free list. Until this point, its successor
     * beads will have remained allocated. Free them now. (This ensures
     * that individual allocations and frees run in constant time.)
     */
    beads = sysPtr->beads;
    bead = beads[sysPtr->unusedBead].next;
    beadPtr = beads + bead;
    if (bead != beadPtr->next) {
	beads[sysPtr->unusedBead].next = beadPtr->next;
    } else {
	sysPtr->unusedBead = 0;
    }
    if (beadPtr->low > 1) {
	BDD_UnrefBead(sysPtr, beadPtr->low);
    }
    if (beadPtr->high > 1) {
	BDD_UnrefBead(sysPtr, beadPtr->high);
    }

    /* Initialize the bead content */
    beadPtr->level = level;
    beadPtr->low = low;
    beadPtr->high = high;
    beadPtr->next = 0;
    beadPtr->refCount = 0;

    /* Increment the refcounts of the successors */
    if (low > 1) {
	++sysPtr->beads[low].refCount;
    }
    if (high > 1) {
	++sysPtr->beads[high].refCount;
    }

    return bead;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_MakeBead --
 *
 *	Searches for a bead in the hash table and constructs one if it is
 *	not found.
 *
 * Results:
 *	Returns the index of the bead.
 *
 * Side effects:
 *	Creates a bead if necessary. Hashes the new bead if one is created.
 *	Increments the reference count of the new bead.
 *
 *-----------------------------------------------------------------------------
 */

BDD_BeadIndex
BDD_MakeBead(
     BDD_System* sysPtr,	/* Pointer to the BDD system in play */
     BDD_VariableIndex level, 	/* Variable index of the bead */
     BDD_BeadIndex low, 		/* Low successor */
     BDD_BeadIndex high)		/* High successor */
{
    BDD_BeadIndex bead;		/* Index of the bead */

    /* 
     * If both successors are the same bead, this bead is redundant and
     * must not appear in any diagram. Otherwise, look for the bead in the
     * hash table. If it's found, use the hashed copy. If it's not found,
     * make a new bead and put it in the hash table.
     */
    if (low == high) {
	bead = low;
    } else if ((bead = FindInHash(sysPtr, level, low, high)) == 0) {
	bead = AddBead(sysPtr, level, low, high);
	AddToHash(sysPtr, bead);
    }

    /* Advance the bead's reference count */
    ++(sysPtr->beads[bead].refCount);

    return bead;
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
		     BDD_BeadIndex bead)
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
    BDD_BeadIndex bead)		/* Index of the bead being unreferened */
{
    Bead* beads = sysPtr->beads;
				/* Bead table */

    if (bead <= 1) return;

    if (--(beads[bead].refCount) == 0) {

	/* Remove the bead from the hash table */
	RemoveFromHash(sysPtr, bead);
	
	/* Mark the bead as freed. */
	beads[bead].level = ~(BDD_VariableIndex) 0;
	
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
 *-----------------------------------------------------------------------------
 *
 * BDD_NthVariable --
 *
 *	Makes a BDD that simply tests a single variable
 *
 * Results:
 *	Returns the BDD.
 *
 * Side effects:
 *	Ref count is 1 plus any extant references; caller should decrement
 *	the ref count when done with the BDD.
 *
 *-----------------------------------------------------------------------------
 */

BDD_BeadIndex
BDD_NthVariable(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_VariableIndex n)	/* Index of the variable */
{
    return BDD_MakeBead(sysPtr, n, 0, 1);
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_NotNthVariable --
 *
 *	Makes a BDD that simply tests the negation of a single variable
 *
 * Results:
 *	Returns the BDD.
 *
 * Side effects:
 *	Ref count is 1 plus any extant references; caller should decrement
 *	the ref count when done with the BDD.
 *
 *-----------------------------------------------------------------------------
 */

BDD_BeadIndex
BDD_NotNthVariable(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_VariableIndex n)	/* Index of the variable */
{
    return BDD_MakeBead(sysPtr, n, 1, 0);
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_Apply --
 *
 *	Applies a Boolean operator between two BDD's.
 *
 * Results:
 *	Returns the resulting BDD.
 *
 * Side effects:
 *	Resulting BDD has a ref count of one plus any previously extant
 *	references. Caller should decrement the ref count when done.
 *
 *-----------------------------------------------------------------------------
 */

static BDD_BeadIndex
Apply(
    BDD_System* sysPtr,		/* System of BDD's */
    Tcl_HashTable* G,		/* Hash table of partial results */
    BDD_BinOp op,		/* Operation to apply */
    BDD_BeadIndex u1,		/* Left operand */
    BDD_BeadIndex u2) 		/* Right operand */
{
    Bead* beads = sysPtr->beads; /* Bead table */
    BDD_BeadIndex u[2];		 /* Bead indices for left- and right-hand 
				  * sides */
    Bead* u1Ptr = beads + u1;	/* Pointer to the left-hand bead */
    Bead* u2Ptr = beads + u2;	/* Pointer to the right-hand bead */
    BDD_VariableIndex level;	/* Level of the result */
    BDD_BeadIndex low1, high1;	/* Low and high transitions of the 
				 * left-hand bead */
    BDD_BeadIndex low2, high2;	/* Low and high transitions of the
				 * right-hand bead*/
    int newFlag;		/* Flag==1 if the output is a new bead */
    BDD_BeadIndex result;	/* Return value */
    Tcl_HashEntry* entry;	/* Pointer to the entry in the
				 * cache of beads for this operation */

    u[0] = u1;
    u[1] = u2;
    entry = Tcl_CreateHashEntry(G, u, &newFlag);
    if (!newFlag) {
	result = (BDD_BeadIndex) Tcl_GetHashValue(entry);
	++sysPtr->beads[result].refCount;
    } else if (u1 < 2 && u2 < 2) {
	unsigned int i = ((unsigned int)u1 << 1) + (unsigned int)u2;
	result = ((op>>i) & 1);
	++sysPtr->beads[result].refCount;
    } else if (u1Ptr->level == u2Ptr->level) {
	level = u1Ptr->level;
	low1 = u1Ptr->low;
	high1 = u1Ptr->high;
	low2 = u2Ptr->low;
	high2 = u2Ptr->high;
	result = BDD_MakeBead(sysPtr,
			      level,
			      Apply(sysPtr, G, op, low1, low2),
			      Apply(sysPtr, G, op, high1, high2));
    } else if (u1Ptr->level < u2Ptr->level) {
	level = u1Ptr->level;
	low1 = u1Ptr->low;
	high1 = u1Ptr->high;
	result = BDD_MakeBead(sysPtr,
			      level,
			      Apply(sysPtr, G, op, low1, u2),
			      Apply(sysPtr, G, op, high1, u2));
    } else /* u1Ptr->level > u2Ptr->level */ {
	level = u2Ptr->level;
	low2 = u2Ptr->low;
	high2 = u2Ptr->high;
	result = BDD_MakeBead(sysPtr,
			      level,
			      Apply(sysPtr, G, op, u1, low2),
			      Apply(sysPtr, G, op, u1, high2));
    }
    Tcl_SetHashValue(entry, result);
    return result;
}

BDD_BeadIndex
BDD_Apply(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BinOp op,		/* Operation to apply */
    BDD_BeadIndex u1,		/* Left operand */
    BDD_BeadIndex u2) 		/* Right operand */
{
    Tcl_HashTable G;
    Tcl_InitCustomHashTable(&G, TCL_CUSTOM_TYPE_KEYS, &Bead2KeyType);
    BDD_BeadIndex entry = Apply(sysPtr, &G, op, u1, u2);
    Tcl_DeleteHashTable(&G);
    return entry;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_SatCount --
 *
 *	Counts the number of variable assignments that satisfy the expression
 *	represented by a BDD.
 *
 * Results:
 *	Returns MP_OKAY if the assignments could be counted, or MP_MEM
 *	or MP_VAL if an error occurred in libtommath,
 *
 * Notes:
 *	Caller is responsible for initializing 'count' prior to the call
 *	and disposing of it afterward.
 *
 *-----------------------------------------------------------------------------
 */

static int
SatCount(
    BDD_System* sysPtr,		/* System of BDD's */
    Tcl_HashTable* hashPtr,	/* Hash table for memoized results */
    BDD_BeadIndex x,		/* BDD to enumerate */
    mp_int* count)		/* Count of satisfying variable assignments */
{
    Bead* xPtr;			/* Pointer to the bead being counted */
    Bead* lowPtr;		/* Pointer to the 0 successor */
    Bead* highPtr;		/* Pointer to the 1 successor */
    mp_int lresult;		/* Count of satisfying assignments of the
				 * 0 successor */
    mp_int hresult;		/* Count of satisfying assignments of the
				 * 1 successor */
    int status;			/* Status return */
    int new;			/* Flag for whether a new hash entry
				 * was created */

    /* Is the expression constant? */
    if (x < 2) {
	return mp_set_int(count, x);
    }

    /* Is the result cached? */
    Tcl_HashEntry* entryPtr = Tcl_CreateHashEntry(hashPtr, (void*) x, &new);
    if (new) {
	mp_int* cachedResult = Tcl_GetHashValue(entryPtr);
	return mp_copy(cachedResult, count);
    }

    mp_init_multi(&lresult, &hresult, NULL);
    xPtr = sysPtr->beads + x;
    lowPtr = sysPtr->beads + xPtr->low;
    highPtr = sysPtr->beads + xPtr->high;
    if ((status = SatCount(sysPtr, hashPtr, xPtr->low,
			   &lresult)) != MP_OKAY
	|| (status = SatCount(sysPtr, hashPtr, xPtr->high,
			      &hresult)) != MP_OKAY
	|| (status = mp_mul_2d(&lresult, lowPtr->level-xPtr->level-1,
			       &lresult)) != MP_OKAY
	|| (status = mp_mul_2d(&hresult, highPtr->level-xPtr->level-1,
			       &hresult)) != MP_OKAY
	|| (status = mp_add(&lresult, &hresult, count)) != MP_OKAY) {
	Tcl_DeleteHashEntry(entryPtr);
    } else {
	mp_int* savedCount = ckalloc(sizeof(mp_int));
	if ((status = mp_init_copy(savedCount, count)) == MP_OKAY) {
	    Tcl_SetHashValue(entryPtr, (void*) savedCount);
	} else {
	    Tcl_DeleteHashEntry(entryPtr);
	}
    }
    mp_clear_multi(&lresult, &hresult, NULL);
    return status;
}
int
BDD_SatCount(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BeadIndex x,		/* BDD to enumerate */
    mp_int* count)		/* Count of satisfying variable assignments */
{
    Tcl_HashTable hash;		/* Hash table of cached results */
    Tcl_HashEntry* cleanup;	/* Hash entry used in cleanup loop */
    Tcl_HashSearch search;	/* State of a hash table iteration */
    mp_int* v;			/* Dead value in the cache */
    int status;			/* Status return from tommath */
    Tcl_InitHashTable(&hash, TCL_ONE_WORD_KEYS);
    status = SatCount(sysPtr, &hash, x, count);
    for (cleanup = Tcl_FirstHashEntry(&hash, &search);
	 cleanup != NULL;
	 cleanup = Tcl_NextHashEntry(&search)) {
	v = (mp_int*) Tcl_GetHashValue(cleanup);
	mp_clear(v);
	ckfree(v);
	Tcl_SetHashValue(cleanup, NULL);
    }
    Tcl_DeleteHashTable(&hash);
    if (status == MP_OKAY) {
	status = mp_mul_2d(count, sysPtr->beads[x].level, count);
    }
    return status;
}

/* FIXME */
static int
Dump(
    Tcl_HashTable* hash,
    Tcl_Interp* interp,
    Tcl_Obj* output,
    BDD_System* sysPtr,
    BDD_BeadIndex beadIndex)
{
    int newFlag;
    Tcl_CreateHashEntry(hash, (void*) beadIndex, &newFlag);
    if (newFlag) {
	Tcl_Obj* content = Tcl_NewObj();
	Bead* beadPtr = sysPtr->beads + beadIndex;
	BDD_VariableIndex level = beadPtr->level;
	BDD_BeadIndex low = beadPtr->low;
	BDD_BeadIndex high = beadPtr->high;
	if (Tcl_ListObjAppendElement(interp, content,
				     Tcl_NewWideIntObj((Tcl_WideInt)level))
	    != TCL_OK
	    || Tcl_ListObjAppendElement(interp, content,
					Tcl_NewWideIntObj((Tcl_WideInt)low))
	    != TCL_OK
	    || Tcl_ListObjAppendElement(interp, content,
					Tcl_NewWideIntObj((Tcl_WideInt)high))
	    != TCL_OK
	    || Tcl_DictObjPut(interp, output,
			      Tcl_NewWideIntObj((Tcl_WideInt)beadIndex),
			      content) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (beadIndex > 1) {
	    if (Dump(hash, interp, output, sysPtr, low) != TCL_OK
		|| Dump(hash, interp, output, sysPtr, high) != TCL_OK) {
		return TCL_ERROR;
	    }
	}
    }
    return TCL_OK;
}
int
BDD_Dump(
    Tcl_Interp* interp,
    Tcl_Obj* output,
    BDD_System* sysPtr,
    BDD_BeadIndex beadIndex)
{
    Tcl_HashTable hashTable;
    int result;
    Tcl_InitHashTable(&hashTable, TCL_ONE_WORD_KEYS);
    result = Dump(&hashTable, interp, output, sysPtr, beadIndex);
    Tcl_DeleteHashTable(&hashTable);
    return result;
}
/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * End:
 */
