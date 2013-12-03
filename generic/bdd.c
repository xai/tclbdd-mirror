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

    /* Initialize the constant beads. */

    sysPtr->beads[0].level = 0;
    sysPtr->beads[0].refCount = 2;
    sysPtr->beads[1].level = 0;
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
    return sysPtr->beads[0].level;
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
    if (level >= sysPtr->beads[0].level) {
	sysPtr->beads[0].level = level+1;
	sysPtr->beads[1].level = level+1;
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
 * BDD_Literal --
 *
 *	Tests if a BDD represents a single (possibly negated) literal
 *	and returns informatin about the literal if so.
 *
 * Results:
 *	Returns 1 if the BDD represents a literal and 0 if it does not.
 *
 * Side effects:
 *	If assignPtr is not NULL, sets *assignPtr to the variable index
 *	and value of the literal. A value of 1 is not negated; a value
 *	of 0 is negated.
 *
 *-----------------------------------------------------------------------------
 */

int
BDD_Literal(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BeadIndex expr,		/* Expression to test */
    BDD_ValueAssignment* assignPtr)
				/* Description of the literal */
{
    /*
     * Constants are not literals
     */
    if (expr <= 1) {
	return 0;
    }

    /*
     * Literals have both next stats constant
     */
    Bead* beadPtr = sysPtr->beads + expr;
    if (beadPtr->high > 1 || beadPtr->low > 1) {
	return 0;
    }

    if (assignPtr) {
	assignPtr->var = beadPtr->level;
	assignPtr->value = beadPtr->high;
    }
    return 1;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_Negate --
 *
 *	Computes the negation of a BDD.
 *
 * Results:
 *	Returns a BDD which is true where the given BDD is false, and
 *	vice versa.
 *
 *-----------------------------------------------------------------------------
 */

static BDD_BeadIndex
Negate(
    Tcl_HashTable* H,		/* Negations already computed */
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BeadIndex u)		/* BDD to negate */
{
    Tcl_HashEntry* entryPtr;	/* Hash entry for precomputed negation */
    int newFlag;		/* Flag == 1 if no precomputed negation found */
    Bead* uPtr = sysPtr->beads+u;
				/* Pointer to the bead data */
    BDD_BeadIndex l, h;		/* Low and high transitions of the result */
    BDD_BeadIndex result;	/* Resulting BDD */

    /*
     * Handle constants
     */

    entryPtr = Tcl_CreateHashEntry(H, u, &newFlag);
    if (!newFlag) {
	result = (BDD_BeadIndex) Tcl_GetHashValue(entryPtr);
	++sysPtr->beads[result].refCount;
    } else if (u <= 1) {
	result = !u;
	++sysPtr->beads[result].refCount;
    } else {
	l = Negate(H, sysPtr, uPtr->low);
	h = Negate(H, sysPtr, uPtr->high);
	result = BDD_MakeBead(sysPtr, uPtr->level, l, h);
	BDD_UnrefBead(sysPtr, l);
	BDD_UnrefBead(sysPtr, h);
    }
    Tcl_SetHashValue(entryPtr, (ClientData) result);
    return result;
}
BDD_BeadIndex
BDD_Negate(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BeadIndex u)		/* BDD to negate */
{
    Tcl_HashTable H;		/* Hash table of precomputed results */
    BDD_BeadIndex result;	/* Negation of the given BDD */
    Tcl_InitHashTable(&H, TCL_ONE_WORD_KEYS);
    result = Negate(&H, sysPtr, u);
    Tcl_DeleteHashTable(&H);
    return result;
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

    BDD_BeadIndex l, h;	        /* Low and high transitions of the result */
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
    } else if (u1 <= 1 && u2 <= 1) {
	unsigned int i = ((unsigned int)u1 << 1) + (unsigned int)u2;
	result = ((op>>i) & 1);
	++sysPtr->beads[result].refCount;
    } else {
	if (u1Ptr->level == u2Ptr->level) {
	    level = u1Ptr->level;
	    low1 = u1Ptr->low;
	    high1 = u1Ptr->high;
	    low2 = u2Ptr->low;
	    high2 = u2Ptr->high;
	    l = Apply(sysPtr, G, op, low1, low2);
	    h = Apply(sysPtr, G, op, high1, high2);
	} else if (u1Ptr->level < u2Ptr->level) {
	    level = u1Ptr->level;
	    low1 = u1Ptr->low;
	    high1 = u1Ptr->high;
	    l = Apply(sysPtr, G, op, low1, u2);
	    h = Apply(sysPtr, G, op, high1, u2);
	} else /* u1Ptr->level > u2Ptr->level */ {
	    level = u2Ptr->level;
	    low2 = u2Ptr->low;
	    high2 = u2Ptr->high;
	    l = Apply(sysPtr, G, op, u1, low2);
	    h = Apply(sysPtr, G, op, u1, high2);
	}
	result = BDD_MakeBead(sysPtr, level, l, h);
	BDD_UnrefBead(sysPtr, l);
	BDD_UnrefBead(sysPtr, h);
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
 * BDD_Restrict --
 *
 *	Simplifies a BDD by restricting the values of certain variables
 *	to 0 or 1.
 *
 * Results:
 *	Returns the restricted BDD. Refcount must be decremented by
 *	the caller when the caller is done with the BDD.
 *
 * Notes:
 *	The restrictions must be listed in ascending order by variable
 *	number.
 *
 *-----------------------------------------------------------------------------
 */
static BDD_BeadIndex
Restrict(
    Tcl_HashTable* H,		/* Hash table of precomputed values */
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BeadIndex u,		/* BDD to restrict */
    const BDD_ValueAssignment r[], 
				/* Restrictions to apply */
    BDD_VariableIndex n)	/* Number of restrictions */
{
    int newFlag;		/* Flag = 1 if we have not seen u yet */
    Tcl_HashEntry* entryPtr;	/* Hash entry for u in precomputed values */
    BDD_BeadIndex l, h;		/* low and high branches of the resulting BDD */
    BDD_BeadIndex result;	/* Returned BDD */
    
    /* Handle tautologies and empty restriction sets */

    if (n == 0 || u <= 1) {
	++sysPtr->beads[u].refCount;
	return u;
    }

    /* Has this value been computed already? */

    entryPtr = Tcl_CreateHashEntry(H, u, &newFlag);
    if (newFlag) {
	Tcl_SetHashValue(entryPtr, 0);
    }
    BDD_BeadIndex cached = (BDD_BeadIndex) Tcl_GetHashValue(entryPtr);
    if (cached != 0) {
	++sysPtr->beads[cached].refCount;
	return cached;
    }

    /*
     * Value is not cached. Handle the first variable of the restriction
     */
    BDD_BeadIndex rvar = r[0].var;
    BDD_BeadIndex uvar = sysPtr->beads[u].level;
    if (rvar < uvar) {
	/*
	 * r[0] is an irrelevant variable in u
	 */
	result = Restrict(H, sysPtr, u, r+1, n-1);
    } else if (rvar == uvar) {
	/*
	 * r[0] appears in u. Bind it.
	 */
	if (r[0].value) {
	    result = Restrict(H, sysPtr, sysPtr->beads[u].high, r+1, n-1);
	} else {
	    result = Restrict(H, sysPtr, sysPtr->beads[u].low, r+1, n-1);
	}
    } else /* rvar > uvar */ {
	/*
	 * u's first variable is unrestricted. Apply the restriction to both
	 * successors of u, and make a bead for the restricted expression.
	 */
	l = Restrict(H, sysPtr, sysPtr->beads[u].low, r, n);
	h = Restrict(H, sysPtr, sysPtr->beads[u].high, r, n);
	result = BDD_MakeBead(sysPtr, uvar, l, h);
	BDD_UnrefBead(sysPtr, l);
	BDD_UnrefBead(sysPtr, h);
    }

    /*
     * Cache the result
     */
    Tcl_SetHashValue(entryPtr, (ClientData) result);
    return result;
}
BDD_BeadIndex
BDD_Restrict(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BeadIndex u,		/* BDD to restrict */
    const BDD_ValueAssignment r[],
				/* Restrictions to apply */
    BDD_VariableIndex n)	/* Count of restrictions */
{
    Tcl_HashTable H;		/* Cache of partial results */
    BDD_BeadIndex result;	/* Bead index of the result */
    Tcl_InitHashTable(&H, TCL_ONE_WORD_KEYS);
    result = Restrict(&H, sysPtr, u, r, n);
    Tcl_DeleteHashTable(&H);
    return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_Quantify --
 *
 *	Apply an existential, unique, or universal quantifier to a BDD.
 *
 * Results:
 *	Returns a BDD representing the quantified expression.
 *
 *-----------------------------------------------------------------------------
 */

static BDD_BeadIndex
Quantify(
    Tcl_HashTable* H,		/* Hash table of cached partial results */
    BDD_System* sysPtr,		/* Pointer to the system of BDD's */
    BDD_Quantifier q,		/* Quantifier to apply */
    const BDD_VariableIndex* v,	/* Variables to quantify */
    BDD_VariableIndex n,	/* Number of quantified variables */
    BDD_BeadIndex u)		/* Expression to quantify */
{
    BDD_BeadIndex l;		/* Low transition of the result */
    BDD_BeadIndex h;		/* High transition of the result */
    BDD_BeadIndex r;		/* Return value */
    int newFlag;		/* Flag == 1 iff the result was not cached */
    Tcl_HashEntry* entryPtr;	/* Pointer to the hash entry for
				 * a cached result */

    /* Check for a cached result */
    entryPtr = Tcl_CreateHashEntry(H, (ClientData) u, &newFlag);
    if (!newFlag) {
	r = (BDD_BeadIndex) Tcl_GetHashValue(entryPtr);
	++sysPtr->beads[r].refCount;
	return r;
    }

    for (;;) {
	Bead* beadPtr = sysPtr->beads + u;
	if (v == 0) {
	    /*
	     * No variables remain to quantify. Simply return the expression
	     * itself.
	     */
	    ++beadPtr->refCount;
	    r = u;
	    break;
	} else if (beadPtr->level < *v) {
	    /*
	     * The current variable in the expression is unquantified.
	     * Quantify the two subexpressions and make the result
	     */
	    l = Quantify(H, sysPtr, q, v, n, beadPtr->low);
	    h = Quantify(H, sysPtr, q, v, n, beadPtr->high);
	    r = BDD_MakeBead(sysPtr, beadPtr->level, l, h);
	    BDD_UnrefBead(sysPtr,h);
	    BDD_UnrefBead(sysPtr,l);
	    break;
	} else if (beadPtr->level == *v) {
	    /*
	     * The current variable in the expression is quantified.
	     * Quantify the two subexpressions with respect to the
	     * remaining variables and then apply the combining operation.
	     *
	     * Would it be advantageous to cache more aggressively here,
	     * rather than creating/destroying the hash table in BDD_Apply?
	     */
	    l = Quantify(H, sysPtr, q, v+1, n-1, beadPtr->low);
	    h = Quantify(H, sysPtr, q, v+1, n-1, beadPtr->high);
	    r = BDD_Apply(sysPtr, q, l, h);
	    BDD_UnrefBead(sysPtr,h);
	    BDD_UnrefBead(sysPtr,l);
	    break;
	} else if (q == BDD_QUANT_UNIQUE) {
	    /* 
	     * There is a quantified variable that does not appear free in
	     * the expression.
	     *
	     * If the quantifier is UNIQUE, then either value of the variable
	     * will satisfy the expression equally, and no unique solution
	     * is possible.
	     */
	    r = 0;
	    ++sysPtr->beads[r].refCount;
	    break;
	} else {
	    /* 
	     * The current variable does not appear free in the expression,
	     * and the quantifier is EXISTS or FORALL. The quantification is
	     * trivially satisfied with respect to the variable in question.
	     * Advance to the next variable.
	     */
	    ++v;
	    --n;
	}
    }
    
    /*
     * Cache and return the result
     * It is possible for an outer quantification to destroy the
     * result altogether, so make sure that the refcount tracks the
     * cache entry.
     */
    ++sysPtr->beads[r].refCount;
    Tcl_SetHashValue(entryPtr, (ClientData) r);
    return r;
}
BDD_BeadIndex
BDD_Quantify(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_Quantifier q,		/* Quantifier to apply */
    const BDD_VariableIndex* v,	/* List of variables to quantify */
    BDD_VariableIndex n,	/* Number of variables to quantify */
    BDD_BeadIndex e)		/* Expression to quantify */
{
    Tcl_HashTable H;		/* Hash table to cache partial results */
    Tcl_HashSearch search;	/* Search state for clearing the hash */
    Tcl_HashEntry* entryPtr;	/* Hash table entry to be cleared */
    BDD_BeadIndex c;		/* Cached result */
    Tcl_InitHashTable(&H, TCL_ONE_WORD_KEYS);
    BDD_BeadIndex r = Quantify(&H, sysPtr, q, v, n, e);
    for (entryPtr = Tcl_FirstHashEntry(&H, &search);
	 entryPtr != NULL;
	 entryPtr = Tcl_NextHashEntry(&search)) {
	c = (BDD_BeadIndex) Tcl_GetHashValue(entryPtr);
	BDD_UnrefBead(sysPtr, c);
    }
    Tcl_DeleteHashTable(&H);
    return r;
	 
    
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
    if (x <= 1) {
	return mp_set_int(count, x);
    }

    /* Is the result cached? */
    Tcl_HashEntry* entryPtr = Tcl_CreateHashEntry(hashPtr, (void*) x, &new);
    if (!new) {
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
    if (status == MP_OKAY) {
	status = mp_mul_2d(count, sysPtr->beads[x].level, count);
    }
    for (cleanup = Tcl_FirstHashEntry(&hash, &search);
	 cleanup != NULL;
	 cleanup = Tcl_NextHashEntry(&search)) {
	v = (mp_int*) Tcl_GetHashValue(cleanup);
	mp_clear(v);
	ckfree(v);
	Tcl_SetHashValue(cleanup, NULL);
    }
    Tcl_DeleteHashTable(&hash);
    return status;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_AllSatStart --
 *
 *	Begins a traversal of a BDD for all satisfying variable assignments
 *
 * Results:
 *	Returns a pointer to a state vector that manages the state
 *	of the traversal. The state vector must be passed to BDD_AllSatNext
 *	to retrieve each variable assignment, and to BDD_AllSatFinish
 *	at the end of the traversal or when the traversal is abandoned.
 *
 * It is a pointer smash to dispose of a BDD_System while traversing one of
 * its BDD's.
 *
 *-----------------------------------------------------------------------------
 */
BDD_AllSatState*
BDD_AllSatStart(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BeadIndex u)		/* BDD to traverse */
{

    /*
     * Allocate the state vector
     */
    BDD_AllSatState* stateVector =
	(BDD_AllSatState*) ckalloc(sizeof(BDD_AllSatState));
    BDD_VariableIndex nVars = BDD_GetVariableCount(sysPtr);

    /*
     * Allocate the stacks
     */
    stateVector->sysPtr = sysPtr;
    stateVector->uStack = (BDD_BeadIndex*)
	ckalloc(nVars * sizeof(BDD_BeadIndex));
    stateVector->sStack = (unsigned char*)
	ckalloc(nVars);
    stateVector->v = (BDD_ValueAssignment*)
	ckalloc(nVars * sizeof(BDD_ValueAssignment));

    /*
     * Store the initial state, in which the top of the expression
     * is on the stack and the stack depth is 1, with the first
     * action being to explore the expression's left hand side.
     */

    stateVector->uStack[0] = u;
    ++sysPtr->beads[u].refCount;
    stateVector->sStack[0] = BDD_ALLSAT_START;
    stateVector->depth = 1;

    return stateVector;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_AllSatNext --
 *
 *	Retrieves the next satisfying assignment from an iterator over
 *	a BDD
 *
 * Results:
 *	Returns 1 if there was a satisfying assignment, 0 at the end of
 *	the iterator.
 *
 * Side effects:
 *	Stores the literals of the satisfying assignment in *v and 
 *	the count of the literals in *n.
 *
 * Irrelevant variables are omitted from the vector. The vector is
 * usable until the next call to BDD_AllSat_Next, or a call to BDD_AllSat_Finish
 *
 * It is necessary to call BDD_AllSat_Finish to dispose of the state
 * vector, even after this function has returned 0.
 *
 *-----------------------------------------------------------------------------
 */

int
BDD_AllSatNext(
    BDD_AllSatState* stateVector,
				/* State vector from BDD_AllSat_Start */
    BDD_ValueAssignment **vPtr, /* OUTPUT: Vector of satisfying literals */
    BDD_VariableIndex *nPtr)	/* OUTPUT: Count of satisfying literals */
{
    BDD_System* sysPtr = stateVector->sysPtr;
				/* System of BDD's */
    BDD_VariableIndex depth = stateVector->depth;
				/* Depth of the stack */
    BDD_BeadIndex u;		/* Current bead */
    unsigned char s;		/* Current state */
	
    for (;;) {

	/*
	 * Initially, BDD_AllSat_Start will have pushed the starting
	 * bead and the BDD_ALLSAT_START state onto the stack, and this
	 * code will pop it again. After reporting a variable assignment
	 * and re-entering this procedure, the top of stack will be
	 * the last bead prior to the 1, and the BDD_ALLSAT_SECONDTRY
	 * or BDD_ALLSAT_RETURN according to whether the next action
	 * is to explore the bead's 'high' transition or to retreat to
	 * the bead's predecessor.
	 *
	 * We also arrive at this point from the bottom of the loop,
	 * in which case u designates the 0 leaf bead, and we want
	 * to unwind the stack in the same way.
	 */

	/*
	 * Pop the state off the stack
	 */
	do {
	    if (depth == 0) {
		/*
		 * When the entire stack is unwound, the BDD has been fully
		 * traversed.
		 */
		return 0;
	    }
	    --depth;
	    u = stateVector->uStack[depth];
	    s = stateVector->sStack[depth];

	    /*
	     * As long as the popped state is RETURN, keep discarding stack
	     * levels.
	     */
	} while (s == BDD_ALLSAT_RETURN);

	/*
	 * At this point, the state is either START, to begin traversing
	 * the diagram, or SECONDTRY, to explore the 'high' transition of
	 * a visited bead. If it's SECONDTRY, advance to the bead's 'high'
	 * transition.
	 */
	if (s == BDD_ALLSAT_SECONDTRY) {
	    stateVector->uStack[depth] = u;
	    stateVector->sStack[depth] = BDD_ALLSAT_RETURN;
	    stateVector->v[depth].var = sysPtr->beads[u].level;
	    stateVector->v[depth].value = 1;
	    ++depth;
	    u = sysPtr->beads[u].high;
	}

	/*
	 * Traverse the 'low' transitions on the current branch
	 * until reaching a terminal, stacking SECONDTRY along the way.
	 */
	while (u != 0) {
	    if (u == 1) {
		/*
		 * We've reached the 1 terminal. Return the satisfying
		 * assignment that we just found. We'll resume by unstacking
		 */
		stateVector->depth = depth;
		*vPtr = stateVector->v;
		*nPtr = depth;
		return 1;
	    }

	    /*
	     * Stack the current bead to restart from the 'high' transition,
	     * then advance to the 'low' transition.
	     */
	    stateVector->uStack[depth] = u;
	    stateVector->sStack[depth] = BDD_ALLSAT_SECONDTRY;
	    stateVector->v[depth].var = sysPtr->beads[u].level;
	    stateVector->v[depth].value = 0;
	    ++depth;
	    u = sysPtr->beads[u].low;
	}

	/* 
	 * We've reached the zero terminal. Return to the top of this
	 * function to unwind the stack to the next decision point.
	 */
    }
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_AllSatFinish --
 *
 *	Terminates an exhaustive search for satisfying variable assigments
 *	in a BDD.
 *
 * Side effects:
 *	Frees the state vector and decrements the ref count of the start bead.
 *
 *-----------------------------------------------------------------------------
 */

void
BDD_AllSatFinish(
    BDD_AllSatState* stateVector) /* State vector for tbe search */
{
    ckfree(stateVector->v);
    ckfree(stateVector->sStack);
    BDD_UnrefBead(stateVector->sysPtr, stateVector->uStack[0]);
    ckfree(stateVector->uStack);
    ckfree(stateVector);
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_Dump --
 *
 *	Formats a BDD as a Tcl dictionary for debugging.
 *
 * Results:
 *	Returns a standard Tcl result.
 *
 * Side effects:
 *	The output object receives a dictionary whose keys are state numbers
 *	and whose values are triples consisting of {level, nextStateIfFalse,
 *	nextStateIfTrue}. The dictionary is in depth-first order starting
 *	from the initial state.
 *
 *-----------------------------------------------------------------------------
 */

static int
Dump(
    Tcl_HashTable* hash,	/* Hash table of visited states */
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_Obj* output,		/* Output object */
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BeadIndex beadIndex)	/* Index of the bead being dumped */
{
    int newFlag;		/* Has this object been seen before? */
    Tcl_CreateHashEntry(hash, (void*) beadIndex, &newFlag);

    if (newFlag) {

	/* 
	 * Make the triple {level ifFalse ifTrue} and store it in the
	 * dictionary keyed by the bead index.
	 */
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

	/*
	 * If this bead isn't a leaf, dump its successors
	 */
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
    Tcl_Interp* interp,		/* Tcl interpreter */
    Tcl_Obj* output,		/* Output object */
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BeadIndex beadIndex)	/* Index of the bead to dump */
{
    Tcl_HashTable hashTable;	/* Hash table of beads that have been seen */
    int result;			/* Tcl status return */

    /*
     * We haven't seen any beads yet.
     */
    Tcl_InitHashTable(&hashTable, TCL_ONE_WORD_KEYS);

    /*
     * Dump the BDD to the given object
     */
    Tcl_SetStringObj(output, NULL, 0);
    result = Dump(&hashTable, interp, output, sysPtr, beadIndex);

    /*
     * Clean up the hashtable
     */
    Tcl_DeleteHashTable(&hashTable);
    return result;
}
/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * End:
 */
