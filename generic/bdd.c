#define USE_TOMMATH_S

#define inline /* don't inline */

#include "bddInt.h"

#include <tcl.h>
#include <tclTomMath.h>

#include <limits.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>

#define MODULUS 0xfffffffb

#define HASHFRACTION 2

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
 * NewUIHash1MP --
 *
 *	Creates a new hash table whose keys are pairs of unsigned
 *	integers and whose values are unsigned integers
 *
 * Results:
 *	Returns a pointer to the newly-created hash table.
 *
 * Side effects:
 *	Allocates memory.
 *
 *-----------------------------------------------------------------------------
 */

static UIHash1MP*
NewUIHash1MP(
    unsigned int n)		/* Initial capacity of the table */
{
    unsigned int hashSize = 1 << HashTableSize((n + HASHFRACTION - 1)
					   / HASHFRACTION);
				/* Required size of the hash table */
    unsigned int i;

    /* Create the table's description */

    UIHash1MP* tablePtr = ckalloc(sizeof(UIHash1MP));

    /* Create the initial table entries and put them all on the freelist */
    tablePtr->entries = ckalloc(n * sizeof(UIEntry1MP));
    tablePtr->entriesAlloc = n;
    for (i = 0; i+1 < n; ++i) {
	tablePtr->entries[i].next = i+1;
    }
    tablePtr->entries[n-1].next = 0;
    tablePtr->unusedEntry = 0;

    /* Create the initial hash table with all buckets empty */
    tablePtr->hashes = ckalloc(hashSize * sizeof(unsigned int));
    tablePtr->hashSize = hashSize;
    for (i = 0; i < hashSize; ++i) {
	tablePtr->hashes[i] = ~(unsigned int) 0;
    }
    
    /* Return the table. */
    return tablePtr;
}

/*
 *-----------------------------------------------------------------------------
 *
 * GrowUIHash1MP --
 *
 *	Expands a hash table whose keys are pairs of unsigned ints and
 *	whose values are unsigned ints.
 *
 * Side effects:
 *	Number of table entries and number of hash buckets are both doubled,
 *	and the table is rehashed.
 *
 * Notes:
 *	The hash table must have no free space when it is grown.
 *
 *-----------------------------------------------------------------------------
 */

static inline void
GrowUIHash1MP(UIHash1MP* tablePtr)
{
    unsigned int oldAlloc = tablePtr->entriesAlloc;
				/* Former size of the table */
    unsigned int newAlloc = 2 * oldAlloc;
				/* New size of the table */
    unsigned int hashSize = 2 * tablePtr->hashSize;
				/* New size of the hash index */
    UIEntry1MP* entries;		/* New allocation of the table */
    unsigned int i;

    /* Allocate more entries */
    entries = (UIEntry1MP*) ckrealloc((char*)(tablePtr->entries),
				    newAlloc * sizeof(UIEntry1MP));

    /* Make a new freelist */
    for (i = oldAlloc + 1; i < newAlloc; ++i) {
	entries[i-1].next = i;
    }
    entries[newAlloc-1].next = oldAlloc;
    tablePtr->entries = entries;
    tablePtr->entriesAlloc = newAlloc;
    tablePtr->unusedEntry = newAlloc-1;

    /* Destroy the old hash table */
    ckfree(tablePtr->hashes);
    
    /* Make a new hash table */
    tablePtr->hashes = (unsigned int*) ckalloc(hashSize * sizeof(unsigned int));
    tablePtr->hashSize = hashSize;
    for (i = 0; i < hashSize; ++i) {
	tablePtr->hashes[i] = ~(unsigned int) 0;
    }

    /* Rehash the entries and add them. */
    for (i = 0; i < oldAlloc; ++i) {
	UIEntry1MP* entryPtr = entries + i;
	unsigned int bucket = entryPtr->key & (hashSize-1);
	if (tablePtr->hashes[bucket] == ~(unsigned int) 0) {
	    entryPtr->next = i;
	    tablePtr->hashes[bucket] = i;
	} else {
	    UIEntry1MP* lastPtr = entries + tablePtr->hashes[bucket];
	    entryPtr->next = lastPtr->next;
	    lastPtr->next = i;
	}
    }
}

/*
 *-----------------------------------------------------------------------------
 *
 * AddToUIHash1MP --
 *
 *	Adds an entry to a UIHash1MP hash table, where the key is a pair
 *	of unsigned integers and the value is a single unsigned integer
 *
 * Results:
 *	Returns MP_OKAY if successful, another value to report an error
 *	in libtommath.
 *
 * Side effects:
 *	Triple is added to the hash table.
 *
 *-----------------------------------------------------------------------------
 */

static int
AddToUIHash1MP(
    UIHash1MP* tablePtr,	/* Pointer to the hashtable */
    unsigned int key,		/* First key */
    mp_int* value)		/* Value */
{
    unsigned int entry;		/* Index of the entry being added */
    unsigned int bucket;	/* Hash bucket of the entry being added */
    UIEntry1MP* entryPtr;	/* Pointer to the entry being added */
    int status;			/* Status return from libtommath */
 
    /* If there is no free space, grow the hashtable */
    if (tablePtr->unusedEntry == ~(unsigned int)0) {
	GrowUIHash1MP(tablePtr);
    }

    /* Find out what hash bucket to use */
    bucket = key & (tablePtr->hashSize - 1);

    /* Allocate an entry from the free list and stash the value in it */
    entry = tablePtr->entries[tablePtr->unusedEntry].next;
    entryPtr = tablePtr->entries + entry;
    entryPtr->key = key;
    if ((status = mp_init_copy(&entryPtr->value, value)) != MP_OKAY) {
	return status;
    }
    if (tablePtr->unusedEntry == entry) {
	tablePtr->unusedEntry = ~(unsigned int) 0;
    } else {
	tablePtr->entries[tablePtr->unusedEntry].next
	    = tablePtr->entries[entry].next;
    }
    
    /* Add the entry to its bucket */
    if (tablePtr->hashes[bucket] == ~(unsigned int) 0) {
	entryPtr->next = entry;
	tablePtr->hashes[bucket] = entry;
    } else {
	UIEntry1MP* lastPtr = tablePtr->entries + tablePtr->hashes[bucket];
	entryPtr->next = lastPtr->next;
	lastPtr->next = entry;
    }
    return MP_OKAY;
}

/*
 *-----------------------------------------------------------------------------
 *
 * FindInUIHash1MP --
 *
 *	Finds an entry in a hash table whose keys are pairs of unsigned ints
 *	and whose values are unsigned ints.
 *
 * Results:
 *	Returns a pointer to the value associated with the given key, or
 *	NULL if there is no such key
 *
 *-----------------------------------------------------------------------------
 */

static mp_int*
FindInUIHash1MP(
    UIHash1MP* tablePtr,	/* Pointer to the hash table */
    unsigned int key)		/* Key */
{
    unsigned int first;		/* Index of first entry in bucket */
    unsigned int entry;		/* Index of current entry */
    unsigned int bucket;	/* What bucket is the value in? */

    /* Find out what hash bucket to use */
    bucket = key & (tablePtr->hashSize - 1);

    first = tablePtr->hashes[bucket];
    entry = first;
    if (entry != ~(unsigned int) 0) {
	do {
	    UIEntry1MP* entryPtr = tablePtr->entries + entry;
	    if (entryPtr->key == key) {
		return &(entryPtr->value);
	    }
	    entry = entryPtr->next;
	} while (entry != first);
    }
    return NULL;
}

/*
 *-----------------------------------------------------------------------------
 *
 * DeleteUIHash1MP --
 *
 *	Delete a hash table whose keys are pairs of unsigned ints and whose
 *	values are unsigned ints.
 *
 * Side effects:
 *	All memory associated with the hash table is freed.
 *
 *-----------------------------------------------------------------------------
 */

static void
DeleteUIHash1MP(
    UIHash1MP* tablePtr)	/* Pointer to the hash table */
{
    unsigned int hashSize = tablePtr->hashSize;
    unsigned int bucket;	/* Bucket in the hash table */
    unsigned int entry;		/* Position of an entry in the hash table */
    unsigned int first;		/* First entry in the current bucket */
    UIEntry1MP* entryPtr;	/* Entry in the hash table */

    /* Free all the mp_ints in use in the table */

    for (bucket = 0; bucket < hashSize; ++bucket) {
	first = tablePtr->hashes[bucket];
	entry = first;
	if (entry != ~(unsigned int) 0) {
	    do {
		entryPtr = tablePtr->entries + entry;
		fprintf(stderr, "hashMP: clean up %u from bucket %u\n",
			entryPtr->key, bucket);
		mp_clear(&(entryPtr->value));
		entry = entryPtr->next;
	    } while (entry != first);
	}
    }

    /* Free the tables themselves */

    ckfree(tablePtr->hashes);
    ckfree(tablePtr->entries);
    ckfree(tablePtr);
}

/*
 *-----------------------------------------------------------------------------
 *
 * NewUIHash2 --
 *
 *	Creates a new hash table whose keys are pairs of unsigned
 *	integers and whose values are unsigned integers
 *
 * Results:
 *	Returns a pointer to the newly-created hash table.
 *
 * Side effects:
 *	Allocates memory.
 *
 *-----------------------------------------------------------------------------
 */

static UIHash2*
NewUIHash2(
    unsigned int n)		/* Initial capacity of the table */
{
    unsigned int hashSize = 1 << HashTableSize((n + HASHFRACTION - 1)
					   / HASHFRACTION);
				/* Required size of the hash table */
    unsigned int i;

    /* Create the table's description */

    UIHash2* tablePtr = ckalloc(sizeof(UIHash2));

    /* Create the initial table entries and put them all on the freelist */
    tablePtr->entries = ckalloc(n * sizeof(UIEntry2));
    tablePtr->entriesAlloc = n;
    for (i = 0; i+1 < n; ++i) {
	tablePtr->entries[i].next = i+1;
    }
    tablePtr->entries[n-1].next = 0;
    tablePtr->unusedEntry = 0;

    /* Create the initial hash table with all buckets empty */
    tablePtr->hashes = ckalloc(hashSize * sizeof(unsigned int));
    tablePtr->hashSize = hashSize;
    for (i = 0; i < hashSize; ++i) {
	tablePtr->hashes[i] = ~(unsigned int) 0;
    }
    
    /* Return the table. */
    return tablePtr;
}

/*
 *-----------------------------------------------------------------------------
 *
 * GrowUIHash2 --
 *
 *	Expands a hash table whose keys are pairs of unsigned ints and
 *	whose values are unsigned ints.
 *
 * Side effects:
 *	Number of table entries and number of hash buckets are both doubled,
 *	and the table is rehashed.
 *
 * Notes:
 *	The hash table must have no free space when it is grown.
 *
 *-----------------------------------------------------------------------------
 */

static inline void
GrowUIHash2(UIHash2* tablePtr)
{
    unsigned int oldAlloc = tablePtr->entriesAlloc;
				/* Former size of the table */
    unsigned int newAlloc = 2 * oldAlloc;
				/* New size of the table */
    unsigned int hashSize = 2 * tablePtr->hashSize;
				/* New size of the hash index */
    UIEntry2* entries;		/* New allocation of the table */
    unsigned int i;

    /* Allocate more entries */
    entries = (UIEntry2*) ckrealloc((char*)(tablePtr->entries),
				    newAlloc * sizeof(UIEntry2));

    /* Make a new freelist */
    for (i = oldAlloc + 1; i < newAlloc; ++i) {
	entries[i-1].next = i;
    }
    entries[newAlloc-1].next = oldAlloc;
    tablePtr->entries = entries;
    tablePtr->entriesAlloc = newAlloc;
    tablePtr->unusedEntry = newAlloc-1;

    /* Destroy the old hash table */
    ckfree(tablePtr->hashes);
    
    /* Make a new hash table */
    tablePtr->hashes = (unsigned int*) ckalloc(hashSize * sizeof(unsigned int));
    for (i = 0; i < hashSize; ++i) {
	tablePtr->hashes[i] = ~(unsigned int) 0;
    }
    tablePtr->hashSize = hashSize;

    /* Rehash the entries and add them. */
    for (i = 0; i < oldAlloc; ++i) {
	UIEntry2* entryPtr = entries + i;
	unsigned int bucket =
	    HashPair(entryPtr->key1, entryPtr->key2) & (hashSize-1);
	if (tablePtr->hashes[bucket] == ~(unsigned int) 0) {
	    entryPtr->next = i;
	    tablePtr->hashes[bucket] = i;
	} else {
	    UIEntry2* lastPtr = entries + tablePtr->hashes[bucket];
	    entryPtr->next = lastPtr->next;
	    lastPtr->next = i;
	}
    }
}

/*
 *-----------------------------------------------------------------------------
 *
 * AddToUIHash2 --
 *
 *	Adds an entry to a UIHash2 hash table, where the key is a pair
 *	of unsigned integers and the value is a single unsigned integer
 *
 * Side effects:
 *	Triple is added to the hash table.
 *
 *-----------------------------------------------------------------------------
 */

static void
AddToUIHash2(
    UIHash2* tablePtr,		/* Pointer to the hashtable */
    unsigned int key1,		/* First key */
    unsigned int key2,		/* Second key */
    unsigned int value)		/* Value */
{
    unsigned int entry;		/* Index of the entry */
    unsigned int bucket;	/* Hash bucket of the entry */
    UIEntry2* entryPtr;
 
    /* If there is no free space, grow the hashtable */
    if (tablePtr->unusedEntry == ~(unsigned int)0) {
	GrowUIHash2(tablePtr);
    }

    /* Find out what hash bucket to use */
    bucket = HashPair(key1, key2) & (tablePtr->hashSize - 1);

    /* Allocate an entry from the free list */
    entry = tablePtr->entries[tablePtr->unusedEntry].next;
    if (tablePtr->unusedEntry == entry) {
	tablePtr->unusedEntry = ~(unsigned int) 0;
    } else {
	tablePtr->entries[tablePtr->unusedEntry].next
	    = tablePtr->entries[entry].next;
    }
    
    /* Initialize the entry and add it to its bucket */
    entryPtr = tablePtr->entries + entry;
    entryPtr->key1 = key1;
    entryPtr->key2 = key2;
    entryPtr->value = value;
    if (tablePtr->hashes[bucket] == ~(unsigned int) 0) {
	entryPtr->next = entry;
	tablePtr->hashes[bucket] = entry;
    } else {
	UIEntry2* lastPtr = tablePtr->entries + tablePtr->hashes[bucket];
	entryPtr->next = lastPtr->next;
	lastPtr->next = entry;
    }
}

/*
 *-----------------------------------------------------------------------------
 *
 * FindInUIHash2 --
 *
 *	Finds an entry in a hash table whose keys are pairs of unsigned ints
 *	and whose values are unsigned ints.
 *
 * Results:
 *	Returns the value associated with the given key, or
 *	~(unsigned int) 0 if there is no such value.
 *
 *-----------------------------------------------------------------------------
 */

static unsigned int
FindInUIHash2(
    UIHash2* tablePtr,		/* Pointer to the hash table */
    unsigned int key1,		/* First key */
    unsigned int key2)		/* Second key */
{
    unsigned int first;		/* Index of first entry in bucket */
    unsigned int entry;		/* Index of current entry */
    unsigned int bucket;	/* What bucket is the value in? */

    /* Find out what hash bucket to use */
    bucket = HashPair(key1, key2) & (tablePtr->hashSize - 1);

    first = tablePtr->hashes[bucket];
    entry = first;
    if (entry != ~(unsigned int) 0) {
	do {
	    UIEntry2* entryPtr = tablePtr->entries + entry;
	    if (entryPtr->key1 == key1
		&& entryPtr->key2 == key2) {
		return entryPtr->value;
	    }
	    entry = entryPtr->next;
	} while (entry != first);
    }
    return ~(unsigned int) 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 * DeleteUIHash2 --
 *
 *	Delete a hash table whose keys are pairs of unsigned ints and whose
 *	values are unsigned ints.
 *
 * Side effects:
 *	All memory associated with the hash table is freed.
 *
 *-----------------------------------------------------------------------------
 */

static void
DeleteUIHash2(
    UIHash2* tablePtr)		/* Pointer to the hash table */
{
    ckfree(tablePtr->hashes);
    ckfree(tablePtr->entries);
    ckfree(tablePtr);
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
 * BDD_SetVariableCount --
 *
 *	Announces how many Boolean variables there are in a system of BDD's
 *
 * Notes:
 *	Will cause failures, silently, if any call to any other function
 *	is made specifying a variable index greater than or equal to the
 *	variable count.
 *
 *-----------------------------------------------------------------------------
 */

void
BDD_SetVariableCount(
    BDD_System* sysPtr,		/* System of BDD's */
    unsigned int n)		/* Variable count */
{
    Bead* beads = sysPtr->beads;
    beads[0].level = n;
    beads[1].level = n;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_GetVariableCount --
 *
 *	Returns the number of variables in a BDD.
 *
 * Results:
 *	The number of variables is returned.
 *
 *-----------------------------------------------------------------------------
 */

unsigned int
BDD_GetVariableCount(
    BDD_System* sysPtr)		/* System of BDD's */
{
    return sysPtr->beads[0].level;
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
    unsigned int level,		/* Level (variable index) of the desired bead */
    unsigned int low,		/* 'false' transition of the desired bead */
    unsigned int high)		/* 'true' transition of the desired bead */
{
    unsigned int bucket = HashTriple(level, low, high)
	& (sysPtr->hashSize - 1);
				/* Hash bucket holding the bead. */
    Bead* beads = sysPtr->beads;
    unsigned int p;

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
    unsigned int bead)		/* Index of the bead being added */
{
    Bead* beadPtr = sysPtr->beads+bead;
				/* Pointer to the bead being added */
    unsigned int bucket = HashBead(beadPtr) & (sysPtr->hashSize - 1);
				/* Hash bucket contaiing the bead */
    beadPtr->next = sysPtr->hashes[bucket];
    sysPtr->hashes[bucket]=bead;
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
    unsigned int oldAlloc = sysPtr->beadsAlloc;
				/* Former size of the bead table */
    unsigned int newAlloc = 2 * oldAlloc;
				/* New size of the bead table */
    Bead* beads;		/* New bead table */
    unsigned int hashSize = sysPtr->hashSize * 2;
				/* New size of the hash table */
    unsigned int i;

    /* Reallocate the beads */
    beads = (Bead*) ckrealloc((char*) (sysPtr->beads), newAlloc * sizeof(Bead));
    memset(beads+oldAlloc, 0, (newAlloc-oldAlloc) * sizeof(Bead));
    sysPtr->beads = beads;
    sysPtr->beadsAlloc = newAlloc;

    /* Link the beads together to splice them into the free list */
    for (i = oldAlloc + 1; i < newAlloc; ++i) {
	beads[i-1].level = ~(unsigned int) 0;
	beads[i-1].next = i;
    }
    beads[newAlloc-1].level = ~(unsigned int) 0;

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
    sysPtr->hashes = (unsigned int*) ckalloc(hashSize * sizeof(unsigned int));
    memset(sysPtr->hashes, 0, hashSize * sizeof(unsigned int));
    sysPtr->hashSize = hashSize;
    
    /* Add allocated beads to the hash table. */
    for (i = 2; i < oldAlloc; ++i) {
	if (beads[i].level != ~(unsigned int) 0) {
	    AddToHash(sysPtr, i);
	}
    }

    /* For now, no automatic reordering. */
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
#if 0
    Tcl_Panic("couldn't find delete bead(%d,%d,%d) in hashtable",
	      beads[bead].level, beads[bead].low, beads[bead].high);
#else
    abort();
#endif
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

static inline unsigned int
AddBead(
    BDD_System* sysPtr,		/* System of BDDs in play */
    unsigned int level,		/* Variable index of the bead to add */
    unsigned int low,		/* Low successor */
    unsigned int high)		/* High successor */
{
    unsigned int bead;		/* Index of the added bead */
    Bead* beadPtr;		/* Memory address of the added bead */
    Bead* beads;

    /* If there is no free space, grow the tables. */
    if (sysPtr->unusedBead == 0) {
	GrowSystemAndRehash(sysPtr);
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

unsigned int
BDD_MakeBead(
     BDD_System* sysPtr,		/* Pointer to the BDD system in play */
     unsigned int level, 	/* Variable index of the bead */
     unsigned int low, 		/* Low successor */
     unsigned int high)		/* High successor */
{
    unsigned int bead;		/* Index of the bead */

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

static unsigned int
Apply(
    BDD_System* sysPtr,		/* System of BDD's */
    UIHash2* G,			/* Hash table of memoized results */
    BDD_BinOp op,		/* Operation to apply */
    unsigned int u1,		/* Left operand */
    unsigned int u2) 		/* Right operand */
{
    int entry;
    Bead* beads = sysPtr->beads;
    Bead* u1Ptr = beads + u1;
    Bead* u2Ptr = beads + u2;
    unsigned int level, low1, high1, low2, high2;
    entry = FindInUIHash2(G, u1, u2);
    if (entry != ~(unsigned int)0) {
	++sysPtr->beads[entry].refCount;
    } else if (u1 < 2 && u2 < 2) {
	unsigned int i = (u1 << 1) + u2;
	entry = ((op>>i) & 1);
	++sysPtr->beads[entry].refCount;
    } else if (u1Ptr->level == u2Ptr->level) {
	level = u1Ptr->level;
	low1 = u1Ptr->low;
	high1 = u1Ptr->high;
	low2 = u2Ptr->low;
	high2 = u2Ptr->high;
	entry = BDD_MakeBead(sysPtr,
			     level,
			     Apply(sysPtr, G, op, low1, low2),
			     Apply(sysPtr, G, op, high1, high2));
    } else if (u1Ptr->level < u2Ptr->level) {
	level = u1Ptr->level;
	low1 = u1Ptr->low;
	high1 = u1Ptr->high;
	entry = BDD_MakeBead(sysPtr,
			     level,
			     Apply(sysPtr, G, op, low1, u2),
			     Apply(sysPtr, G, op, high1, u2));
    } else /* u1Ptr->level > u2Ptr->level */ {
	level = u2Ptr->level;
	low2 = u2Ptr->low;
	high2 = u2Ptr->high;
	entry = BDD_MakeBead(sysPtr,
			     level,
			     Apply(sysPtr, G, op, u1, low2),
			     Apply(sysPtr, G, op, u1, high2));
    }
    AddToUIHash2(G, u1, u2, entry);
    return entry;
}

unsigned int
BDD_Apply(
    BDD_System* sysPtr,		/* System of BDD's */
    BDD_BinOp op,		/* Operation to apply */
    unsigned int u1,		/* Left operand */
    unsigned int u2) 		/* Right operand */
{
    UIHash2* G = NewUIHash2(16);
    unsigned int entry = Apply(sysPtr, G, op, u1, u2);
    DeleteUIHash2(G);
    return entry;
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

unsigned int
BDD_NthVariable(
    BDD_System* sysPtr,		/* System of BDD's */
    unsigned int n)		/* Index of the variable */
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

unsigned int
BDD_NotNthVariable(
    BDD_System* sysPtr,		/* System of BDD's */
    unsigned int n)		/* Index of the variable */
{
    return BDD_MakeBead(sysPtr, n, 1, 0);
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_Dotify --
 *
 *	Produce BDD output in a suitable format for 'dot'
 *
 * Side effects:
 *
 *	Displays the given BDD in 'dot' format on the standard output.
 *
 * TODO: Make this return the diagram as a string, print from the caller.
 *
 *-----------------------------------------------------------------------------
 */

static void
DotifyBead(
    BDD_System* sysPtr,		/* System of BDD's */
    unsigned char* done,	/* Flags for whether beads have been printed */
    unsigned int x)		/* BDD to display */
{
    unsigned int byte = x / CHAR_BIT;
    unsigned int bit = 1 << (x % CHAR_BIT);
    if (done[byte] & bit) return;
    done[byte] |= bit;
    if (x > 1) {
	printf("    n%u [shape=box,label=\"%u\"];\n",
	       x, sysPtr->beads[x].level);
	printf("    n%u -> n%u [style = dotted];\n", x, sysPtr->beads[x].low);
	printf("    n%u -> n%u;\n", x, sysPtr->beads[x].high);
	DotifyBead(sysPtr, done, sysPtr->beads[x].low);
	DotifyBead(sysPtr, done, sysPtr->beads[x].high);
    } else {
	printf("    n%u [shape=oval, label=\"%u\"];\n", x, x);
    }
}    
void
BDD_Dotify(
    BDD_System* sysPtr,		/* System of BDD's */
    unsigned int x)		/* BDD to display */
{
    unsigned int bytes = (sysPtr->beadsAlloc + CHAR_BIT - 1) / CHAR_BIT;
    unsigned char* done = ckalloc(bytes);
    memset(done, 0, bytes);
    printf("digraph G {\n");
    DotifyBead(sysPtr, done, x);
    printf("}\n");
    fflush(stdout);
    ckfree(done);
}

/*
 *-----------------------------------------------------------------------------
 *
 * BDD_Restrict --
 *
 *	Restricts a BDD by replacing a set of variables with constants.
 *
 * Results:
 *	Returns a bead index representing the BDD as restricted.
 *
 * The set of replacements is represented by an array of BDD_VarAssignment
 * pairs. Each of the pairs represents a variable and the value that the
 * variable should receive. The variable indices must be listed in ascending
 * order.
 *
 *-----------------------------------------------------------------------------
 */

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
    UIHash1MP* hashPtr,		/* Hash table for memoized results */
    unsigned int x,		/* BDD to enumerate */
    mp_int* count)		/* Count of satisfying variable assignments */
{
    Bead* xPtr;
    Bead* lowPtr;
    Bead* highPtr;
    mp_int lresult, hresult;
    int status;

    /* Is the result cached? */
    mp_int* cachedResult = FindInUIHash1MP(hashPtr, x);
    if (cachedResult != NULL) {
	return mp_copy(cachedResult, count);
    }
    if (x < 2) {
	return mp_set_int(count, x);
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
	/* do nothing */
    } else {
	AddToUIHash1MP(hashPtr, x, count);
    }

    mp_clear_multi(&lresult, &hresult, NULL);
    return status;
}
int
BDD_SatCount(
    BDD_System* sysPtr,		/* System of BDD's */
    unsigned int x,		/* BDD to enumerate */
    mp_int* count)		/* Count of satisfying variable assignments */
{
    UIHash1MP* hashPtr = NewUIHash1MP(16);
				/* Hash table for memoized results */
    int status = SatCount(sysPtr, hashPtr, x, count);
    DeleteUIHash1MP(hashPtr);
    if (status == MP_OKAY) {
	status = mp_mul_2d(count, sysPtr->beads[x].level, count);
    }
    return status;
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * End:
 */
