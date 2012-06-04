#ifndef __HASHCONS_I_H__
#define __HASHCONS_I_H__


/* Header files */

#include "hashcons.h"


/* Definitions of macros */

/* null pointer (invalid value) */
#define HCNUL         ((tagHC) (-1))
/* mask for hashing */
#define HCMASK        ((tagHC) (HCTABLESIZE - 1))
/* manipulating the Active bit (head: MSB) during HC GC */
#define ACTIVEMASK    ((tag) 1 << (8 * sizeof(tag) - 1))


/* Definitions of constants */

/* the hash table size */
extern const tagHC HCTABLESIZE;
/* the heap size for the hash consing table */
extern const tagHC HCHEAPSIZE;


/* Definitions of types */

struct world_tag {
  struct world_tag * prev;
  list l[];                   /* incomplete... */
};

struct bucketHC {					
  tagHC ptr;            /* pointer to the corresponding bucket */
};

/* heap node in a bucket */
struct nodeHC {
  tag   head;
  list  tail;
  tagHC ptr;
};


/* Definition of variables */

/* pointer to the next free record of the heap */
extern tagHC freeHC;
/* hash table */
extern struct bucketHC * tableHC;
/* heap for list Hash Consing */
extern struct nodeHC * heapHC;


#endif
