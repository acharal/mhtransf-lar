#ifndef __WAREHOUSE_I_H__
#define __WAREHOUSE_I_H__


/* Header files */

#include "warehouse.h"


/* Definitions of macros */

/* null pointer (invalid value) */
#define WHNUL         ((tagWH) (-1))
/* mask for hashing */
#define WHMASK        ((tagWH) (WHTABLESIZE - 1))
/* number of entries triggering WH GC */
#define WHGCLIMIT     ((tagWH) (WHHEAPSIZE * 0.98))
/* number of entries left after WH GC */
#define WHGCRATE      ((tagWH) (WHHEAPSIZE * 0.0))
/* entries age limit rate */
#define AGERATE       0.5						
/* the "struct hack" is used in the heap node */
#define heapWH(i)     ((struct nodeWH *) \
		       ((unsigned char *) rawHeapWH \
			+ (i) * (sizeof(struct nodeWH) \
			       + MAXDIM * sizeof(list))))
/* WH entry waiting for a value, to prevent WH GC */
#define WAITINGVALUE ((value) 1 << (8 * sizeof(value) - 1))


/* Definitions of constants */

/* the hash table size */
extern const tagWH WHTABLESIZE;
/* the heap size for the warehouse */
extern const tagWH WHHEAPSIZE;


/* Definition of types */

typedef unsigned int tagAge;

struct bucketWH {					
  tagWH ptr;            /* pointer to the corresponding bucket */
};

/* heap node in a bucket */
struct nodeWH {	
  identifier id;
  value      val;
  tagAge     age;
  tagWH      ptr;
  dimension  depDim;   /* new thing! */
  list       l[];      /* incomplete... */
};


/* Definitions of variables */

/* number of succesful WH searches */
extern tagWH hits;
/* age of the younger entry */
extern tagWH globalAge;
/* number of entries in the heap */
extern tagWH entriesWH;
/* pointer to the next free record of the heap */
extern tagWH freeWH;
/* hash table */
extern struct bucketWH * tableWH;
/* warehouse heap */
extern void * rawHeapWH;


/* Function prototypes */

void gcollectWH (void);


#endif
