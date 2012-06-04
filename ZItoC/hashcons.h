#ifndef __HASHCONS_H__
#define __HASHCONS_H__


/* Definitions of types */

typedef unsigned char tag;
typedef unsigned int  list;

typedef unsigned char dimension;
typedef struct world_tag World;


/* Definitions of constants */

/* number of dimensions */
extern const dimension MAXDIM;


/* Definitions of types */

typedef unsigned int tagHC;


/* Definitions of variables */

/* number of entries triggering HC GC */
extern tagHC HCGCLIMIT;
/* number of entries in the heap */
extern tagHC entriesHC;

/* Function prototypes */

list call         (tag i, list l);
list actuals      (tag i, list l);
tag  cases        (list l);

void initializeHC (void);
void statsHC      (void);
void gcollectHC   (World *w);

void printList    (list l);


#endif
