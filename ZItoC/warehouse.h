#ifndef __WAREHOUSE_H__
#define __WAREHOUSE_H__


/* Header files */

#include <stdbool.h>
#include "hashcons.h"


/* Definitions of types */

typedef unsigned int identifier;
typedef int value;
typedef unsigned int tagWH;


/* Function prototypes */

void  initializeWH (void);
bool  seekWH       (identifier id, dimension d, World *w, tagWH *hV);
void  putWH        (tagWH hV, value val);
value getWH        (tagWH hV);
void  statsWH      (void);

void  printWorld   (World *w);

#endif
