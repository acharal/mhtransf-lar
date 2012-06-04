CC=gcc

#HASH=-DHASH=HASH_J
#HASH=-DHASH=HASH_ROT
#HASH=-DHASH=HASH_SIM
#HASH=-DHASH=HASH_UC
#HASH=-DHASH=HASH_UNI
#HASH=-DHASH=HASH_ZOB
#HASH=-DHASH=HASH_CRC
#HASH=-DHASH=HASH_FNV
#HASH=-DHASH=HASH_ONE
#HASH=-DHASH=HASH_WANG
#HASH=-DHASH=HASH_JJ
#HASH=-DHASH=HASH_DJB

#WAREHOUSE=-DGC_KEEP_WAREHOUSE
WAREHOUSE=-DGC_KEEP_WAREHOUSE_NO

PROFILE=
#PROFILE=-pg -fprofile-arcs

CFLAGS=-std=c99 -pedantic -Wall -g $(WAREHOUSE) $(HASH) $(PROFILE)
#CFLAGS=-std=c99 -pedantic -Wall $(WAREHOUSE) $(HASH) $(PROFILE) -DDEBUG
#CFLAGS=-std=c99 -pedantic -Wall $(WAREHOUSE) $(HASH) $(PROFILE) -DSANITY
#CFLAGS=-std=c99 -pedantic -Wall $(WAREHOUSE) $(HASH) $(PROFILE) -DDEBUG -DSANITY

HLIB=hashcons.h warehouse.h
HILIB=hashcons-i.h warehouse-i.h
CLIB=hashcons.c warehouse.c
OLIB=hashcons.o warehouse.o
SRCLIB=$(HLIB) $(HILIB) $(CLIB)

.PHONY: clean

libftoic.a: $(OLIB)
	ar r $@ $^

hashcons.o: hashcons.c hashcons-i.h warehouse-i.h hashcons.h warehouse.h
warehouse.o: warehouse.c hashcons-i.h warehouse-i.h hashcons.h warehouse.h

clean:
	$(RM) $(OLIB) *~ gmon.out *.da

distclean: clean
	$(RM) libftoic.a
