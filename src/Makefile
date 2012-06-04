PROFILE=
#PROFILE=-pg -fprofile-arcs

CC=gcc

CFLAGS=-std=c99 -Wall

CLIB=ftoic
#CLIB=ftoicJ
#CLIB=ftoicROT
#CLIB=ftoicSIM
#CLIB=ftoicUC
#CLIB=ftoicUNI
#CLIB=ftoicZOB
#CLIB=ftoicCRC
#CLIB=ftoicFNV
#CLIB=ftoicONE
#CLIB=ftoicWANG
#CLIB=ftoicJJ
#CLIB=ftoicDJB

PROFILES=
#PROFILES=-pg -fprofile-arcs

.PHONY: dist
.PHONY: mhtransf
.PHONY: %.ex

dist: mhtransf parser

mhtransf:
	ghc --make -o $@ Main.hs

%.ex:
	cat $*.hs | ./mhparse -q | ./mhtransf $(PARAMS) > $*.c
	$(CC) $(CFLAGS) -O3 $(PROFILES) $*.c

parser:
	cd MH-Parser && make 

clean:
	$(RM) *.hi *.o main.c a.out gmon.out *~ *.da
	cd MH-Parser && make clean

distclean: clean
	$(RM) mhtransf
	cd MH-Parser && make distclean

