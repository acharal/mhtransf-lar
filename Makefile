CC=gcc
PARSE=./parse
TRANSF=./transf

CFLAGS=-std=c99 -Wall

INCDIR=./ZItoC
LIBDIR=./ZItoC
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

LARPARAMS=
WHPARAMS=-nd -ht 22 -hh 20 -wt 22 -wh 20

.PHONY: dist
.PHONY: mhtransf
.PHONY: %.ex


dist:
	cd src && make dist
	cd ZItoC && make
	ln -s src/MH-Parser/mhparse parse
	ln -s src/mhtransf          transf

%.ex:
	$(PARSE) -q < $*.hs | $(TRANSF) -clar $(LARPARAMS) > $*.c
	$(CC) $(CFLAGS) -O3 $(PROFILES) $*.c -o $*-lar

%.ex.wh:
	$(PARSE) -q < $*.hs | $(TRANSF) -cwh  $(WHPARAMS) > $*.c
	$(CC) $(CFLAGS) -O3 $(PROFILES) -I$(INCDIR) -L$(LIBDIR) $*.c -o $*-wh -l$(CLIB)

distclean:
	cd src && make distclean
	cd ZItoC && make clean
	rm parse transf
