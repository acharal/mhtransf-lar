#! /bin/sh

make distclean

for h in `cat allHash.txt | grep -v '^#'` ; do
  echo "Building library with HASH_$h"
  make HASH="-DHASH=HASH_$h"
  mv -f libftoic.a libftoic$h.a
  make distclean
done
