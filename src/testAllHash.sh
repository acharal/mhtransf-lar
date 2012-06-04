#! /bin/sh

if [ $# != 2 ]; then
  echo "Usage: testAllHash <make-target> <out-file-prefix>"
  exit 1
fi

for h in `cat ZItoC/allHash.txt | grep -v '^#'` ; do
  echo "Building $1 with HASH_$h"
  make CLIB="ftoic$h" "$1"
  ./a.out > "$2_$h.out"
  gprof > "$2_$h.prof"
done
