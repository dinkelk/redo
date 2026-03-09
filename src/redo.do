# Dependencies:
source=`ls *.hs`
csource=`ls *.c`
redo-ifchange $source $csource

#PROFILE="-prof -fprof-auto -rtsopts"
PROFILE=
ROOT=`pwd`/..

# Compile Main.hs to temporary filename $3
# Use -fPIC because CI links with dynamic objects and C objects must be PIC.
stack --allow-different-user ghc -- -O2 -fPIC -v1 -Wall $PROFILE -o $3 Main.hs $csource 1>&2
