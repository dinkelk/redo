# Dependencies:
source=`ls *.hs`
redo-ifchange $source

#PROFILE="-prof -fprof-auto -rtsopts"
PROFILE=
ROOT=`pwd`/..

# Compile Main.hs to temporary filename $3
stack --allow-different-user ghc -- -O2 -v1 -Wall $PROFILE -o $3 Main.hs 1>&2
