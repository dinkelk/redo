# Dependencies:
source=`ls *.hs`
redo-ifchange $source

PROFILE="-prof -fprof-auto -rtsopts"
#PROFILE=
ROOT=`pwd`/..
SANDBOX=$ROOT/.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d
PACKAGE="-package-db=$SANDBOX"

# Compile redo.hs to filename $3 with verbosity set to low 
# so there is nothing printed to standard out
ghc -O2 -v1 -Wall $PACKAGE $PROFILE -o $3 redo.hs 1>&2
