# Dependencies:
source=`ls *.hs`
redo-ifchange $source

#PROFILE="-prof -fprof-auto -rtsopts"
PROFILE=
ROOT=`pwd`/..
SANDBOX="`echo $ROOT/.cabal-sandbox/*-packages.conf.d | head -1`"
PACKAGE="-package-db=$SANDBOX"

# Compile Main.hs to temporary filename $3
ghc -O2 -v1 -Wall $PACKAGE $PROFILE -o $3 Main.hs 1>&2
