# Dependencies:
source=InstallHelper.hs
redo-ifchange $source

ROOT=`pwd`/..
SANDBOX=$ROOT/.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d
PACKAGE="-package-db=$SANDBOX"
DIRS=../src

# Compile the helper:
ghc -O2 -v1 -Wall -i$DIRS $PACKAGE -o $3 $source 1>&2
