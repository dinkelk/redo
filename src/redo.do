# Dependencies:
source=`ls *.hs`
redo-ifchange $source

#PROFILE="-prof -fprof-auto -rtsopts"
PROFILE=

# Compile redo.hs to filename $3 with verbosity set to low 
# so there is nothing printed to standard out
ghc -O2 -v0 -Wall $PROFILE -o $3 redo.hs 1>&2
