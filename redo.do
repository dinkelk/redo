#!/bin/sh

# Compile redo.hs to filename $3 with verbosity set to low 
# so there is nothing printed to standard out
ghc -v0 -o $3 redo.hs
