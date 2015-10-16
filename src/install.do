#!/bin/sh

# Dependencies:
binaries="redo redo-ifchange"
redo-ifchange $binaries

# Create bin directory and copy over built files:
installdir=../bin
mkdir -p $installdir
mv $binaries $installdir 
