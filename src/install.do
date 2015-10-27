#!/bin/sh

# Dependencies:
binaries="redo redo-ifchange redo-ifcreate redo-always"
redo-ifchange $binaries

# Create bin directory and copy over built files:
installdir=../bin
mkdir -p $installdir
for b in $binaries; do strip $b; done
mv $binaries $installdir 

touch $3
