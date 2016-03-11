#/bin/sh

##################################################################
##################################################################

What is redo?
'redo target' finds a .do file for "target" and runs it

What is redo-ifchange?
'redo-ifchange target' check whether target needs to be rebuilt,
if so, it finds a .do file for "target" and runs it. After running
the do file, it updates the redo database.

##################################################################
##################################################################

# Legend:

run in the shell
# inner workings of redo

##################################################################
##################################################################

# redo - first run
redo foo
   foo.do
       redo-ifchange foo.o
       # has foo.o been built? - no, run foo.o.do
           foo.o.do
               redo-ifchange foo.c 
               # has foo.c been built? - it already exists, and 
               #   redo didn't build it, so it is a source file
               # mark foo.c as a source file in database
               # store a dependency foo.o -> foo.c in the database
               # store the timestamp of foo.c in the database
               gcc -c foo.c -o $3
       # store a dependency foo -> foo.o in the database
       # store the timestamp of foo.o in the database
       gcc foo.o -o $3
# store the timestamp of foo in the database

##################################################################
##################################################################

# redo - second run
redo foo
   foo.do
       redo-ifchange foo.o
       # has foo.o been built? - yes
       # foo.o -> foo.c, has foo.c been built? - yes
       # foo.c is a source, has it been changed? - no
       #
       # store a dependency foo -> foo.o in the database
       # store the timestamp of foo.o in the database
       gcc foo.o -o $3
# store the timestamp of foo in the database

##################################################################
##################################################################

# redo-ifchange
redo-ifchange foo
   # has foo been built? - yes
   # foo -> foo.o, has foo.o been built? - yes
   # foo.o -> foo.c, has foo.c been built? - yes
   # foo.c is a source, has it been changed? - no

##################################################################
##################################################################

# make a change to foo.c and rebuild
redo foo
   foo.do
       redo-ifchange foo.o
       # has foo.o been built? - yes
       # foo.o -> foo.c, has foo.c been built? - yes
       # foo.c is a source, has it been changed? - yes
           foo.o.do
               redo-ifchange foo.c 
               # has foo.c been built? - it already exists so it is a source file
               # store the timestamp of foo.c in the database
               # store a dependency foo.o -> foo.c in the database
               gcc -c foo.c -o $3
       # store a dependency foo -> foo.o in the database
       # store the timestamp of foo.o in the database
       gcc foo.o -o $3
# store the timestamp of foo in the database

##################################################################
##################################################################

