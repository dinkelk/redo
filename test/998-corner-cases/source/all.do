rm -f a
rm -f a.log
touch source

redo-ifchange a
test -f a || exit 1

# removing a source file if a redo-ifchange is on it should always say no rule to build until dep in dep file is removed
../../flush-cache
rm -f source
! redo-ifchange a >& /dev/null || exit 2
test -f a || exit 1
! redo-ifchange a >& /dev/null || exit 3
test -f a || exit 1
! redo-ifchange a >& /dev/null || exit 4
test -f a || exit 1

# updating a.do should cause a successful rebuild
cp a.do.bak a.do
redo-ifchange a || exit 5
test -f a || exit 1
redo-ifchange a || exit 6
test -f a || exit 1

# restoring original do should cause another failure
../../flush-cache
cp a.do.orig a.do
! redo-ifchange a >& /dev/null || exit 12
test -f a || exit 1
! redo-ifchange a >& /dev/null || exit 13
test -f a || exit 1
! redo-ifchange a >& /dev/null || exit 14

# recreating source should cause a rebuild
touch source
redo-ifchange a || exit 15
test -f a || exit 1
redo-ifchange a || exit 16
test -f a || exit 1

# touching source should also cause a rebuild
../../flush-cache
touch source
redo-ifchange a || exit 17
redo-ifchange a || exit 27

[ `cat a.log | wc -l` = "4" ] || exit 99
