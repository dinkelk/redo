rm -f *.log a b

redo-ifchange a || exit 56
test -f a || exit 1
test -f b || exit 2

# removing b.do should cause a rebuild of a, but then mark it as source
mv b.do b.do.bck
../../flush-cache
redo-ifchange a || exit 56
test -f a || exit 1
test -f b || exit 2
redo-ifchange a || exit 56
redo-ifchange a || exit 56
../../flush-cache # this should not cause a rebuild
redo-ifchange a || exit 56

# providing a b.do should not cause a rebuild because b is now source
mv b.do.bck b.do
redo-ifchange a || exit 56
redo-ifchange a || exit 56
../../flush-cache # this should not cause a rebuild
redo-ifchange a || exit 56

[ `cat a.log | wc -l` = "2" ] || exit 99
[ `cat b.log | wc -l` = "1" ] || exit 98

# removing b will make it a target once again
rm -f b
redo-ifchange a || exit 56
redo-ifchange a || exit 56
../../flush-cache # this should not cause a rebuild
redo-ifchange a || exit 56

[ `cat a.log | wc -l` = "3" ] || exit 99
[ `cat b.log | wc -l` = "2" ] || exit 98
