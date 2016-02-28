rm -f a.b.c.do*
rm -f default.c.do*
rm -f default.b.c.do*
rm -f a.b.c
rm -f default.do.log

../../flush-cache
redo-ifchange a.b.c
test -f a.b.c || exit 1
[ `cat default.do.log | wc -l` = "1" ] || exit 9

../../flush-cache
echo 'touch $3\necho "asdf" >> default.c.do.log' > default.c.do
redo-ifchange a.b.c
test -f a.b.c || exit 2
[ `cat default.c.do.log | wc -l` = "1" ] || exit 99

../../flush-cache
echo 'touch $3\necho "asdf" >> default.b.c.do.log' > default.b.c.do
redo-ifchange a.b.c
test -f a.b.c || exit 3
[ `cat default.b.c.do.log | wc -l` = "1" ] || exit 98

../../flush-cache
echo 'touch $3\necho "asdf" >> a.b.c.do.log' > a.b.c.do
redo-ifchange a.b.c
test -f a.b.c || exit 4
[ `cat a.b.c.do.log | wc -l` = "1" ] || exit 97

../../flush-cache
rm -f a.b.c.do*
redo-ifchange a.b.c
test -f a.b.c || exit 3
[ `cat default.b.c.do.log | wc -l` = "2" ] || exit 96

../../flush-cache
rm -f default.b.c.do*
redo-ifchange a.b.c
test -f a.b.c || exit 2
[ `cat default.c.do.log | wc -l` = "2" ] || exit 95

../../flush-cache
rm -f default.c.do*
redo-ifchange a.b.c
test -f a.b.c || exit 1
[ `cat default.do.log | wc -l` = "2" ] || exit 94

rm -f default.do.log
rm -f a.b.c
