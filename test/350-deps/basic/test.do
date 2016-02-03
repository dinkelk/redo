rm -f *.out *.log

../../flush-cache
redo-ifchange 1.out 2.out
[ "$(cat 1.log | wc -l)" -eq 1 ] || exit 55
[ "$(cat 2.log | wc -l)" -eq 1 ] || exit 56
../../flush-cache
# Touching a file is not enough to rebuild it in haskell redo... the file needs to change.
#touch 1.in
# do this instead
cp -f 1.in.mod 1.in

redo-ifchange 1.out 2.out
cp -f 1.in.back 1.in
[ "$(cat 2.log | wc -l)" -eq 1 ] || exit 58
. ../../skip-if-minimal-do.sh
[ "$(cat 1.log | wc -l)" -eq 2 ] || exit 57

