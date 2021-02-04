rm -f static.log

redo static1 static2
sleep 0.01 # for some reason static2 and static.in were getting the same timestamp which was failing the test.

touch static.in
../flush-cache
#redo-ifchange static1 static2
redo-ifchange -x --debug2 --check static1 static2 2>out

COUNT=$(wc -l <static.log)
. ../skip-if-minimal-do.sh
[ "$COUNT" -eq 4 ] || exit 55
