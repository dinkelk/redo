rm -f static.log

redo static1 static2

#touch static.in
# ^ touching wont work, we need to modify the file for hash based redo
cp -f static.in.mod static.in

#../flush-cache
redo-ifchange static1 static2

cp -f static.in.back static.in

COUNT=$(wc -l <static.log)
. ../skip-if-minimal-do.sh
[ "$COUNT" -eq 4 ] || exit 55

touch $3
