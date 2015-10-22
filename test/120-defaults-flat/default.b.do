#if [ -e "$1.a" -o -e "default${1#$2}.a" ]; then # I changed this because I use a differen $2 arg
if [ -e "$1.a" -o -e "default.b.a" ]; then
	redo-ifchange "$1.a"
	echo a-to-b
	cat "$1.a"
else
	redo-ifchange "$1.b"
	echo b-to-b
	cat "$1.b"
fi
../sleep 1.1
