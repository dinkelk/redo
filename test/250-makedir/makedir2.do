rm -f makedir.log
redo makedir
touch makedir/outfile
# Don't need this for haskell redo
#../flush-cache
redo-ifchange makedir
COUNT=$(wc -l <makedir.log)
[ "$COUNT" -eq 1 ] || exit 99
