# make sure redo-ifchange records the dependency correctly if we chdir
cd ..
redo-ifchange 550-chdir/chdir1
cd - >> /dev/null
touch $3
