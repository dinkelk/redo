# Dependencies:
redo-ifchange redo

# Just copy over the redo binary since it is also redo-ifchange
# We could just create a symlink here, but this is safer
cp redo $3
