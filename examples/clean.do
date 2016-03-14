for dir in `ls -d */`
do
  echo $dir'clean'
done | xargs redo

