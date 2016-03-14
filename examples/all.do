for dir in `ls -d */`
do
  echo $dir'all'
done | xargs redo

