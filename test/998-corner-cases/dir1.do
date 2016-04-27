cwd=`pwd`
cd ..
mkdir -p $3
touch $3/file
echo "asdf" >> $cwd/$1.log
