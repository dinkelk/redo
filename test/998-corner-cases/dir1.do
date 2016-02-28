cwd=`pwd`
cd ..
mkdir -p $cwd/$3
touch $cwd/$3/file
echo "asdf" >> $cwd/$1.log
