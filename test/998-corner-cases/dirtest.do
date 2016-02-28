dirTest()
{
  target=$1
  rm -f $target.log
  rm -rf $target
  redo-ifchange $target || exit 90
  test -d $target || exit 91
  test -f $target/file || exit 92
  # These should not do anything
  redo-ifchange $target || exit 93
  test -d $target || exit 96
  test -f $target/file || exit 97
  redo-ifchange $target || exit 94
  test -d $target || exit 96
  test -f $target/file || exit 97
  # Remove should trigger a rebuild
  rm -rf $target
  redo-ifchange $target || exit 95
  test -d $target || exit 96
  test -f $target/file || exit 97
  redo-ifchange $target || exit 98
  [ `cat $target.log | wc -l` = "2" ] || exit 99
}

dirTest dir
dirTest dir1
dirTest dir2
dirTest dir3


