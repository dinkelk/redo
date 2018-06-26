if test -f "do_error"
then
  rm -f do_error
  echo "BADBADBAD"
  exit 1
else
  touch do_error
  if test -f maybe_error
  then
    cat maybe_error
  fi
  echo -n "Y"
  redo-ifchange static
  exit 0
fi
