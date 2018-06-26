redo clean
../flush-cache

# Expect successful build:
redo depend_on_maybe_error
if ! test "`cat maybe_error`" = "Y"
then
  exit 3
fi
../flush-cache

# Expect error this time:
# Force a rebuild
touch maybe_error.do
! redo depend_on_maybe_error 2>/dev/null
if ! test "`cat maybe_error`" = "Y"
then
  exit 1
fi
../flush-cache

# Expect success this time:
redo depend_on_maybe_error
if ! test "`cat maybe_error`" = "YY"
then
  exit 2
fi
