
ercho()
{
  echo "$@" >&2
}

check_dependency()
{
  dep=$1
  which $dep > /dev/null
  if [ "$?" != "0" ]
  then
    ercho "Error: '$dep' must be installed before proceeding!"
    exit 1
  fi
}

check_exit()
{
  if [ "$?" != "0" ]
  then
    ercho "Error: installation failed!"
    exit 1
  fi
}

# Make sure the user has stack installed:
ercho
ercho "Making sure haskell stack (haskellstack.org) is installed..."
check_dependency "stack"
ercho

# Install GHC and dependencies:
stack setup --allow-different-user
stack build --allow-different-user

# Build redo and install it locally in ./bin:
ercho "Building redo..."
redo src/all

# Print success:
ercho
ercho "Install complete - redo binaries installed in 'bin/'"
