
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

# Make sure the user has cabal and ghc installed:
ercho
ercho "Making sure ghc and cabal are installed..."
check_dependency "ghc"
check_dependency "cabal"
ercho

# Make sure cabal dependencies are installed:
# Don't mess with the users local haskell installation:
ercho "Creating a cabal sandbox to store the redo dependencies..."
cabal sandbox init >&2
check_exit
ercho
ercho "Installing the redo dependencies..."
cabal install --dependencies-only >&2
check_exit
ercho

# Run the source building and installation:
ercho "Building redo..."
redo src/all

# Print success:
ercho
ercho "Install complete - redo binaries installed in 'bin/'"

