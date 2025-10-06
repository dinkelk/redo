
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

# Check if we should use system GHC
SYSTEM_GHC_FLAG=""
if grep -q "^system-ghc: *true" stack.yaml 2>/dev/null; then
  SYSTEM_GHC_FLAG="--system-ghc"
fi

# Install GHC (skip if using system GHC):
if [ -z "$SYSTEM_GHC_FLAG" ]; then
  stack ${STACK_ALLOW_DIFFERENT_USER:+--allow-different-user} setup
fi

# Install and build package dependencies
stack ${STACK_ALLOW_DIFFERENT_USER:+--allow-different-user} $SYSTEM_GHC_FLAG build

# Build redo and install it locally in ./bin:
ercho "Building redo..."
redo src/all

# Print success:
ercho
ercho "Install complete - redo binaries installed in 'bin/'"
