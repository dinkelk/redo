#!/bin/bash
# Prepare stack.yaml for Docker-based builds with system GHC
set -e

# Enable system-ghc to use the GHC installed in the Docker image
sed -i '/^resolver:/a system-ghc: true' stack.yaml

# Uncomment existing system-ghc line if present (for idempotency)
sed -i 's/^# system-ghc: true/system-ghc: true/' stack.yaml

# Allow newer minor versions of GHC than specified in the resolver
sed -i 's/^# compiler-check: newer-minor/compiler-check: newer-minor/' stack.yaml

echo "stack.yaml prepared for Docker build with system GHC"
