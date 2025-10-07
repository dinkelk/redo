#!/bin/bash
# Prepare stack.yaml for Docker-based builds with system GHC
set -e

# Enable system-ghc to use the GHC installed in the Docker image
sed -i '/^resolver:/a system-ghc: true' stack.yaml

# Uncomment existing system-ghc line if present (for idempotency)
sed -i 's/^# system-ghc: true/system-ghc: true/' stack.yaml

# Skip GHC version check - use whatever system GHC is available
sed -i '/^resolver:/a skip-ghc-check: true' stack.yaml

echo "stack.yaml prepared for Docker build with system GHC"
