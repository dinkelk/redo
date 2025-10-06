#
# Dockerfile for redo development environment
#
# Build from project root:
#   docker build -t redo-dev -f docker/Dockerfile .
#
# Run interactively:
#   docker run -it --rm -v $PWD:/workspace -w /workspace redo-dev bash
#
FROM ubuntu:22.04
LABEL org.opencontainers.image.source=https://github.com/dinkelk/redo
LABEL org.opencontainers.image.description="Development environment for redo"
LABEL org.opencontainers.image.licenses=MIT

ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies (same as CI workflow)
RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    libgmp-dev \
    zlib1g-dev \
    libtinfo-dev \
    libffi-dev \
    libncurses-dev \
    python3 \
    gcc \
    wget \
    xz-utils \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install hlint (may not be available on all architectures)
RUN apt-get update && (apt-get install -y hlint || echo "hlint not available via apt") \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Set working directory
WORKDIR /workspace

# Default command
CMD ["/bin/bash"]
