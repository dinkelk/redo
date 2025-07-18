#
# This Dockerfile needs to be run from within the project/ directory (AKA ../../ from here)
# so that docker has access to all the files it needs. ie.
#
# $ docker build -t $DOCKER_IMAGE_NAME -f redo/docker/Dockerfile .
#
# For best results use the ./build_image.sh and ./create_container.sh scripts
# provided in this directory.
#
FROM haskell:9.10.2 AS base
LABEL org.opencontainers.image.source=https://github.com/dinkelk/redo
LABEL org.opencontainers.image.description="Development environment for redo"
LABEL org.opencontainers.image.licenses=MIT

# Install minimal additional tools and hlint
RUN apt-get update && apt-get install -y \
    git \
    build-essential \
    python3 \
    && rm -rf /var/lib/apt/lists/* \
    && cabal update \
    && cabal install hlint --install-method=copy

# Copy the local redo source code (with our updates)
COPY . /root/redo

# Build redo with updated dependencies
WORKDIR /root/redo
RUN ./do all

# Add redo to the root user's path
ENV PATH="/root/redo/bin:${PATH}"

# Make sure user is root at end.
USER root
