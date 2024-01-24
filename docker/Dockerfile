#
# This Dockerfile needs to be run from within the project/ directory (AKA ../../ from here)
# so that docker has access to all the files it needs. ie.
#
# $ docker build -t $DOCKER_IMAGE_NAME -f redo/docker/Dockerfile .
#
# For best results use the ./build_image.sh and ./create_container.sh scripts
# provided in this directory.
#
FROM ubuntu:22.04 AS base
LABEL org.opencontainers.image.source=https://github.com/dinkelk/redo
LABEL org.opencontainers.image.description="Development environment for redo"
LABEL org.opencontainers.image.licenses=MIT

ENV DEBIAN_FRONTEND=noninteractive

# install common dependencies
RUN DEBIAN_FRONTEND=noninteractive apt-get update && DEBIAN_FRONTEND=noninteractive apt-get -yq install \
    software-properties-common \
    apt-utils \
    locales \
    curl \
    lsb-release \
    sudo \
    python3 \
    git \
    build-essential \
    cmake \
    hlint \
    && DEBIAN_FRONTEND=noninteractive apt-get -yq clean

# ensure we have the en_US.UTF-8 locale available
RUN locale-gen en_US.UTF-8
RUN rm /etc/apt/apt.conf.d/docker-clean

# Install redo:
ENV STACK_ROOT=/root/.stack
RUN DEBIAN_FRONTEND=noninteractive sudo apt-get install -yq wget \
    && wget -qO- https://get.haskellstack.org/ | sh \
    && git config --global core.autocrlf false \
    && git clone https://github.com/dinkelk/redo.git /root/redo \
    && /root/redo/do /root/redo/all

# Add redo to the root user's path
RUN echo 'export PATH="/root/redo/bin:$PATH"' >> /root/.bashrc

# Make sure user is root at end.
USER root
