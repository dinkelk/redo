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

# setup the user
#RUN if ! getent passwd $DOCKER_USER; then useradd -d /home/$DOCKER_USER -m -s /bin/bash $DOCKER_USER; fi \
#    && echo $DOCKER_USER:$DOCKER_USER | chpasswd \
#    && echo "$DOCKER_USER ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers \
#    && mkdir -p /etc/sudoers.d \
#    && echo "$DOCKER_USER ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/$DOCKER_USER \
#    && chmod 0440 /etc/sudoers.d/$DOCKER_USER \
#    && chown -R $DOCKER_USER:$DOCKER_USER $HOME
RUN rm /etc/apt/apt.conf.d/docker-clean

# Install leveldb:
RUN cd /root \
    && git clone --recurse-submodules https://github.com/google/leveldb.git \
    && cd leveldb && mkdir -p build && cd build \
    && cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON .. && cmake --build .
    

# Install redo:
ENV STACK_ROOT=/root/.stack
RUN DEBIAN_FRONTEND=noninteractive sudo apt-get install -yq wget \
    && wget -qO- https://get.haskellstack.org/ | sh
#    && git config --global core.autocrlf false \
#    && git clone https://github.com/dinkelk/redo.git /root/redo \
#    && /root/redo/do /root/redo/all

# Make sure user is root at end.
USER root
