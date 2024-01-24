#!/bin/sh

DOCKER_CONTAINER_NAME="redo_container"
DOCKER_IMAGE_NAME="ghcr.io/dinkelk/redo:latest"
export DOCKER_CONTAINER_NAME
export DOCKER_IMAGE_NAME

# Helper function to print out command as executed:
execute () {
  echo "$ $@"
  eval "$@"
}
