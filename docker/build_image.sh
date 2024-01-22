#!/bin/sh

. ./docker_config.sh
# Execute the docker machine from the project/ directory so that we have access
cd ../..
execute "docker build --progress=plain -t $DOCKER_IMAGE_NAME -f redo/docker/Dockerfile ."
cd - >/dev/null
