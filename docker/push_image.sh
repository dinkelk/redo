#!/bin/sh

. ./docker_config.sh
execute "docker push $DOCKER_IMAGE_NAME"
