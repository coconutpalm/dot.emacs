#!/usr/bin/env -S bash -x

echo cd $DOCKER_DEV
cd $DOCKER_DEV
pwd


if [ -z "$(docker image ls -a | grep docker-devenv)" ]; then
    # If someone powerwashed before calling rebuild,then clean and rebuild from scratch
    make clean
    make
elif [ -z "$1" ]; then
    # If there's no argument, default to the 'interactive' target
    make interactive
else
    make $@
fi
