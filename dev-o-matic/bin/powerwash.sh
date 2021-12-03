#!/usr/bin/env -S bash -x
source "${DOCKER_DEV_SCRIPTS}/environment"
source "${DOCKER_DEV_SCRIPTS}/devenv-utils"

$PODMAN container ls --all | \
    grep "docker-devenv:latest" | \
    grep Exited | \
    awk '{ print $1}' | \
    xargs docker container rm

$PODMAN image ls -a | \
    grep ${IMAGENAME} | \
    sort | \
    awk '{ print $3 }' | \
    xargs docker rmi

make -C "${DOCKER_DEV_SCRIPTS}/.." clean
