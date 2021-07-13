#!/usr/bin/env -S bash -x

MYDIR=$(dirname $0)
source "${MYDIR}/../scripts/environment"
source "${MYDIR}/../scripts/devenv-utils"

docker container ls --all | \
    grep "docker-devenv:latest" | \
    grep Exited | \
    awk '{ print $1}' | \
    xargs docker container rm

docker image ls -a | \
    grep ${IMAGENAME} | \
    sort | \
    awk '{ print $3 }' | \
    xargs docker rmi

make -C "${MYDIR}/.." clean
