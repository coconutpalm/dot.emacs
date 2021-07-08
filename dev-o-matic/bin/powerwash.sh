#!/usr/bin/env -S bash -x

MYDIR=$(dirname $0)
source "${MYDIR}/../scripts/environment"
source "${MYDIR}/../scripts/devenv-utils"

docker image ls -a | grep ${IMAGENAME} | sort | awk '{ print $3 }' | xargs docker rmi
dgc
make -C "${MYDIR}/.." clean
