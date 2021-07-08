#!/usr/bin/env bash
set -e

#
# Build Docker container
#

source ../scripts/environment

GNAME=`cat /etc/group | grep :$(id -g): | awk -F : '{ print $1 }'`
GID=`id -g`
TZ=`readlink /etc/localtime | cut -d'/' -f6,7`   # Note this is a Mac-specific `cut`

cp -R ~/.ssh docker/build

cd docker && docker build \
        --build-arg USER="$USER" \
        --build-arg UID="$UID" \
        --build-arg GNAME="$GNAME" \
        --build-arg GID="$GID" \
        --build-arg TZ="$TZ" \
        --rm \
        --memory 24g \
        --shm-size 2g \
        -t ${IMAGENAME} \
    	  . && cd ..

rm -fr docker/build/.ssh
