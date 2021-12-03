#!/usr/bin/env bash
set -e
source "${DOCKER_DEV_SCRIPTS}/environment"

if [ "$1" == '-f' ]; then
    $PODMAN rm "$CONTAINERNAME"
    $PODMAN rmi "$IMAGENAME"
else
    echo "Removes the current dev container and image"
    echo "   Usage: dev rm -f"
fi
