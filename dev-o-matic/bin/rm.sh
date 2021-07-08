#!/usr/bin/env bash
set -e


if [ "$1" == '-f' ]; then
    docker rm "$CONTAINERNAME"
    docker rmi "$IMAGENAME"
else
    echo "Removes the current dev container and image"
    echo "   Usage: dev rm -f"
fi
