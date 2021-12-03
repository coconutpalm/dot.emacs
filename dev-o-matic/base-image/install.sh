#!/usr/bin/env bash
set -e

#
# Build Docker container
#
source ../scripts/environment

$PODMAN pull ubuntu:rolling
$PODMAN build \
       -t ${IMAGENAME}-base \
       --rm \
       --memory 24g \
       --shm-size 2g \
       .
