#!/usr/bin/env bash
source "${DOCKER_DEV_SCRIPTS}/environment"
echo
echo "** You probably meant to type 'docker images'"
echo "** but hey, we're pretty laid back around here..."
echo
$PODMAN images $@
