# This build step depends on the specified source artifacts
SHELL := /bin/bash

.DEFAULT_GOAL := .build-step
DEPENDENCIES=$(find . -type f | grep -v build-step)

.build-step: ${DEPENDENCIES}
	./install.sh
	touch .build-step


clean:
	rm .build-step
	$PODMAN container rm $($PODMAN container ls -a | grep $IMAGENAME | awk '{ print $1 }')
