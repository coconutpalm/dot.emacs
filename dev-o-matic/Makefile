#!/usr/bin/env make
SHELL := /bin/bash

export $(shell sed 's/=.*//' scripts/environment)

export DOCKER_BUILDKIT=0

CONFIG_UPDATED=prereqs/.all-upgrades-applied

.DEFAULT_GOAL := ALL

# Build all the things
ALL: .prereqs base-image/.build-step interactive-image/.build-step

.prereqs:
	make -C prereqs

base-image/.build-step:
	IMAGENAME=${IMAGENAME}-base make -C base-image

interactive-image/.build-step:
	make -C interactive-image


# Running and maintanence
interactive:
	bash -c "docker rmi ${IMAGENAME}; exit 0"
	bash -c '[ -f interactive-image/.build-step ] && rm interactive-image/.build-step; exit 0'
	make

run: .full-image
	bin/run.sh

clean:
	rm -f .full-image
	find . -name .build-step | xargs rm -f

veryclean: clean
	docker rm "${CONTAINERNAME}"

# Danger!  This is the nuclear option; it will blow away *all* EXITED containers!
# including possibly those not related to repeatable-dev-environment
powerwash: veryclean
	bin/powerwash.sh
