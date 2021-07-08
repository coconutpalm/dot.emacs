# This build step depends on the specified source artifacts

.DEFAULT_GOAL := .build-step

.build-step: Dockerfile base-environment.sh Makefile
	docker pull ubuntu:rolling
	docker build \
		-t ${IMAGENAME} \
		--rm \
		--memory 24g \
		--shm-size 2g \
		.
	touch .build-step
