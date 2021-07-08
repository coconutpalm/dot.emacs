# This build step depends on the specified source artifacts

.DEFAULT_GOAL := .build-step
DEPENDENCIES=$(find . -type f | grep -v build-step)

.build-step: ${DEPENDENCIES}
	./install.sh
	touch .build-step


clean:
	rm .build-step
	docker container rm $(docker container ls -a | grep $IMAGENAME | awk '{ print $1 }')
