# This build step depends on the specified source artifacts
SHELL := /bin/bash

.DEFAULT_GOAL := .build-step

.build-step: Dockerfile base-environment.sh Makefile
	./install.sh
	touch .build-step
