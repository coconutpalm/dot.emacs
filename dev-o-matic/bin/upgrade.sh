#!/usr/bin/env bash

set -e
source ~/bin/devenv-utils


echo "This *will* overwrite changes you may have made to your stock environment."
# TODO: Add a "press <return> to continue prompt here"

echo "Upgrading the main dev environment project"
cd "$DOCKER_DEV"
git pull origin master

echo "Upgrading standard/stock scripts and configuration"
rsync -raq --progress --no-group prereqs/rc-skel/. ${RCDIR}


# echo "Upgrading plugins"
# TODO
