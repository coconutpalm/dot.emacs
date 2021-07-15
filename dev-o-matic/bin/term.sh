#!/usr/bin/env -S bash
set -e
source ~/bin/devenv-utils

xhost + 127.0.0.1 > /dev/null 2>&1
ssh -q -Y -p 2222 localhost mlterm &
