#!/usr/bin/env bash
set -e

#
# Build Docker container
#

source ../scripts/environment

set_linux_ip() {
    if [[ "$(uname -a)" =~ "WSL" ]]; then
        echo "Building for WSL2"
        HOST_IP="$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}')"
    else
        echo "Building for regular Linux"
        HOST_IP="$(ip addr | grep 'state UP' -A2 | tail -n1 | awk '{print $2}' | cut -f1  -d'/')"
    fi
}

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    ADDUSER_GROUPS='docker'
    set_linux_ip
elif [[ "$OSTYPE" == "darwin" ]]; then
    echo "Building for MacOS"
    GNAME=`cat /etc/group | grep :$(id -g): | awk -F : '{ print $1 }'`
    GID=`id -g`
    ADDUSER_GROUPS="$GID,$GNAME,docker"
    HOST_IP="$(ifconfig -l | xargs -n1 ipconfig getifaddr)"
else
    echo "$OSTYPE: unsupported platform"
    exit 1
fi

TZ=`readlink /etc/localtime | cut -d'/' -f6,7`   # Note this is a Mac-specific `cut`

cp -R ~/.ssh docker/build

cd docker && docker build --no-cache \
        --build-arg USER="$USER" \
        --build-arg UID="$UID" \
        --build-arg ADDUSER_GROUPS="$ADDUSER_GROUPS" \
        --build-arg TZ="$TZ" \
        --build-arg HOST_IP="$HOST_IP" \
        --rm \
        --memory 24g \
        --shm-size 2g \
        -t ${IMAGENAME} \
    	  . && cd ..

rm -fr docker/build/.ssh
