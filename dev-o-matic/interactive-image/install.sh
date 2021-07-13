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
    set_linux_ip
    TZ=`readlink /etc/localtime | cut -d'/' -f5,6`   # Linux-specific `cut`
    ADDUSER_GROUPS='docker'
    docker/skel/bin/maximize-open-files
elif [[ "$OSTYPE" == "darwin" ]]; then
    echo "Building for MacOS"
    HOST_IP="$(ifconfig -l | xargs -n1 ipconfig getifaddr)"
    TZ=`readlink /etc/localtime | cut -d'/' -f6,7`   # Mac-specific `cut`
    GNAME=`cat /etc/group | grep :$(id -g): | awk -F : '{ print $1 }'`
    GID=`id -g`
    ADDUSER_GROUPS="$GID,$GNAME,docker"
else
    echo "$OSTYPE: unsupported platform"
    exit 1
fi

cp -R ~/.ssh docker/build

cd docker && docker build --no-cache \
        --build-arg USER_NAME="$(git config --get user.name)" \
        --build-arg USER_EMAIL="$(git config --get user.email)" \
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
