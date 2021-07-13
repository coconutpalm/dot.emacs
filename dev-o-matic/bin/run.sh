#!/usr/bin/env bash
set -e
source ~/bin/devenv-utils


# Run prestart script if present
HOST_PRESTART= "$DOCKER_DEV_CONFDIR"/pre-interactive-onhost
[ -f "$HOST_PRESTART" ] && "$HOST_PRESTART"


# allow X UI access from localhost
xhost + 127.0.0.1



ensure_dir() {
    dirpath="$1"
    [ ! -d "$dirpath" ] && mkdir -p "$dirpath"
    echo "(exists! $dirpath)"
}

ensure_dir "$DOCKER_DEV_USERHOME"
ensure_dir "$DOCKER_DEV_USERBREW"


ensure_symlink() {
    sourcepath="$1"
    linkpath="$2"
    [ ! -L $linkpath ] && ln -s $sourcepath $linkpath
    echo "(exists! $linkpath)"
}


cleanup_container() {
    if [ ! -z "$(docker container ls | grep $CONTAINERNAME)" ]; then
        docker rm $CONTAINERNAME
    fi
}


strip_comments() {
    FILENAME="$1"
    grep -v '^#.*$' "$FILENAME" | \
    grep -v '^\s*$'
}

debug() {
    echo $1 > /dev/stderr
    echo $1
}


volumes() {
    # Escape '/' chars
    STATEDIR=$(echo $DOCKER_DEV_USERSTATE | sed -e 's/\//\\\//g')

    MOUNT_EXPR_SCRIPT='
      my $f0 = glob($F[0]);
      my $f1 = $F[1];

      $f1 =~ s/\~/\/home\/$ENV{"USER"}/;
      print "--mount type=bind,source=$f0,target=$f1,consistency=delegated\n" if (-d $f0);'

    # Ignore comment lines and empty lines, then expand F[0] and F[1];
    # generate the full "--volume this:that" line if `this` dir exists;
    # then paste the lines together into a single long line
    grep -v '^#.*$' $1 | \
    grep -v '^\s*$' | \
    sed -e "s/\$USERSTATE/$STATEDIR/g" | \
    sed -e "s/\$USER/$USER/g" | \
    perl -lane "$MOUNT_EXPR_SCRIPT" | \
    paste -sd ' ' -
}

link_volumes() {
    for tolink in $(strip_comments $1); do
        source=~/"$tolink"
        target="/tmp/$tolink"

        if [ -e $source ]; then
            symlink="${DOCKER_DEV_USERHOME}/${tolink}"
            [ ! -L "$symlink" ] && ln -s "$target" "$symlink"
            echo "--mount type=bind,source=$source,target=$target,consistency=delegated "
        fi
    done
}

cleanup_container

MOUNTS=$(volumes "$DOCKER_DEV_CONFDIR"/mounts)
LINKS=`link_volumes "$DOCKER_DEV_CONFDIR"/links | paste -sd ' ' -`

echo "Configured bind-mounts: $MOUNTS $LINKS"

# Make sure we can bind-mount .devrc/conf via the link method
ensure_dir "${DOCKER_DEV_USERHOME}/.devrc"
ensure_symlink "/tmp/devrc.rc" "${DOCKER_DEV_USERHOME}/.devrc/conf"
ensure_symlink "/tmp/devrc.docs" "${DOCKER_DEV_USERHOME}/.devrc/docs"


# start a new container
# --priviliged is for Chrome
# --[blah... fuse] stuff is to run AppImages without extracting them; SYS_ADMIN is redundent but there for documentation purposes
docker run -it \
     --privileged \
     --cap-add SYS_ADMIN --cap-add MKNOD --device /dev/fuse:mrw \
     --ulimit nofile=99000:99000 \
     --shm-size=1g \
     --name $CONTAINERNAME \
     --hostname dev \
     --mount type=bind,source=/var/run/docker.sock,target=/var/run/docker.sock,consistency=delegated \
     --mount type=bind,source="$DOCKER_DEV_USERSTATE",target=/home,consistency=delegated \
     --mount type=bind,source="$DOCKER_DEV_CONFDIR",target=/tmp/devrc.rc,consistency=delegated \
     --mount type=bind,source="$DOCKER_DEV_USERDOCS",target=/tmp/devrc.docs,consistency=delegated \
     $MOUNTS $LINKS \
     -p 8900-8909:8900-8909 \
     -p 5901:5901 \
     -p 5902:5902 \
     -p 5903:5903 \
     -p 5904:5904 \
     -p 2222:22 \
     $IMAGENAME:latest

# Clean up the container when it exits.
echo Bye

sleep 1
sudo chgrp $USER /var/run/docker.sock
