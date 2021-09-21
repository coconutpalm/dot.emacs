#!/usr/bin/env -S bash -x

# Info about the user we're logging in
USER_LOGINNAME="$1"
USER_UID="$(su -c 'id -u' $USER_LOGINNAME)"
export USER_NAME="$3"
export USER_EMAIL="$4"

cd /home/$USER_LOGINNAME

# Allow plenty of open files
bin/maximize-open-files

# Run init scripts
/etc/init.d/postgresql start
/etc/init.d/rabbitmq-server start

# Run the system service daemon (lighter-weight than systemd)
runsvdir -P /etc/service/ &

# Ensure everyone in the `docker` group has access to Docker
chgrp docker /var/run/docker.sock
chmod g+w /var/run/docker.sock

# Make sure there's a dbus directory for the user
DBUS_DIR=/run/user/$USER_UID/dbus-1
if [ ! -d $DBUS_DIR ]; then
    mkdir -p $DBUS_DIR
    chmod -R 700 $DBUS_DIR
    chown -R "${USER_LOGINNAME:?}:" $DBUS_DIR
fi

# Run the user's on-container-start
CONTAINER_START_SCRIPT="${HOME}/.devrc/conf/on-container-start"
[ -f "$CONTAINER_START_SCRIPT" ] && su - "$1" -c "$CONTAINER_START_SCRIPT $USER_NAME $USER_EMAIL"

echo "Choose a password for your container:"
sudo passwd $USER_LOGINNAME

# Pass control to user's login shell
exec -l su - "$USER_LOGINNAME"
