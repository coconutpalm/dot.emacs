#!/usr/bin/env bash

PW_SPELLED=`apg | tail -1`
PW=`echo "$PW_SPELLED" | awk '{ print $1 }'`

# account password
echo "$1:$PW" | chpasswd

# Set VNC server password
sudo -u $1 vncserver -xstartup /usr/bin/xterm <<EOF
$PW
$PW
n
EOF
sleep 1
vncserver -kill :1

# But because firewalling you don't actually need the password, so
echo "**************************************************************"
echo "* SSH/VNC access to dev-o-matic:"
echo "*"
echo "*   Username: $1"
echo "*"
echo "*   Host:     All services are mapped to localhost"
echo "*"
echo "*   SSH:      ssh -p 2222 localhost"
echo "*             pw: ${PW_SPELLED}"
echo "*"
echo "*   VNC:      'desktop' from lambda prompt or localhost:1"
echo "*             TigerVNC is the fastest Mac client."
echo "*"
echo "* Type 'dev run' to (re)start.  The first start is slow;"
echo "* it will be fast after that."
echo "*"
echo "* 'dev rebuild' will quick-rebuild the container by default."
echo "*"
echo "* If you want to rebuild all containers from scratch,"
echo "* type 'dev powerwash' before you 'dev rebuild'."
echo "*"
echo "* See the documentation for more details."
echo "*"
echo "**************************************************************"
