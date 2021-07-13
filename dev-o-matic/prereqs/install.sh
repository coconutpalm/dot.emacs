#!/usr/bin/env -S bash


echo "Checking prerequisites..."

# If we're on a Linux host
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    # Add code to install Linux host dependencies here
    echo "Updating ~/bin directory"
    rsync -uav --ignore-existing rc-skel/home/_user/bin ~
    export PATH="$PATH:$HOME/bin"

    read -n 1 -r -s -p $'Press enter to continue...\n'
    exit 0
fi

# Ensure XQuartz is available
if [ ! -z "$(defaults read org.macosforge.xquartz.X11 2>&1 | grep 'does not exist')" ]
then
    echo "*** You need to install the XQuartz X-Windows server before continuing"
    echo "    (Opening XQuartz home page in your default web browser)"
    echo
    open 'https://www.xquartz.org/'
    ERROR="yes"
fi

# Ensure Docker is avilable and running
DOCKER=`which docker 2>&1 | grep ^/`
if [ -z "$DOCKER" ]
then
    echo "Docker not found!  Opening a browser to the right place"
    echo
    open 'https://www.docker.com/products/docker-desktop'
    ERROR="yes"
fi

DOCKER_EXEC_ERR=`docker info 2>&1 | grep ERROR`
if [ ! -z "$DOCKER_EXEC_ERR" ]
then
    echo $DOCKER_EXEC_ERR
    ERROR="yes"
fi

[ ! -z "$ERROR" ] && exit 1

echo "...everything looks good."


#
# Configure XQuartz
#
echo "Stopping XQuartz if it's currently running so we can fix some settings"
XPID=`ps aux | grep launchd_startx | grep Xquartz | awk '{ print $2 }'`
[ ! -z "$XPID" ] && kill $XPID
sleep 1


echo "Enabling XQuartz 3d graphics, remote connections, and X-style ALT keys"
echo "...check the prereqs/install.sh script if you want to see details."
# @see https://pawelgrzybek.com/change-macos-user-preferences-via-command-line/
# Hardware-accelerated 3d rendering please...
defaults write org.macosforge.xquartz.X11 enable_iglx -boolean true

# Open the X11 network port so Docker run graphical applications on your Mac's display
# (It will still refuse all connections until you configure a trust policy.)
defaults write org.macosforge.xquartz.X11 nolisten_tcp -boolean false

# So X sees the Mac keyboard as if it were a real Linux keyboard...
defaults write org.macosforge.xquartz.X11 option_sends_alt -boolean true

# If you want to disable XQuartz opening an XTerm whenever it starts, run
# the following:
#   defaults write org.macosforge.xquartz.X11 app_to_run /usr/bin/true

sleep 1

echo "Pre-configuration complete.  Restarting XQuartz."
echo
echo "***********************************************************************"
echo "* If you get tired of XQuartz opening an XTerm every time it launches,"
echo "* run the following from a Terminal/ITerm2 session:"
echo "*"
echo "*   defaults write org.macosforge.xquartz.X11 app_to_run /usr/bin/true"
echo "***********************************************************************"
echo
open -a XQuartz
xhost + 127.0.0.1   # Allow connections from localhost only

read -n 1 -r -s -p $'Press enter to continue...\n'

exit 0
