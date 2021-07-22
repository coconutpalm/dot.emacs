#!/usr/bin/env -S bash -x
set -e

##
## This script runs as root inside the container and creates the base XUbuntu-desktop image
##

# Pass a git repo and it will print the latest revision tag
latest-version() {
    TMPDIR="/tmp/gitrepo-$RANDOM"
	 mkdir -p $TMPDIR
	 cd $TMPDIR
	 git init > /dev/null
	 git remote add origin $1 > /dev/null
	 git fetch --tags origin > /dev/null 2>&1
	 echo "$(git describe --tags `git rev-list --tags --max-count=1`)"
}

# As-of this writing these directories must be manually added
[ ! -d /usr/share/i18n/charmaps ] && mkdir -p /usr/share/i18n/charmaps

# Basic prerequisites for this script and for other tools to install/work
apt-get update
apt-get install -y \
        apt-transport-https \
        apt-utils \
        ca-certificates \
        curl \
        dialog \
        gnupg-agent \
        gpg \
        locales \
        software-properties-common \
        wget

# Here we'll install a desktop environment, then add certificates and package repositories that
# are prerequisites for `extra-packages` we'll add on top.
#
# The extra packages we'll install all live in `extra-packages.lst`; we'll install all of them
# at once at the bottom of this script.

# Install stock XUbuntu (in the future allow customization?)
yes | unminimize
apt-get install -y xubuntu-desktop


# Add Apt repositories for standard tools

# With Docker-ce inside please...
bash -c 'apt-get remove -y docker docker.io containerd runc; exit 0'
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
add-apt-repository \
    "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
       $(lsb_release -cs) \
       stable"

# google-drive-ocamlfuse
add-apt-repository ppa:alessandro-strada/ppa

# Chrome
wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add -
sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list'

# Microsoft Edge
curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /tmp/microsoft.gpg
install -o root -g root -m 644 /tmp/microsoft.gpg /etc/apt/trusted.gpg.d/
sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/edge stable main" > /etc/apt/sources.list.d/microsoft-edge-dev.list'
rm /tmp/microsoft.gpg


# aws-okta is now distributed via packagecloud.io, so we install its repository and...
curl -s https://packagecloud.io/install/repositories/segment/aws-okta/script.deb.sh | sudo bash

# Now update and install all the extra-packages
apt-get update
apt-get install -y $(cat extra-packages.lst)

# Manual installs now that dependencies are installed

wget https://downloads.rclone.org/rclone-current-linux-amd64.deb
dpkg -i rclone-current-linux-amd64.deb

# emacs (because xwidgets isn't included in Ubuntu's stock build)
#
# From https://launchpad.net/~kelleyk/+archive/ubuntu/emacs
add-apt-repository ppa:kelleyk/emacs
apt-get update
apt-get -y install emacs27

# code
wget -O code.deb 'https://code.visualstudio.com/sha/download?build=stable&os=linux-deb-x64'
dpkg -i code.deb

# clojure
CLOJURE_VER=$(latest-version https://github.com/clojure/clojure.git)
curl -O https://download.clojure.org/install/linux-install-1.10.1.716.sh
chmod +x linux-install-1.10.1.716.sh
sudo ./linux-install-1.10.1.716.sh
rm ./linux-install-1.10.1.716.sh

# docker-compose
DCO_VER=$(latest-version https://github.com/docker/compose.git)
curl -L "https://github.com/docker/compose/releases/download/$DCO_VER/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose

# Slack - Version has to be manually updated
SLACK_VER='4.17.0'
wget https://downloads.slack-edge.com/linux_releases/slack-desktop-${SLACK_VER}-amd64.deb
dpkg -i slack-desktop-${SLACK_VER}-amd64.deb
