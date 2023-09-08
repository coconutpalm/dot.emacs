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
        wget \
        git

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

# docker-compose
DCO_VER=$(latest-version https://github.com/docker/compose.git)
curl -L "https://github.com/docker/compose/releases/download/$DCO_VER/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose

# kind
KIND_VER=$(latest-version https://github.com:kubernetes-sigs/kind.git)
curl -Lo /usr/local/bin/kind https://kind.sigs.k8s.io/dl/$KIND_VER/kind-linux-amd64
chmod +x /usr/local/bin/kind

# Kubectl
curl -Lo /usr/local/bin/kubectl "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
chmod +x /usr/local/bin/kubectl

# The latest postgresql
sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -

# google-drive-ocamlfuse
add-apt-repository ppa:alessandro-strada/ppa

# Inkscape
add-apt-repository ppa:inkscape.dev/stable-1.2

# Chrome
wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add -
sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list'

# microsoft-edge-dev
curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /tmp/microsoft.gpg
install -o root -g root -m 644 /tmp/microsoft.gpg /etc/apt/trusted.gpg.d/
sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/edge stable main" > /etc/apt/sources.list.d/microsoft-edge-dev.list'
rm /tmp/microsoft.gpg

# Peek screen recorder
#
# Broken as of 2/6/2023
#
#add-apt-repository ppa:peek-developers/stable

# Syncthing
sudo curl -o /usr/share/keyrings/syncthing-archive-keyring.gpg https://syncthing.net/release-key.gpg
echo "deb [signed-by=/usr/share/keyrings/syncthing-archive-keyring.gpg] https://apt.syncthing.net/ syncthing stable" | sudo tee /etc/apt/sources.list.d/syncthing.list

#
# Now update and install all the extra-packages
#
apt-get update
apt-get install -y $(cat extra-packages.lst)

#
# Ensure Docker's group ID is 2001.  (/etc/group is backed up to '/etc/group.backup' during this operation)
#
sed -i.backup 's/\(^docker:x:\)[0-9]\+/\12001/g' /etc/group

# Manual installs now that dependencies are installed

wget https://downloads.rclone.org/rclone-current-linux-amd64.deb
dpkg -i rclone-current-linux-amd64.deb

# DBeaver database tool
wget https://dbeaver.io/files/dbeaver-ce_latest_amd64.deb
dpkg -i dbeaver-ce_latest_amd64.deb

# code
wget -O code.deb 'https://code.visualstudio.com/sha/download?build=stable&os=linux-deb-x64'
dpkg -i code.deb

# clojure
curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
chmod +x linux-install.sh
sudo ./linux-install.sh

# Slack - Version has to be manually updated
SLACK_VER='4.33.90'  # slack-4.33.90-0.1.el8.x86_64
wget https://downloads.slack-edge.com/releases/linux/${SLACK_VER}/prod/x64/slack-desktop-${SLACK_VER}-amd64.deb
dpkg -i slack-desktop-${SLACK_VER}-amd64.deb


# Gommit - Git pre-commit manager

# Because gommit doesn't follow URL conventions everyone else does
GOMMIT_URL=https://github.com/antham/gommit/releases/download/v2.4.0/gommit_2.4.0_linux_amd64.tar.gz
wget -qO- "$GOMMIT_URL" | tar xvzf - -C /tmp
cp /tmp/gommit /usr/local/bin
chmod ugo+x /usr/local/bin/gommit
