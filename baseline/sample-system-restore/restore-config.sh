#!/usr/bin/env bash

sudo apt-key add Repo.keys
sudo cp -R apt-sources/* /etc/apt
sudo apt-get update
sudo apt -y install $(cat pkgs_manual.lst)
