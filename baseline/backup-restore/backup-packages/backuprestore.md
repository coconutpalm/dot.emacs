# For just manually/auto'd installed packages

Create these two files:

```bash
$ apt-mark showauto > pkgs_auto.lst
$ apt-mark showmanual > pkgs_manual.lst
```

Then we restore the files in the target machine:

```bash
$ sudo apt-mark auto $(cat pkgs_auto.lst)
$ sudo apt-mark manual $(cat pkgs_manual.lst)
```

Or we just use the `pkgs_manual.lst` as input to apt:

```bash
# something like:
$ sudo apt install $(cat pkgs_manual.lst)
```

(Of course one must also backup/restore `/etc/apt/sources.list.d`)

This is the approach I've used most successfully in the past


# Using apt-clone to backup and restore everything


```bash
sudo apt install apt-clone

sudo apt-clone clone /path/to/clone.tar.gz
#or
sudo apt-clone clone path-to/apt-clone-state-ubuntu-$(lsb_release -sr)-$(date +%F).tar.gz


sudo apt-clone restore path-to/apt-clone-state-ubuntu.tar.gz
#or for newer release
sudo apt-clone restore-new-distro path-to/apt-clone-state-ubuntu.tar.gz $(lsb_release -sc)
```

# Using dpkg

```bash
dpkg --get-selections > ~/Package.list
sudo cp -R /etc/apt/sources.list* ~/
sudo apt-key exportall > ~/Repo.keys
rsync --progress /home/`whoami` /path/to/user/profile/backup/here


rsync --progress /path/to/user/profile/backup/here /home/`whoami`
sudo apt-key add ~/Repo.keys
sudo cp -R ~/sources.list* /etc/apt/
sudo apt-get update
sudo apt-get install dselect
sudo dpkg --set-selections < ~/Package.list
sudo apt-get dselect-upgrade -y
```

