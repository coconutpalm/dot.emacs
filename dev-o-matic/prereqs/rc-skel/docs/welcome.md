# Welcome to dev-o-matic

Welcome to your immutable rechargeable batteries-included development environment.

When it gets messed up, rebuild it and keep right on working.

When you get a new computer (or VM), bootstrap a brand-new fully-configured development environment in about an hour.

If you hate your Mac or Windows development experience, install a fully-configured Linux development environment in minutes and keep right on working using the best-in-class development tools available.

## How to manage your environment

The *`dev`* command helps manage your dev-o-matic container.

*  *`dev run`* - (Re)run the current container.
*  *`dev rebuild`* - Rebuild the container.  If *`dev run`* fails, rebuild then re-run.

Your data is bind-mounted into the container from *`~/.devrc`* so you can safely rebuild your container any time.

*  *`dev help`* lists the other commands.

## Graphics integration

### From the command line

On a Mac, when you run graphical tools from the default dev-o-matic shell, they will automatically appear on your host's desktop thanks to XQuartz.  Note that some graphical tools perform better in desktop mode than through XWindows/XQuartz.

On Windows, you will need to `ssh -Y` into your container and run your graphical applications separately from the default dev-o-matic shell.  Then you'll be able to run graphical tools from that command line.  The `wsl-scripts/forward-vnc.cmd` script illustrates how to do this.  (It also shows how to forward VNC from the container back to Windows.)

### Desktop mode

If you're on a Mac, type *`desktop`* from the lambda prompt to instantly enter a full desktop development environment in an XQuartz window.

You can obtain better performance and nicer host integration using the latest [TigerVNC client](https://tigervnc.org).  I have experienced the most reliability using the Java-based client.

Connect to *`localhost:1`*  and specify "None" for authentication method.

* (If a bad actor has console access to your machine, you have much bigger problems on your hands than not having a second layer of authentication to get to the Linux desktop.)

In desktop mode, your original lambda prompt becomes the system log but you can still get that prompt back by hitting *`<enter>`* inside the window.

### Editors

* *`emacs`* A nice operating system that also happens to include a really good text editor (also preconfigured for Clojure and Scala).
* *`idea`* Intellij Ultimate.  (You'll still need a license to use it after the trial expires.)
* *`code`* VSCode.  Run `vscode-install-extensions` to configure just about everything you need for front and back-end development.  The list of extensions it installs is found in *`~/.devrc/conf/vscode-extensions.lst`*.
* *`vim`* Installed but not preconfigured for anything.  You're welcome.  Pull requests are also welcome. :-D

### Other software

Most things you need should be pre-installed, but in the event that something isn't, there's:

*  *`syntaptic`* - a graphical application that is helpful when searching for packages
*  *`sudo apt install <thing>`*
*  *`brew install <thing>`*

## Docker and Kubernetes integration

### Kubernetes

**`kind create cluster`** will turn Docker into a full-blown "multi-host" Kubernetes cluster.

*`~/bin`* inside the container has utilities for managing *`microk8s`* should you decide to install that.

### Docker

You'll be controlling your host's docker instance, not running a new Docker inside your Docker container

If you add the command **`source ~/bin/devenv-utils`** to your host's *`zsh`* or *`bash`* login scripts, you'll get a number of handy commands to manage your Docker instance.  Read the source for details.

## Configuration
### Automatic configuration

The build scripts copy the following from your Mac or Linux host environment into the Docker container:

* Your username and group memberships.
* *`~/.ssh`*
* *`~/.aws`* if it exists
* Your Git *`user.name`* and *`user.email`*
* The system timezone.

### Host integration

* *SSH* (port 22) in the container is forwarded to port 2222 on the host.
*  *VNC* lives on port 5901, which is VNC display 1.  Use SSH to forward it to your main host as illustrated by *`wsl-scripts/forward-vnc.cmd`*
* The *`wsl-scripts`* folder contains a number of handy scripts to aid host integration on Windows.  Copy them someplace on your `PATH`.

### Manual configuration

You can edit anything inside *`~/.devrc/conf`*.  Use the *`links`* file there to mount folders from your host home directory into the dev container and when you're inside the container, everything you want to keep needs to live inside one of your mounted host directories.

*`~/.devrc/home`* is bind-mounted to *`/home`* inside the container.

## Next steps
### Places to explore

1. *`dev-o-matic/wsl-scripts`* has some handy Windows utilities you may want to copy somewhere in your path.
1. Your *`.ssh`* folder is copied into the container when it's built.  Therefore a way to enable passwordless SSH login is to *`ssh-copy-id your.name@localhost`* on your host (with your host's SSH service turned on).  Then *`dev rebuild`* and enjoy passwordless SSH login from then on.
1. Examine *`~/bin`* inside the dev container.  A lot of handy utilities are installed here.
1. Examine *`~/.devrc`* both on the host and from inside the container

### Press 'q' to continue.

* Once you exit this document, you'll be in a Bash prompt inside the *`dev`* Docker container.  *`ls`* will show that key directories from your host computer are also available inside the container.

**NOTE**: This file is available as *`welcome.md`* inside your *`~/.devrc/docs`* directory.
