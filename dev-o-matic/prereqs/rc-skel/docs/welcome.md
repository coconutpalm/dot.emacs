# Welcome to dev-o-matic

When dev-o-matic first starts, sometimes background jobs obscure the initial command prompt.  Just hit *`<enter>`* when the text stops scrolling to see the prompt.  Both **`ssh localhost:2222`** and **Desktop Mode** avoid this concern.

## Graphics application integration

* **From the command line**

On a Mac, when you run graphical tools from the default dev-o-matic shell, they will automatically appear on your host's desktop thanks to XQuartz.  Note that some graphical tools perform better in desktop mode.

On Windows, you will need to `ssh -Y` into your container to run graphical tools from the command line.

* **Desktop mode (using TigerVNC)**
  * If you're on a Mac, type *`desktop`* from the lambda prompt to instantly enter a full desktop development environment in an XQuartz window.
  * You can get even better performance and nicer host integration if you install the latest [TigerVNC client](https://tigervnc.org) for your host.  The Java-based client seems to be most reliable.
    * Use *`localhost:1`* for the connection string and specify "None" for authentication method.  (If a bad actor has console access to your machine, you have much bigger problems on your hands than this.)
  * In desktop mode, your original lambda prompt becomes the system log but you can still get that prompt back by hitting *`<enter>`* inside the window.

* **editors**
  * *`emacs`* A nice operating system that also happens to include a really good text editor (also preconfigured for Clojure and Scala).
  * *`idea`* Intellij Ultimate.  (You'll still need a license to use it after the trial expires.)
  * *`code`* VSCode.  Run `vscode-install-extensions` to configure just about everything you need for front and back-end development.  The list of extensions it installs is found in `~/.devrc/conf/vscode-extensions.lst`.
  * *`vim`* Installed but not preconfigured for anything.  You're welcome.  Pull requests are also welcome. :-D

* **Other software** - Most things you need should be pre-installed, but in the event that something isn't:
  *  `syntaptic` - a graphical application that is helpful when searching for packages
  *  `sudo apt install <thing>`
  *  `brew install <thing>`

## Host integration

  * SSH (port 22) in the container is forwarded to port 2222 on the host.
  * VNC lives on port 5901, which is VNC display 1.  It's also forwarded from the container to the host.
  * The `wsl-scripts` folder contains a number of handy scripts to aid host integration on Windows.  Copy them someplace on your `PATH`.

## Docker and Kubernetes integration

* **Kubernetes**
* `kind create cluster` will turn Docker into a full-blown multi-host Kubernetes cluster.
* `~/bin` inside the container has utilities for managing `microk8s` should you decide to install that.

* **Docker**
  * You'll be controlling your host's docker instance, not running a new Docker inside your Docker container
  * If you add the command *`source ~/bin/devenv-utils`* to your host's *`zsh`* or *`bash`* login scripts, you'll get a number of handy commands to manage your Docker instance.  Read the source for details.

## Configuration

The build scripts copy the following from your Mac or Linux host environment into the Docker container:

* Your username and group memberships.
* `~/.ssh/*`
* `~/.aws` if it exists
* Your Git `user.name` and `user.email`
* The system timezone.

You can edit anything inside *`~/.devrc/conf`*.  Use the *`links`* file there to mount folders from your host home directory into the dev container and when you're inside the container, everything you want to keep needs to live inside one of your mounted host directories.

`~/.devrc/home` is bind-mounted to `/home` inside the container.

## Next steps

* Once you exit this document, you'll be in a Bash prompt inside the *`dev`* Docker container.  *`ls`* will show that key directories from your host computer are also available inside the container.
* Examine ~/bin inside the dev container.  A lot of handy utilities are installed here.
* Open a new terminal window and explore more *`dev`* subcommands.  *`dev help`* will list them.
  * Read the *`bin`* folder inside the original project you cloned on your host.  Adding a new shell script here adds a new `dev` subcommand.
* Examine *`~/.devrc`* both on the host and from inside the container

## Hints

1. Your *`.ssh`* folder is copied into the container when it's built.  Therefore a way to enable passwordless SSH login is to *`ssh-copy-id your.name@localhost`* on your host (with your host's SSH service turned on).  Then *`dev rebuild`* and enjoy passwordless SSH login from then on.

## Press 'q' to continue.

*NOTE*: This file is available as *welcome.md* inside your *~/.devrc/docs* directory.
