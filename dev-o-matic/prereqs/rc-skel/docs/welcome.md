# Welcome to dev-o-matic

When dev-o-matic first starts, sometimes background jobs obscure the initial command prompt.  Just hit *`<enter>`* when the text stops scrolling to see the prompt.  Both **`ssh localhost:2222`** and **Desktop Mode** avoid this concern.

## Commands you can use right now

When you run graphical tools, they will automatically appear on your host's desktop thanks to XQuartz.  Note that some graphical tools perform better in desktop mode.

* **Desktop mode**
  * Type *`desktop`* from the lambda prompt to instantly enter a full desktop development environment in an XQuartz window.
  * You can get even better performance and nicer host integration if you install the latest TigerVNC client for your host
    * https://tigervnc.org/  Open the dmg, than drag the app into your Applications folder.
    * Use *`localhost:1`* for the connection string
  * In desktop mode, your original lambda prompt becomes the system log but you can still get that prompt back by hitting *`<enter>`* inside the window.
* **editors**
  * *`idea`* Intellij Ultimate.  (You'll still need a license to use it after the trial expires; file a ticket with IT for that.)
  * *`code`* VSCode with JVM development tools (including Scala, of course) preconfigured
  * *`emacs`* A nice operating system that also happens to include a really good text editor (also preconfigured for Scala).
  * *`vim`* Installed but not preconfigured for anything.  You're welcome.  Pull requests are also welcome. :-D
* **`docker`**
  * You'll be controlling your host's docker instance, not running a new Docker inside your Docker container
  * If you add the command *`source ~/bin/devenv-utils`* to your host's *`zsh`* or *`bash`* login scripts, you'll get a number of handy commands to manage your Docker instance.  Read the source for details.
* **`chop`** or Chopshop or Connect or Find and Price Care or FPC
  * The *`chop`* command controls your local dockerized FPC stack.
  * It's preconfigured to run but needs to be *`./install`*d and loaded with data.  See *`~/code/connect-docker-compose/README.md`*.

## Configuration

You can edit anything inside *`~/.devrc/conf`*.  Use the *`links`* file there to mount folders from your host home directory into the dev container and when you're inside the container, everything you want to keep needs to live inside one of your mounted host directories.

You should consider all other directories inside *`~/.devrc`* to be cache; don't store anything valuable there.  If something becomes messed up on your container, you should be able to *`rm -fr`* any or all of those directories and start again.  (The configuration scripts will respect changes you made inside *`~/.devrc/conf`*.)

## Next steps

* Once you exit this document, you'll be in a Bash prompt inside the *`dev`* Docker container.  *`ls`* will show that key directories from your host computer are also available inside the container.
* Examine ~/bin inside the dev container
* Open a new Terminal/iTerm window and explore more *`dev`* subcommands.  *`dev help`* will list them.
  * Read the *`bin`* folder inside the original project you cloned on your host.
* Examine *`~/.devrc`* both on the host and from inside the container

Say some more words about how to navigate, the dev command, graphical login, etc.

## Customizing your environment

* Read ~/.devrc
* *`dev`* subcommands are scripts inside the *`bin`* folder

## Hints

1. Your *`.ssh`* folder is copied into the container when it's built.  Therefore a way to enable passwordless SSH login is to *`ssh-copy-id your.name@localhost`* on your host (with your host's SSH service turned on).  Then *`dev rebuild`* and enjoy passwordless SSH login from then on.

## Press 'q' to continue.

*NOTE*: This file is available as *welcome.md* inside your *~/.devrc/docs* directory.
