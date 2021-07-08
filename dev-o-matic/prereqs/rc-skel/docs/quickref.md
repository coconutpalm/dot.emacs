# Quick reference

## On the host

**tl;dr:**

* `dev run` \- \(re\)start development environment container\.
* `dev rebuild` -
* `dev help` \- List all dev subcommands
* Examine `~/.devrc`
* Read all `*.md` files it installs and in the source.
* Use the source.

## More hints

* Be sure to keep your work inside a mounted folder inside the container or you will lose it the next time you rebuild the container.
    * Mounts can be defined in the `mounts` file inside `~/.devrc`. You have to rebuild the container to activate new ones.
* `~/bin/devenv-utils` can be sourced in your "\~/.bashrc" or "\~/.zshrc" to add some useful docker utility commands to your shell
* Inside the container
    * `~/bin` is prepopulated with a bunch of useful stuff.
    * `aws-login` does what it sounds like it does (once you have AWS permissions)
* The source
    * Everything is built using `make` to keep rebuilding as efficient as possible.
        * There are two disk images: `${imagename}-base` is XUbuntu and only needs to be rebuilt for security updates; `${imagename}` adds developer goodness and interactive login via ssh/x2go.
    * The `dev` script provides a user-friendly front-end for your `path`.
    * `dev` subcommands are just `subcommand.sh` files inside the `bin` directory.

## Customize

* `~/.devrc/conf`
* Any `bin/<cmd>.sh` file you add to the project tree becomes a subcommand of `dev`.

## Upgrade

This will be automated in the future but for now, exit your container and stop it if it doesn't automatically stop. Then run the following sequence of commands:

``` bash
dev rm -f
dev powerwash
cd <where you checked out this project>
git pull origin master
make veryclean  # Any errors you see here may be safely ignored
make
```

Changes you make inside `~/.devrc` are never overwritten but new configuration files/directories will be added during the upgrade.

All of your home directory environment modifications MUST be in directories mounted from your host computer's home via the `conf/links` configuration file.

Said differently, the only thing you should customize is the configuration inside `~/.devrc/conf`. In particular, `rm -fr ~/.devrc/user` should only force `dev reinstall` and `dev run` to rebuild your user state and user caches from scratch and not remove anything you care to lose.

## Limitations

The shell inside the container is currently hard-coded to BASH (pull requests welcome; the infra to auto-detect BASH vs. ZSH exists but rc files need to be ported).

If the `dev` command doesn't work right after install you may need to open up a new `Terminal` or `iTerm` to update your path.

The default `on-container-start` script badly needs to be refactored.

## What we change on your host computer outside Docker

* Symlink the `dev` command and supporting libraries from your checkout to `~/bin/dev`. If `~/bin` doesn't exist we create it and add it to your path.
* Add the .devrc folder and put all the stateful things there.
