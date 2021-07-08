# dev-o-matic - a portable dev container

**`make`**

The result is:

* A beautiful, extensible, reusable, shareable developer environment.
* It gives your host computer new superpowers; it doesn't replace your host computer.
    * `dev run` launches a lambda BASH prompt with development tools preconfigured
    * All configuration is scripted so if your environment gets messed up you can easily revert to a known-good state *(i.e.: repeatable builds for dev environments!)*
* Batteries included.
* Your own nonstandard things are easy to add too.
* The obligatory screenshot:

This is currently beta quality. It works for the author and others are invited to use/contribute/PR, etc.

# Install and run

* `git clone` this repository somewhere you keep code
* `cd <clone-dir>`
* `make`
    * It will be slow the first time and when you rebuild from scratch to install upstream security patches
    * Be sure to note your initial SSH password when it finishes
* `dev run`
    * When it's done with first-time configuration it will display a welcome screen with next steps.
* See also `dev help`

## Design choices

* You use the same credentials on your host computer and inside Docker.
    * Your username, groups, SSH credentials, and even your timezone are automatically built into your dev container.
* Your home directory in the container persists inside your host computer account and is accessible in both places all the time.
* Important directories like \~/code, \~/.ivy2, etc. are literally the same files inside and outside your dev environment.
* After the initial build and run, rebuilding your container with customizations should take just a few seconds.
* It's straightforward to blow away all caches and rebuild from scratch. Repeatable builds FTW.
