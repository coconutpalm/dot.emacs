# CLI Construction Kit

This is a set of scripts and a method for creating nontrivial CLI interfaces with a master command and a hierarchical set of subcommands.

## Features:

* Command tree may be any depth.
* Automatic, built-in help system that maps the command tree and can format/display Markdown-based documentation (pretty-printed if Pandoc is installed)
* Automatic completion for command hierarchy; extensible interface for command option completion.
* Tree nodes may have status information associated with them that is automatically printed in the help interface.

## HOWTO:

* Copy this directory and its contents to a new directory where you want to install your new command.  Add that directory to your `PATH`.
* Edit the `environment` file to add common environment variables.
* Rename `cmd` to the name of your base command.  For example, Git's base command is `git`.
* Create directories corresponding to subcommands.
* Create `leaf.sh` scripts corresponding to each leaf command in the command tree in the directory corresponding to where the leaf command should live.
