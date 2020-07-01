My Emacs configuration for easy moving between machines.  Also a collection of setup scripts, fonts, and open-source tools for initializing a Scala/Clojure(script)/Java development environment quickly, repeatably, and easily.

Here's a brief directory describing what's here:

* The `bootstrap` script sets up things that are configured the same across Mac and Linux.
* The `baseline` folder contains things that need to be manually installed.  Some of these things are expected by the Emacs config before it will load:
  * The `mononoki` coding font must be installed via the usual platform-specific way.  (A few other coding fonts are included in addition.)
  * The emacs config expects a `~/.snippets` folder for local yasnippets.
* The `dot.emacs` file is a backup of `~/.emacs`.
* `emacs-init.el` is the main Emacs configuration.  Yeah it's a big long file that I've evolved over ~25years so there's some cruft.  That said, there are comments identifying things.
* `elisp` contains downloaded elisp projects that aren't on (m)elpa, etc.  There aren't many of these any more.

