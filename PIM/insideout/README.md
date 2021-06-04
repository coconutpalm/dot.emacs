# Working from the InsideOut

Most coding happens from the outside-in.  We scaffold our applications inside a dedicated layer of construction tooling like an ocean liner in a dry dock.  When we're done, we press a button and the ship is launched into the water--and sinks.

So we drag the ship back to our "dry dock", try to understand what went wrong, and try again.

But what if we blow up the edit/compile/test/run/deploy life cycle and instead edit our application live, as it's running, in a local replica of our production environment?

What if we could code *everything* from the inside-out instead of from the outside-in?

This is an experiment in doing just that.

Put differently, it's Emacs for your app.  Just as Emacs is written in itself and can be evolved from within itself as it's running, why not do the same thing for our applications?

## How does it work?

*InsideOut* is a program launcher written in Clojure for the JVM.  When you run it, it runs your application.  These are the initial set of features this launcher will include:

*  Component-like microcontainers with classloader-level dependency isolation.  A message-passing bus for subsystems to communicate.  (With no apologies to Stuart Sierra whose great idea I'm shamelessly stealing.)
*  Runtime dependency resolution.  Dependencies don't live in deps.edn, project.clj, or build.boot.  They are resolved and loaded the first time they are needed, by the component that needs them.

This is the minimal launcher.  To support modern software development, these initial (additional) components will be delivered:

*  nRepl so you can still edit code using your favorite editor and troubleshoot using your favorite workflow.  (Optional?  Dynamically resolve/load?)
*  Automatic code reloading as you work.

And since sometimes you need to deliver a user interface for your app:

*  A SWT native platform user interface component so you can write user interfaces using native platform controls (on Linux, Mac, and Windows).  SWT and its native libraries are dynamically resolved so you don't pay for it if you don't use it.

## Why?

*  Because I want something as fast and low-ceremony to use as Babashka, that scales up and out like an application server, that weighs just few megs as a self-executing uberjar.
*  Because I believe our development environments have become too heavyweight.  With dynamic dependency resolution and an embedded nRepl, build tools become obsolete and all we need then is an editor.

## And the big question...

Will I ever build a dynamically-loaded editor component for *InsideOut* so it could be like Emacs, but written in Clojure?

*  Only with the community's help.  I definitely could see value in a web-based REPL-enabled editor inside my applications...
