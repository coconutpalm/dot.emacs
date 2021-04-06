# TODO

* Process command-line arguments:
  * In config/create-or-read
    * :fusecode-dir *DONE*
    * :offline
  * In launcher/start
    * Add relevent fusion.core options to :tasks for Boot
      * :port / :host
      * :offline

  * Boot pass-through options (Maybe not / not now?)

* Extract common namespaces / functions into a common library
  * (newboot "--command" "line" "--arguments") - factor classpath-isolated Boot into a library function?
  * Config file processing
  * Files
  * Patterns
  * Include common library here via `git submodule add path`?  (For the small number of common things, pushing to Clojars feels heavyweight.)

* Make a classloader for (newboot) capable of operating from memory rather than extracting _bootstrap.jar to a file.

* Figure out how to package as a plugin
  * Boot
  * Leiningen
  * SBT
