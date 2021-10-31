# TODO

## Immediate tasks
*  on-xxx  functions - Infer adapter function type from parent type
*  Breadcrumb prop during construction
*  Try/catch everything; use breadcrumb; always construct as much as possible; never let UI exceptions propagate.

## Autorefresh
*  Turn on autorefresh by default inside `src`; Leaning toward `c.t.n.repl/refresh`
    *  Decide if I should use `c.t.n.repl/refresh` or the nextjournal library.
    *  Decide if I will use the DI library from Juxt or add `meta` to a namespace pointing to start/stop functions.
    *  Decide if I should refresh at namespace granularity or if I should use the nextjournal alpha library to refresh at the `var` dependency level.

## Modeled UI
*  Persist UI tree and state when contructed and when modified programmatically or by the user.
*  Keep a(t least one) last-known-good version.  Be able to recreate the UI in a known-good state if a refresh fails.
*  UI Cells - UI controls/layouts that are a function of data.  Reactive.

## Datalog
*  Datascript (or [Doxa](https://github.com/ribelo/doxa/)) in-memory database(s) - Seems like a very reasonable "Database as a sequence of values" implementation and can be swapped with a true database when needed.
*  Prevayalog? - A file is a sequence of EDN objects.  The initial one is a snapshot.  Subsequent ones are elements in the transaction journal.  Ascii/UTF-8 so they can be synchronized via Git.
*  An object gets an ID on creation based on `(System/currentTimeMillis)`, maintains a dead/alive flag, and is indexed by a content-hash.  Deleted objects aren't truly deleted until the next snapshot.
