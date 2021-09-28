(ns insideout.repl-y
  "A repl-y module for InsideOut.  Repl-y has a lot of dependencies but more features
  than stock nrepl."
  (:require [insideout.dynamo :as dyn]))

(dyn/require-dependencies
 [['reply "0.5.1"]]
 ['reply.main])

(defn start-repl-y []
  (let [t (Thread.
           (fn [] ((ns-resolve 'reply.main 'launch)
                  {:custom-eval '(do (println "Welcome to this awesome app"))
                   :port 9999})))]
    (.start t)))
