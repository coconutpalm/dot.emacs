(ns insideout.repl-y
  "A repl-y module for InsideOut.  Repl-y has a lot of dependencies but more features
  than stock nrepl."
  (:require [insideout.dynamo :as dyn]))

(dyn/require-dependencies
 [['reply "0.5.1"]]
 ['reply.main])


(defn background-repl-y
  "Start repl-y in a background thread."
  []
  (let [t (Thread.
           (fn [] ((ns-resolve 'reply.main 'launch)
                  {:custom-eval '(do (println "Welcome to inside-out!"))
                   :port 9999})))]
    (.start t)))


(defn start
  "Start repl-y using the current thread."
  []
  ((ns-resolve 'reply.main 'launch)
   {:custom-eval '(do (println "Welcome to inside-out!"))
    :port 9999}))
