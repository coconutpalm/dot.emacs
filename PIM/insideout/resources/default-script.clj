;; Default InsideOut startup script

(require '[insideout.nrepl :as nrepl-server]
         '[insideout.dynamo :as dynamo]
         '[util.core :as util-core])

(println "== InsideOut default startup ==")
(println)
(println "Running `default-script.clj`.  Override this behavior by creating")
(println (str (util-core/config :startup-file) " in the current directory."))
(println)
(println "If you create `src/`, `test/`, and `resources` folders, these will be")
(println "automatically included in the classpath in the usual way.")
(println)
(println "Starting nrepl...")

(dynamo/resolve-sources)
(nrepl-server/start! :cider)

(loop []
  (Thread/sleep 1000)
  (recur))
