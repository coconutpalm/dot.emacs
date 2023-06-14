(require '[insideout.nrepl :as nrepl-server]
         '[insideout.dynamo :as dynamo]
         '[util.core :as util-core])

(defn io-main
  "Default InsideOut startup"
  [& args]

  (println "== InsideOut default startup ==")
  (println)
  (println "Running `default-script.clj`.  Override this behavior by creating")
  (println (str (util-core/config :startup-file) " in a `src` directory."))
  (println)
  (println "If you create `src/`, `test/`, and `resources/` folders, these will be")
  (println "automatically included in the classpath in the usual way.")
  (println)

  (dynamo/add-source-folders-to-classpath)
  (nrepl-server/start! :cider)

  (println "nrepl started.")

  (loop []
    (Thread/sleep 1000)
    (recur)))
