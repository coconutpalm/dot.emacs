(ns insideout.nrepl
  "Namespace to dynamically load and init an nRepl in
  the dynamo supervisor or in a dynamo service."
  (:require [insideout.dynamo :as dyn])
  (:import  [java.io File]))


(def ^:dynamic *nrepl-opts*
  {:port 0
   :bind "127.0.0.1"})


(def ^:dynamic *coordinates*
  {:nrepl {:dependencies
           '[[nrepl/nrepl "0.8.3"]]
           :middleware
           []}                          ;Defensive programming

   :cider {:dependencies
           '[[cider/cider-nrepl "0.26.0-SNAPSHOT"]
             [refactor-nrepl "2.5.1"]]
           :middleware
           '[cider.nrepl/cider-middleware
             refactor-nrepl.middleware/wrap-refactor]}

   :cljs {:dependencies
          '[[cider/piggieback "0.4.2"]]
          :middleware
          '[cider.piggieback/wrap-cljs-repl]}

   :reveal {:dependencies
            '[[vlaaad/reveal "1.3.196"]]
            :middleware
            '[vlaaad.reveal.nrepl/middleware]}})

(defn deps [thing] (-> *coordinates* thing :dependencies))
(defn mids [thing] (-> *coordinates* thing :middleware))


(defonce main-server (atom nil))


(dyn/require-dependencies
     (deps :nrepl)
     '[nrepl.server :as nrepl :refer [start-server]])


(defn write-port-file [port filename]
  (if (.exists (File. filename))
    (write-port-file port (str filename "_"))
    (do
      (spit filename (str port))
      filename)))

(defn start! [& extra-modules]
  (let [modules (conj extra-modules)
        sources (mapcat deps modules)
        middleware (mapcat mids (or modules []))]

    (dyn/resolve-libs sources)

    (swap! main-server
           (fn [maybe-server]
             (if maybe-server
               maybe-server
               (let [server   (nrepl/start-server
                               (flatten (vec (assoc *nrepl-opts*
                                                    :handler (apply nrepl/default-handler middleware)))))
                     portfile (write-port-file (:port server) ".nrepl-port")]
                 (.addShutdownHook (Runtime/getRuntime)
                                   (Thread. ^Runnable #(let [nrepl-portfile (File. portfile)]
                                                         (when (.exists nrepl-portfile)
                                                           (.delete (File. portfile))))))
                 (assoc server :portfile portfile)))))))

(defn stop! []
  (swap! main-server
         (fn [maybe-server]
           (and maybe-server
                (.delete (File. (:portfile maybe-server)))
                #_(nrepl/stop-server maybe-server)
                nil))))


#_(defonce nrepl-resolution
    (dynamo/import-dependencies [swt-lib]
                                ['[org.eclipse.swt.widgets Display Shell]
                                 '[org.eclipse.swt SWT]]))
