(ns insideout.nrepl
  "Namespace to dynamically load and init an nRepl in
  the dynamo supervisor or in a dynamo service."
  (:require [insideout.dynamo :as dyn])
  (:import  [java.io File]))


(defonce main-server (atom nil))


(def ^:dynamic *nrepl-opts*
  {:port 0
   :bind "127.0.0.1"
   :init-ns 'insideout.user})


(def ^:dynamic *coordinates*
  {:nrepl {:dependencies
           '[[nrepl/nrepl "0.8.3"]]
           :requires
           [['nrepl.server :as 'nrepl]
            ['nrepl.middleware :as 'nrepl-mw]
            ['nrepl.middleware.session :as 'nepl-mw-session]]
           :middleware
           []}                          ;Defensive programming

   :cider {:dependencies
           '[[cider/cider-nrepl "0.26.0"]
             [refactor-nrepl "2.5.0"]]
           :requires
           [['cider.nrepl :as 'cider]
            ['refactor-nrepl.middleware :as 'refactor]]
           :middleware
           ['cider.nrepl/cider-middleware
            'refactor-nrepl.middleware/wrap-refactor]}

   :cljs {:dependencies
          '[[cider/piggieback "0.4.2"]]
          :requires
          ['cider.piggieback :as 'piggieback]
          :middleware
          '[cider.piggieback/wrap-cljs-repl]}

   :reveal {:dependencies
            '[[cljfx         "1.7.14"]
              [vlaaad/reveal "1.3.196"]]
            :requires
            ['vlaaad.reveal.nrepl :as 'reveal]
            :middleware
            ['vlaaad.reveal.nrepl/middleware]}})


(defn deps [thing] (-> *coordinates* thing :dependencies))
(defn reqs [thing]
  (let [r (-> *coordinates* thing :requires)]
    (if (sequential? (first r))
      r
      [r])))


;; Dynamically load dependencies when (require)'d
(dyn/require-libs (deps :nrepl)
                  (reqs :nrepl))


(defn- wrap-init-vars
  "Make sure certain vars are set inside the nREPL session.

  In particular this

  - tries to load user.clj (or a different init-ns)
  - sets it as the initial *ns*
  - loads add data_readers.edn on the classpath
  - propagates the top level *data-readers* into the nREPL session

  This code has made its way in various shapes from Leiningen to Boot, and
  finally here. It adds an extra middleware to nREPL. It uses `with-local-vars`
  because Leiningen expects middleware to be vars.

  It uses `with-local-vars` a second time, because nREPL's session works by
  persisting vars across messages."

  ;; https://github.com/technomancy/leiningen/blob/master/src/leiningen/repl.clj
  ;; https://github.com/boot-clj/boot/blob/master/boot/pod/src/boot/repl_server.clj

  [init-ns]
  (with-local-vars
    [wrap-init-vars'
     (fn [handler]
       ;; this needs to be a var, since it's in the nREPL session
       (with-local-vars [init-ns-sentinel nil]
         (fn [{:keys [session] :as msg}]
           (when-not (@session init-ns-sentinel)
             (#'clojure.core/load-data-readers)
             (swap! session assoc
                    init-ns-sentinel      true
                    (var *data-readers*)  (.getRawRoot #'*data-readers*)
                    (var *ns*)            (do
                                            (try
                                              (require init-ns)
                                              (catch java.io.FileNotFoundException e)
                                              (catch Throwable t
                                                (println "Error in init-ns" init-ns)
                                                (.printStackTrace t)))
                                            (create-ns init-ns))))
           (handler msg))))]
    (doto wrap-init-vars'
      ;; set-descriptor! currently nREPL only accepts a var
      (nrepl-mw/set-descriptor!
       {:requires #{#'nrepl.middleware.session/session}
        :expects #{"eval"}})
      (alter-var-root (constantly @wrap-init-vars')))))


(defn mids [thing]
  (letfn [(try-resolve [sym]
            (let [val (some-> `~sym resolve var-get)]
              (when-not val
                (throw (IllegalStateException. (str sym " cannot be resolved to a value"))))
              (if (sequential? val)
                (mapcat try-resolve val)
                [sym])))]
    (mapcat try-resolve (-> *coordinates* thing :middleware))))


(comment
  (def middleware (resolve-modules :cider :reveal))
  (apply nrepl/default-handler middleware)

  (start! :cider)
  (stop!)
  (System/exit 0)
  ,)


(defn write-port-file [port filename]
  (if (.exists (File. filename))
    (write-port-file port (str filename "_"))
    (do
      (spit filename (str port))
      filename)))


(defn resolve-modules [& modules]
  (let [sources (mapcat deps modules)
        requires (mapcat reqs modules)]

    (dyn/require-libs sources requires)
    (mapcat mids modules)))


(defn start! [& modules]
  (let [middleware (conj (apply resolve-modules modules)
                         (wrap-init-vars (:init-ns *nrepl-opts*)))]
    (swap! main-server
           (fn [maybe-server]
             (if maybe-server
               maybe-server
               (let [server   (apply nrepl/start-server
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
                (nrepl/stop-server maybe-server)
                nil))))


#_(defonce nrepl-resolution
    (dynamo/import-dependencies [swt-lib]
                                ['[org.eclipse.swt.widgets Display Shell]
                                 '[org.eclipse.swt SWT]]))
