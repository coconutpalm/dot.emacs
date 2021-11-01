(ns insideout.reload
  (:require
   [clojure.java.io              :as io]
   [clojure.tools.namespace.repl :as tns]
   [insideout.dynamo             :refer [require-libs *classpath-dirs*]]
   [clj-foundation.io            :refer [file-details]]))


(require-libs
 [['clojure-watch "LATEST"]]
 [['clojure-watch.core :refer ['start-watch]]])


(defonce clojure-source #{"clj" "cljc" "cljs"})

(defonce file-change (atom {}))
(defonce refreshing (atom false))

(defn filesystem-change [event filename]
  (let [details (file-details (io/file filename))]
    (when (and @refreshing (clojure-source (:extension details)))
      (apply tns/refresh @refreshing))
    (reset! file-change {:event event
                         :file details})))


;; Set of filesystem events
(def filesystem-event? #{:init :create :modify :delete})
(def ^:private end-watch-fn (atom nil))

(defn end-watches
  "Stop watching directories/files and clear the `end-watch-fn` atom.  Has
  no effect if not watching anything."
  []
  (swap! end-watch-fn
         (fn [end-watch]
           (when end-watch (end-watch))
           nil)))


(defn- watch
  "Begin watching files/dirs specified in the `watchinfo` maps.  If already
  watching, ends the current watch and starts a new one."
  [watchinfo]
  (swap! end-watch-fn
         (fn [end-watch]
           (when end-watch (end-watch))
           (start-watch watchinfo))))


(defn watch-classpath-dirs
  "Begin watching directories on the classpath for filesystem changes.  Publish
  changes to the `file-change` atom."
  []
  (when-not @end-watch-fn
    (letfn [(watchinfo [path] {:path path
                               :event-types [:create :modify :delete]
                               ;; :bootstrap init-project
                               :callback filesystem-change
                               :options {:recursive true}})]
      (let [watch-config (map watchinfo *classpath-dirs*)]
        (watch watch-config)))))


(defn start-reload-watcher
  "Begin watching `*classpath-dirs*` for filesystem changes.  Call
  clojure.tools.namespace.repl/refresh whenever a change is detected in a
  Clojure(script) source file.  `options` are the same as for
  clojure.tools.namespace.repl/refresh."
  [& options]
  (end-watches)
  (tns/set-refresh-dirs *classpath-dirs*)
  (reset! refreshing (or options []))
  (watch-classpath-dirs))
