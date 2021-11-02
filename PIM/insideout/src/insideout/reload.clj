(ns insideout.reload
  "Call `reload-classpath-dirs` to watch and automatically reload namespaces (and dependencies)
  when they are saved.  A namespace's metadata map will be queried for `:on-pre-reload` and
  `:on-post-reload`.  If present, these keys are expected to name a function to be called
  before or after the namespace is reloaded, respectively.  These may be used to persist and
  restore state as needed."
  {:on-pre-reload 'test
   :on-post-reload 'test}
  (:require
   [clojure.java.io              :as io]
   [insideout.dynamo             :refer [require-libs *classpath-dirs* out-of-date-namespaces]]
   [clj-foundation.io            :refer [file-details]]
   [clj-foundation.errors        :refer [maybe-barf]]
   [clj-foundation.patterns      :refer [let-map]]))

(require-libs
 [['clojure-watch "LATEST"]]
 [['clojure-watch.core :refer ['start-watch]]])


(def ^:dynamic *clojure-source* #{"clj" "cljc" "cljs" "edn" "sql"})

(defonce on-pre-reloaders (atom []))
(defonce on-post-reloaders (atom []))


(defn- fire-ns-event [ns-name fn-name]
  (maybe-barf ["Notifying" ns-name fn-name]
              (when fn-name
                (when-let [ns-event (find-var (symbol (name ns-name) (name fn-name)))]
                  (println (str "Notifying " ns-event))
                  (ns-event)))))

(defn- reload
  "Reload the specified namespaces and fire ns events"
  [ns-symbols]
  (let [resolved-meta (map #(let-map [ns-sym    %
                                      namespace (find-ns ns-sym)
                                      ns-meta   (meta namespace)])
                           ns-symbols)]
    (doseq [{:keys [ns-sym ns-meta]} resolved-meta]
      (fire-ns-event ns-sym (:on-pre-reload ns-meta)))
    (doseq [ns-sym ns-symbols]
      (println (str "Reloading " ns-sym))
      (maybe-barf [:reload ns-sym]
                  (require ns-sym :reload)))
    (doseq [{:keys [ns-sym ns-meta]} resolved-meta]
      (fire-ns-event ns-sym (:on-post-reload ns-meta)))))


(defn- filesystem-change [event filename]
  (let [details (file-details (io/file filename))
        change  {:event event
                 :file-details details}]
    (doseq [f @on-pre-reloaders]
      (f change))
    (when (*clojure-source* (:extension details))
      (reload (out-of-date-namespaces)))
    (doseq [f @on-post-reloaders]
      (f change))))


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


(defn reload-classpath-dirs
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


#_(defmacro realias
  "clojure.tools.namespace.repl/refresh loses namespace aliases in namespaces that are
  marked to not be reloaded.

  This macro generates `alias` statements to restore them."
  []
  (let [realiases (->> *ns*
                     (ns-aliases)
                     (map (fn [[a n]] [a (symbol (.getName n))]))
                     (mapcat (fn [[a n]] `[(ns-unalias *ns* ~a) (alias ~a ~n)])))]
    `(do ~@realiases)))
