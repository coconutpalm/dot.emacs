(remove 'ui.SWT)

(ns ui.SWT
  (:refer-clojure :exclude [list])
  (:require [ui.SWT-deps]
            [ui.inits :refer [extract-style-from-args widget* widget-classes->inits eclipsedoc-url]]
            [ui.gridlayout]
            [clojure.string :as str]
            [clj-foundation.interop :refer [array]]
            [clj-foundation.conversions :refer :all]
            [clj-foundation.data :refer [nothing->identity ->kebab-case]])
  (:import [clojure.lang IFn]
           [java.util.function Predicate]
           [java.lang.reflect Modifier Field]
           [org.reflections Reflections ReflectionUtils]
           [org.reflections.scanners SubTypesScanner]
           [org.eclipse.swt SWT]
           [org.eclipse.swt.events TypedEvent]
           [org.eclipse.swt.custom SashFormLayout ScrolledCompositeLayout CTabFolderLayout]
           [org.eclipse.swt.layout RowLayout]
           [org.eclipse.swt.widgets Display TypedListener Shell Composite Widget Layout
            Tray TaskBar TaskItem ScrollBar Item Control]
           [org.eclipse.swt.opengl GLCanvas]))


(defn display [] (Display/getDefault))

;; All the above, plus derived, reactive (cell) properties
;; Lazy sequences of property values?  (via continuations)

;; TODO!
;;   * Make display func ^^above^^ an atom.  Construct explicitly in event-loop! and clear on close.
;;   * An ui.SWT/top-level-shell atom.  Also set and cleared inside event-loop!
;;   * (rename event-loop! -> application ??)
;;
;;   * Ability to background the Display thread in REPL.
;;   * In REPL, top-level-shell hides; doesn't close.
;;
;;   * A way to pass state into/out-of  UI:
;;     - Inits are f: arg-kvs => ( [props parent] => props )
;;     - (id! [kw] (fn [props parent] (swap! props assoc [kw parent])))
;;        o Refactor existing inits to the above signature
;;
;;   * A basic InsideOut Shell:
;;     - F12 toggles design text editor with run mode using StackLayout
;;   * A draggable tabbed view framework using example snippet in Chrome on phone (for "run mode")
;;
;;   * System tray icon when running, plus menu with show/hide toggle; add new..., etc.
;;
;;   * data-form - CompositeTable reimagined using an init to describe the row...
;;   * code-mirror control
;;
;;   * Start putting things in Crux


(defn |
  "Combine the specified SWT style bits for the \"style\" constructor parameter.  A synonym
  for `bit-or`."
  [& styles]
  (apply bit-or styles))

(defn shell
  "org.eclipse.swt.widgets.Shell"
  [& inits]
  (let [[style
         inits] (extract-style-from-args inits)
        style   (nothing->identity SWT/SHELL_TRIM style)
        style   (if (= style SWT/DEFAULT)
                  SWT/SHELL_TRIM
                  style)
        init    (widget* Shell style (or inits []))]

    (fn [disp]
      (let [sh (init disp)]
        (.open sh)
        sh))))

(def ^:private swt-index
  (-> (Reflections. (to-array [(SubTypesScanner.)]))))

(def ^:private swt-composites (->> (.getSubTypesOf swt-index Composite)
                                 (seq)
                                 (remove #{Shell GLCanvas})))

(defmacro ^:private composite-inits []
    (let [inits (widget-classes->inits swt-composites)]
      `[~@inits]))
(composite-inits)

(def ^:private swt-widgets (->> (.getSubTypesOf swt-index Widget)
                              (seq)
                              (remove #(.isAssignableFrom Composite %))
                              (remove #(.isAssignableFrom Item %))
                              (remove #{Control Tray TaskBar TaskItem ScrollBar})))

(defmacro ^:private widget-inits []
  (let [inits (widget-classes->inits swt-widgets)]
    `[~@inits]))
(widget-inits)

(def ^:private swt-layouts (->> (.getSubTypesOf swt-index Layout)
                              (seq)
                              (remove #{SashFormLayout ScrolledCompositeLayout CTabFolderLayout})))

(defn- types-in-package [swt-package]
  (->> (Reflections. (array [Object]
                          (str "org.eclipse.swt." swt-package)
                          (SubTypesScanner. false)))
     (.getAllTypes)
     (seq)
     (sort)
     (map #(Class/forName %))))

(def ^:private swt-layoutdata (types-in-package "layout"))


;; =====================================================================================
;; Generate online docs from class metadata

(defn- layoutdata-by-layout []
  (letfn [(layout-type [clazz]
            (-> (.getSimpleName clazz)
               ->kebab-case
               (.split "\\-")
               first))]
    (reduce (fn [cur layout-class]
              (let [key (layout-type layout-class)
                    layoutdata (filter #(= (layout-type %) key) swt-layoutdata)]
                (conj cur [layout-class layoutdata])))
            {}
            swt-layouts)))

(defn- fn-names<- [classes]
  (letfn [(fn-name<- [clazz]
            (-> (.getSimpleName clazz) ->kebab-case))]
    (sort-by first (map (fn [c] [(fn-name<- c) c]) classes))))

#_(defn- getters [^Class clazz]
  (ReflectionUtils/getAllMethods clazz (-> (ReflectionUtils/withModifier Modifier/PUBLIC)
                                           (.and (ReflectionUtils/withPrefix "get"))
                                           (.and (ReflectionUtils/withParametersCount 0)))))

(defn- extract-java-meta [xs]
  (->> xs
     (map (fn [x] [(symbol (str (.getName x)
                               (if (instance? Field x)
                                 ""
                                 (str "("
                                      (str/join ", " (map #(symbol (.getSimpleName %)) (.getParameterTypes x)))
                                      ")"))))
                  {:type (if (instance? Field x) (.getType x) (.getReturnType x))
                   :declaring-class (.getDeclaringClass x)}]))
     (sort-by first)))


(defn- fields [^Class clazz]
  (->> (.getFields clazz)
     (filter (fn [field] (not= 0 (bit-and (.getModifiers field) Modifier/PUBLIC))))
     (extract-java-meta)))

(defn- setters [^Class clazz]
  (->> (.getMethods clazz)
     (filter (fn [method] (and (not= 0 (bit-and (.getModifiers method) Modifier/PUBLIC))
                              (.startsWith (.getName method) "set"))))
     (extract-java-meta)))

(defn- non-prop-methods [^Class clazz]
  (->> (.getMethods clazz)
     (filter (fn [method] (and (not= 0 (bit-and (.getModifiers method) Modifier/PUBLIC))
                              (not= Object (.getDeclaringClass method))
                              (not (.startsWith (.getName method) "get"))
                              (not (.startsWith (.getName method) "set")))))
     (extract-java-meta)))

(defn sorted-publics
  "Like `ns-publics` but returns the results sorted by symbol name."
  [ns]
  (if (string? ns)
    (sorted-publics (symbol ns))
    (->> (ns-publics ns) vec (sort-by first))))

(def ^:private documentation
  {:package {:ui.SWT (sorted-publics 'ui.SWT)
             :ui.gridlayout (sorted-publics 'ui.gridlayout)}
   :swt {:SWT {SWT (fields SWT)}
         :composites (fn-names<- (conj swt-composites Shell))
         :widgets (fn-names<- swt-widgets)
         :items (->> (.getSubTypesOf swt-index Item) (seq) (sort-by #(.getSimpleName %)))
         :events (->> (.getSubTypesOf swt-index TypedEvent) (seq) (sort-by #(.getSimpleName %)))
         :listeners (->> (.getSubTypesOf swt-index org.eclipse.swt.internal.SWTEventListener)
                       (filter (fn [clazz] (not (.contains (.getSimpleName clazz) "$"))))
                       (seq)
                       (sort-by #(.getSimpleName %)))
         :graphics (types-in-package "graphics")
         :program (types-in-package "program")
         :layout-managers (layoutdata-by-layout)}})


(defn swtdoc
  "Print documentation on the SWT library support."
  [& query]
  (letfn [(doc-for-node [node]
            (cond
              (class? node)                         {:class node
                                                     :fields (fields node)
                                                     :properties (setters node)
                                                     :methods (non-prop-methods node)
                                                     :eclipsedoc (eclipsedoc-url node)}
              (and (map? node)
                   (not (empty? node))
                   (keyword? (first (first node)))) {:subtopics (sort (keys node))}
              (var? node)                           (or (:doc (meta node)) node)
              :else                                 node))

          (name-str [x] (name (convert clojure.lang.Named x)))

          (traverse [current-doc topic]
            (cond
              (map? current-doc)        (get current-doc topic)
              (sequential? current-doc) (if (sequential? (first current-doc))
                                          (second (first (filter (fn [x] (>= (.indexOf (name-str (first x)) (name-str topic)) 0)) current-doc)))
                                          (first (filter (fn [x] (>= (.indexOf (name-str x) (name-str topic)) 0)) current-doc)))
              :default                  nil))

          (swtdoc* [breadcrumb current-doc query]
            (if-let [topic (first query)]
              (if-let [next-doc (traverse current-doc topic)]
                (swtdoc* (conj breadcrumb topic) next-doc (rest query))
                (ex-info (str "Couldn't find documentation node: " topic) {:breadcrumb breadcrumb
                                                                           :next-topic topic
                                                                           :rest-of-query (rest query)}))
              {:breadcrumb breadcrumb
               :result (doc-for-node current-doc)}))]

    (swtdoc* [] documentation query)))



;; =====================================================================================
;; A wrapped nullary function that captures its result or thrown exception in the
;; `result` and `exception` atoms respectively.  Construct using `runnable-fn`

(defrecord RunnableFn [f result exception]
  IFn Runnable
  (run [this] (.invoke this))

  (invoke [_]
    (try
      (reset! result (f))
      @result
      (catch Throwable t
        (reset! exception t)
        t))))

(defn runnable-fn
  "Returns a RunnableFn w wrapping f.  Usage: (w), (:result w), or (:exception w)"
  [f]
  (RunnableFn. f (atom nil) (atom nil)))

;; =====================================================================================

(defn on-ui-thread?
  "Returns true if executing on the UI thread and false otherwise."
  []
  (let [t (Thread/currentThread)
        dt (.getThread (Display/getDefault))]
    (= t dt)))


(defn with-ui*
  "Implementation detail: Use `sync-exec!` or `ui` instead."
  [f]
  (if (on-ui-thread?)
    (f)
    (let [r (runnable-fn f)]
      (.syncExec (display) r)

      (when @(:exception r)
        (throw @(:exception r)))
      @(:result r))))

(defmacro ui
  "Run the specified code on the UI thread and return its results or rethrow exceptions."
  [& more]
  (cond
    (coll? (first more)) `(with-ui* (fn [] ~@more))
    :else                `(with-ui* (fn [] (~@more)))))


(defn sync-exec!
  "Synonym for `Display.getDefault().syncExec( () -> f() );` except that the result of executing
  `f()` is captured/returned and any uncaught exception thrown by `f` is rethrown."
  [f]
  (with-ui* f))

(defn async-exec!
  "Synonym for Display.getDefault().asyncExec( () -> f() ); "
  [f]
  (.asyncExec (display) (runnable-fn f)))

(defn- process-event
  "Process a single event iff Shell sh isn't disposed.  Returns a pair
  [shell-disposed? event-queue-not-empty?]"
  [d sh]
  (let [disposed (.isDisposed sh)]
    [disposed
     (and (not disposed)
          (.readAndDispatch d))]))

(defn process-pending-events!
  "Process events until the event queue is exhausted."
  ([]
   (process-pending-events! (display)))
  ([display]
   (while (.readAndDispatch display))))


(defn event-loop!
  "Run the event loop while the specified `init` shell-or-fn is not disposed."
  [init]
  (let [d (display)
        s (cond (instance? Shell init)  init
                (fn? init)              (init d)
                :default                (throw (ex-info (str "Can't make a shell from " init) {:shell-or-init init})))]

    (loop [[disposed busy] (process-event d s)]
      (when (not busy)
        (.sleep d))
      (when (not disposed)
        (recur (process-event d s))))

    (process-pending-events!)))


(defn rowlayout-example []
  (event-loop!
   (shell "Example SWT app"
          :layout (RowLayout. SWT/VERTICAL)
          (label "A. Label")
          (combo SWT/BORDER "Default text")
          (group "Example group"
                 :layout (RowLayout. SWT/VERTICAL)
                 (label "A. Label")
                 (text SWT/BORDER "Default text")))))

(comment



  (ui (.dispose (display)))


  ,)
