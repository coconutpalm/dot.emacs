(ns ui.SWT
  (:refer-clojure :exclude [list])
  (:require [ui.SWT-deps]
            [ui.inits :refer [extract-style-from-args widget* widget-classes->inits]]
            [clj-foundation.data :refer [nothing->identity]])
  (:import [clojure.lang IFn]
           [org.reflections Reflections]
           [org.reflections.scanners SubTypesScanner]
           [org.eclipse.swt SWT]
           [org.eclipse.swt.layout RowLayout]
           [org.eclipse.swt.widgets Display Shell Composite Widget Layout
            Tray TaskBar TaskItem ScrollBar]
           [org.eclipse.swt.opengl GLCanvas]))


(defn display [] (Display/getDefault))

;; We have:
;;
;; Composite/Control
;; Properties
;; xxxListener
;;
;; Want:
;;
;; All the above, plus derived, reactive (cell) properties
;; Lazy sequences of property values?  (via continuations)

;; setups is a f: parent => some-control
;;  where some-control could be the parent or could be a child


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

(def ^:private subtype-index (Reflections. (to-array [(SubTypesScanner.)])))

(def ^:private swt-composites (->> (.getSubTypesOf subtype-index Composite)
                                 (seq)
                                 (remove #{Shell GLCanvas})))

(defmacro ^:private composite-inits []
    (let [inits (widget-classes->inits swt-composites)]
      `[~@inits]))
(composite-inits)

(def ^:private swt-widgets (->> (.getSubTypesOf subtype-index Widget)
                              (seq)
                              (remove #(.isAssignableFrom Composite %))
                              (remove #{Tray TaskBar TaskItem ScrollBar})))

(defmacro ^:private widget-inits []
  (let [inits (widget-classes->inits swt-widgets)]
    `[~@inits]))
(widget-inits)

(def ^:private swt-layouts (-> (.getSubTypesOf subtype-index Layout)
                              (seq)))

(def ^:private layout-index (Reflections. (to-array ["org.eclipse.swt.layout" (SubTypesScanner. false)])))

(def ^:private swt-layoutdata (->> (.getAllTypes layout-index)
                                 (seq)))


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




(comment

  (event-loop!
   (shell "Example SWT app"
          :layout (RowLayout. SWT/VERTICAL)
          (label "A. Label")
          (combo SWT/BORDER "Default text")
          (group "Example group"
                 :layout (RowLayout. SWT/VERTICAL)
                 (label "A. Label")
                 (text SWT/BORDER "Default text"))))

  (ui (.dispose (display)))


  ,)
