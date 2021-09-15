(remove-ns 'ui.SWT)

(ns ui.SWT
  (:refer-clojure :exclude [list])
  (:require [ui.SWT-deps]
            [clj-foundation.patterns :refer [nothing]]
            [clj-foundation.data :refer [->kebab-case setter nothing->identity]])
  (:import [clojure.lang IFn Keyword Atom Reflector]
           [java.io Closeable]
           [org.reflections Reflections]
           [org.reflections.scanners SubTypesScanner]))             ;Requiring the deps loads them

(import '[org.eclipse.swt SWT]
        '[org.eclipse.swt.layout RowLayout]
        '[org.eclipse.swt.widgets Display Shell Composite Widget Layout
          Tray TaskBar TaskItem ScrollBar]
        '[org.eclipse.swt.opengl GLCanvas])

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


(defn- run-inits
  "Initialize the specified control using the functions in the `inits` seq."
  [control inits]
  (doseq [init inits]
    (init control)))

(defmulti ->init
  "Convert first and second arguments (from front of vararg list) into an init function.  Returns the
  function and the number of arguments consumed from the arglist."
  (fn [arg1 _]
    (class arg1)))

(defmethod ->init
  IFn [arg1 _]
  [arg1 1])

(defmethod ->init
  String [arg1 _]
  (letfn [(set-text-on [control]
            (.setText control arg1))]
    [set-text-on 1]))

(defmethod ->init
  Keyword [arg1 arg2]
  (letfn [(set-property [o]
            (Reflector/invokeInstanceMethod o
             (setter arg1)
             (into-array Object [arg2])))]
    [set-property 2]))

(defn- args->inits
  [args]
  (letfn [(process-args [inits args]
            (let [arg1        (first args)
                  arg2        (second args)
                  [next-init
                   args-used] (->init arg1 arg2)]
              (cond
                (nil? arg2)      (conj inits next-init)
                (= 1 args-used) (process-args (conj inits next-init) (rest args))
                (= 2 args-used) (process-args (conj inits next-init) (rest (rest args))))))]
    (process-args [] args)))

(defn- extract-style-from-args
  [args]
  (let [maybe-style (first args)]
    (if (instance? Integer maybe-style)
      [maybe-style (rest args)]
      [nothing args])))

(defmacro widget*
  "Meta-construct the specified SWT class derived from Widget."
  [^Class clazz style args]
  `(fn [^Composite parent#]
    (let [child# (new ~clazz parent# ~style)
          inits# (args->inits ~args)]
      (run-inits child# inits#)
      child#)))

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


(defn- clazzes->ctors [classes]
  (map (fn [clazz]
         (let [name (.getName clazz)
               doc (str "Construct a " name
                        "\n\nhttps://help.eclipse.org/latest/index.jsp?topic=%2Forg.eclipse.platform.doc.isv%2Freference%2Fapi%2F"
                        (.replaceAll name "\\." "%2F") ".html")
               name-sym (symbol name)
               fn-name (symbol (-> (.getSimpleName clazz) ->kebab-case))]
           `(defn ~fn-name
              ~doc
              [& inits#]
              (let [[style#
                     inits#] (extract-style-from-args inits#)
                    style# (nothing->identity SWT/NULL style#)]
                (widget* ~name-sym style# (or inits# []))))))
       classes))

(def ^:private subtype-index (Reflections. (to-array [(SubTypesScanner.)])))

(def ^:private swt-composites (->> (.getSubTypesOf subtype-index Composite)
                                 (seq)
                                 (remove #{Shell GLCanvas})))

(defmacro ^:private composite-ctors []
    (let [ctors (clazzes->ctors swt-composites)]
      `[~@ctors]))
(composite-ctors)

(def ^:private swt-widgets (->> (.getSubTypesOf subtype-index Widget)
                              (seq)
                              (remove #(.isAssignableFrom Composite %))
                              (remove #{Tray TaskBar TaskItem ScrollBar})))

(defmacro ^:private widget-ctors []
  (let [ctors (clazzes->ctors swt-widgets)]
    `[~@ctors]))
(widget-ctors)

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

(defn with-ui* [f]
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
  "Process a single event iff Shell sh isn't disposed."
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
