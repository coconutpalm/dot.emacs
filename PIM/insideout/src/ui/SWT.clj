(ns ui.SWT
  (:require [ui.SWT-deps])
  (:import [org.reflections Reflections]
           [org.reflections.scanners SubTypesScanner]))             ;Requiring the deps loads them

(import '[org.eclipse.swt SWT]
        '[org.eclipse.swt.widgets Display Shell Composite Widget Layout])

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


(defn- run-inits [control inits]
  (doseq [init inits]
    (init control)))

(defmacro composite*
  "Meta-construct the specified SWT class derived from Composite."
  [clazz style inits]
  `(fn [parent#]
     (let [^Composite child# (new ~clazz parent# ~style)]
       (run-inits child# ~inits)
       child#)))

(defn |
  "Combine the specified SWT style bits for the \"style\" constructor parameter.  A synonym
  for `bit-or`."
  [& styles]
  (apply bit-or styles))

(defn shell
  "org.eclipse.swt.widgets.Shell"
  [style & inits]
  (let [sty (if (= style SWT/DEFAULT)
              SWT/SHELL_TRIM
              style)]
    (composite* Shell sty (or inits []))))

(def subtype-index (Reflections. (to-array [(SubTypesScanner.)])))

(def swt-composites (->> (.getSubTypesOf subtype-index Composite)
                       (seq)
                       (remove #(= Shell %))))

(->> swt-composites
   (map (fn [clazz]
          [clazz {:simple-name (.getSimpleName clazz)
                  :kebab-name ()}])))

(def swt-widgets (->> (.getSubTypesOf subtype-index Widget)
                    (seq)
                    (remove #(.isAssignableFrom Composite %))))

(def swt-layouts (-> (.getSubTypesOf subtype-index Layout)
                    (seq)))

(def layout-index (Reflections. (to-array ["org.eclipse.swt.layout" (SubTypesScanner. false)])))

(def swt-layoutdata (->> (.getAllTypes layout-index)
                       (seq)))


;; A wrapped nullary function that captures its result or thrown exception in the
;; `result` and `exception` atoms respectively.  Construct using `wrap-fn`
(defrecord WrapFn [f result exception]
  clojure.lang.IFn
  (invoke [_]
    (try
      (reset! result (f))
      @result
      (catch Throwable t
        (reset! exception t)
        t))))

(defn wrap-fn
  "Returns a WrapFn w wrapping f.  Usage: (w), (:result w), or (:exception w)"
  [f]
  (WrapFn. f (atom nil) (atom nil)))

(defn on-ui-thread?
  "Returns true if executing on the UI thread and false otherwise."
  []
  (let [t (Thread/currentThread)
        dt (.getThread (Display/getDefault))]
    (= t dt)))

(defn with-ui* [f]
  (if (on-ui-thread?)
      (f)
      (let [r (wrap-fn f)]
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
  (.asyncExec (display) f))

(defn- process-event
  "Process a single event as long as Shell s isn't disposed."
  [d s]
  (ui [(.isDisposed s)
       (and (not (.isDisposed s))
            (.readAndDispatch d))]))

(defn process-pending-events!
  "Process events until the event queue is exhausted."
  ([]
   (process-pending-events! (display)))
  ([display]
   (if (on-ui-thread?)
     (while (.readAndDispatch display))
     (ui (while (.readAndDispatch display))))))

(defn event-loop!
  "Run the event loop while the specified shell is not disposed."
  [sh]
  (let [d (display)
        s (cond (instance? Shell sh) sh
                (fn? sh)             (ui (sh d))
                :default             (throw (ex-info (str "Can't make a shell from " sh) {:sh sh})))]

    (ui
     (when-not (.isVisible s)
       (.open s)))

    (loop [[disposed busy] (process-event d s)]
      (when (not busy)
        (ui .sleep d))
      (when (not disposed)
        (recur (process-event d s))))

    (process-pending-events!)))


(comment


  (.readAndDispatch (display))

  (let [s (shell SWT/DEFAULT)]
    (event-loop! s))


  (clojure.core/require
   '[clojure.core :refer :all]
   '[clojure.repl :as repl]
   '[ui.SWT :as swt])

  swt/platform-swt-lib

  (dynamo/import-libs [platform-swt-lib]
                      '[org.eclipse.swt.widgets Display Shell])

  (dynamo/import-libs [platform-swt-lib]
                      '[[org.eclipse.swt.widgets Display Shell]
                        [org.eclipse.swt SWT]])

  ,)
