(ns ui.SWT
  (:refer-clojure :exclude [list])
  (:require [ui.internal.SWT-deps]
            [ui.internal.docs :as docs]
            [ui.internal.reflectivity :as meta]
            [ui.inits :as i]
            [ui.gridlayout]
            [clj-foundation.patterns :refer [nothing]]
            [clj-foundation.data :refer [nothing->identity ->kebab-case]])
  (:import [clojure.lang IFn]
           [org.eclipse.swt SWT]
           [org.eclipse.swt.events TypedEvent]
           [org.eclipse.swt.widgets Display Shell Item]))


(def display
  "The default SWT Display object or `nothing`"
  (atom nothing))

(def toplevel-shell
  "The top-level application shell or `nothing`"
  (atom nothing))

;; Wishlist:
;;
;; derived, reactive (cell) properties
;; Lazy sequences of property values?  (via continuations)

;; TODO!
;;   * Ability to background the Display thread in REPL.
;;   * In REPL, top-level-shell hides; doesn't close?
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
;;
;;   * Use Java reflection to learn parameter types and guard API calls using (convert DestType val)
;;
;; Make id! hierarchical.  An id! on a Text inside a Composite with an id winds
;;  up as {:composite-id {:text-id the-text}}

;;  (ap->let the-map & exprs) - Top-level keywords become variables in the let binding


;; =====================================================================================
;; Reflectively generate the API here

(i/composite-inits)
(i/widget-inits)


;; =====================================================================================
;; Props manipulation

(defn with-props
  "Execute `f` passing `props` and `parent`.  Its purpose is to allow developers to inject or capture
  state using the `props` atom during construction of the user interface."
  [f]
  (fn [props parent]
    (f props parent)))

(def main
  "Define the Application's main function.  By convention, must come after creating the application shell.
  Implemented as a synonym of `with-props`; it provides a way for a developer to communicate the intent
  \"here is where everything starts\"."
  with-props)

(defn id!
  "(swap! props assoc kw parent-control; Names parent-control using kw inside the props."
  [kw]
  (fn [props parent]
    (swap! props assoc kw parent)))


;; =====================================================================================
;; Generate main API

(defn |
  "Combine the specified SWT style bits for the \"style\" constructor parameter.  A synonym
  for `bit-or`."
  [& styles]
  (apply bit-or styles))

(declare process-pending-events!)

(defn open-on-top
  "A kludge to force a shell to open on top of the other windows."
  [sh]
  (.open sh)
  (process-pending-events!)
  (.setVisible sh false)
  (.open sh))

(defn shell
  "org.eclipse.swt.widgets.Shell"
  [& inits]
  (let [[style
         inits] (i/extract-style-from-args inits)
        style   (nothing->identity SWT/SHELL_TRIM style)
        style   (if (= style SWT/DEFAULT)
                  SWT/SHELL_TRIM
                  style)
        init    (i/widget* Shell style (or inits []))]

    (fn [props disp]
      (let [sh (init props disp)]
        (open-on-top sh)
        sh))))


;; =====================================================================================
;; Specialized online docs

(def ^:private documentation
  {:package {:ui.SWT (meta/sorted-publics 'ui.SWT)
             :ui.gridlayout (meta/sorted-publics 'ui.gridlayout)}
   :swt {:SWT {SWT (meta/fields SWT)}
         :composites (meta/fn-names<- (conj meta/swt-composites Shell))
         :widgets (meta/fn-names<- meta/swt-widgets)
         :items (->> (.getSubTypesOf meta/swt-index Item) (seq) (sort-by #(.getSimpleName %)))
         :events (->> (.getSubTypesOf meta/swt-index TypedEvent) (seq) (sort-by #(.getSimpleName %)))
         :listeners (->> (.getSubTypesOf meta/swt-index org.eclipse.swt.internal.SWTEventListener)
                       (filter (fn [clazz] (not (.contains (.getSimpleName clazz) "$"))))
                       (seq)
                       (sort-by #(.getSimpleName %)))
         :graphics (meta/types-in-package "graphics")
         :program (meta/types-in-package "program")
         :layout-managers (meta/layoutdata-by-layout)}})

(defn swtdoc
  "Print documentation on the SWT library support."
  [& query]
  (docs/swtdoc* [] documentation query))


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
;; Event processing must happen on the UI thread.  Some helpers...

(defn on-ui-thread?
  "Returns true if executing on the UI thread and false otherwise."
  []
  (let [t (Thread/currentThread)
        dt (.getThread (Display/getDefault))]
    (= t dt)))


(defn with-ui*
  "Implementation detail: Use `sync-exec!` or `ui` instead.  Public because it's called
  from the `ui` macro."
  [f]
  (if (on-ui-thread?)
    (f)
    (let [r (runnable-fn f)]
      (.syncExec @display r)

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
  (.asyncExec @display (runnable-fn f)))

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
   (process-pending-events! @display))
  ([display]
   (while (.readAndDispatch display))))


;; =====================================================================================
;; The main application and her children

(defn child-of
  "Mount the child specified by child-init-fn inside parent passing initial-props-value inside the props atom.
  Returns a map containing the :child and resulting :props"
  [parent initial-props-value child-init-fn]
  (let [props (atom initial-props-value)]
    {:child (child-init-fn props parent)
     :props @props}))


(defn application
  "Run the event loop while the specified `init` shell-or-fn is not disposed."
  [& more]
  (let [props (atom {})
        d (Display/getDefault)
        i (i/args->inits more)]

    (try
      (reset! display d)

      (let [init-results (i/run-inits props d i)
            maybe-shell  (first (filter #(instance? Shell %) init-results))
            s            (if (instance? Shell maybe-shell)
                           maybe-shell
                           (throw (ex-info "Couldn't make shell from args" {:args more})))]

        (reset! toplevel-shell s)

        (loop [[disposed busy] (process-event d s)]
          (when (not busy)
            (.sleep d))
          (when (not disposed)
            (recur (process-event d s)))))

      (process-pending-events!)

      (catch Throwable t
        (reset! toplevel-shell nothing)
        (throw t))

      (finally
        (reset! toplevel-shell nothing)))))

;;  Oddly, this throws ClassNotFoundException on Shell.
#_(defn background
    "Runs `f` in a background thread.  Returns the thread.  Propogates the context classloader to
  the new thread."
    [f]
    (let [cl (.getContextClassLoader (Thread/currentThread))
          job (fn []
                (.setContextClassLoader (Thread/currentThread) cl)
                (f))
          t   (Thread. job)]
      (.start t)
      t))


(require '[ui.gridlayout :as layout])

(defn example-app []
  (application
   (shell "Example SWT app"
          (layout/grid-layout :numColumns 2 :makeColumnsEqualWidth false)

          (label "A. Label"
                 (layout/align-left))
          (combo SWT/BORDER "Default value"
                 :items ["one" "two" "three" "Default value" "four"]
                 (layout/hgrab))

          (group "Example group"
                 (id! :name)
                 (layout/align-left :horizontalSpan 2)

                 (layout/grid-layout :numColumns 2 :makeColumnsEqualWidth false)
                 (label "A. Label"
                        (layout/align-left))
                 (text SWT/BORDER "Default text"
                       (layout/align-left)
                       (id! :default-text))))

   (main
    (fn [props _]
      (let [t (:default-text @props)]
        ;; Set up event handlers, etc...
        (println t))))))


(comment
  (example-app)
  (ui (.dispose @display))

  ,)
