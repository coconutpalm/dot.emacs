(remove-ns 'ui.SWT)

(ns ui.SWT
  (:refer-clojure :exclude [list])
  (:require [ui.internal.SWT-deps]
            [ui.internal.docs :as docs]
            [ui.internal.reflectivity :as meta]
            [ui.inits :as i]
            [clj-foundation.patterns :refer [nothing something?]]
            [clj-foundation.data :refer [nothing->identity]])
  (:import [clojure.lang IFn]
           [org.eclipse.swt SWT]
           [org.eclipse.swt.graphics GC]
           [org.eclipse.swt.layout FillLayout]
           [org.eclipse.swt.events TypedEvent]
           [org.eclipse.swt.widgets Display Shell Item]))

;; TODO
;;
;; Event handlers - Instead of proxy nonsense:
;;  (on-widget-selected [event] &forms)
;; Reflectively (.addSelectionListener (proxy [SelectionAdapter] [] (widgetSelected [event] ...
;;
;; Make id! hierarchical.  An id! on a Text inside a Composite with an id winds
;;  up as {:composite-id {:text-id the-text}}
;;
;;  (ap->let the-map & exprs) - Top-level keywords become variables in the let binding


(defn ui-scale!
  "Scale the user interface by `factor`.  Must be called before
  the Display is initialized."
  [factor]
  (let [multiplier (str factor)]
    (System/setProperty "sun.java2d.uiScale" multiplier)
    (System/setProperty "glass.gtk.uiScale" multiplier)))


(def display
  "The default SWT Display object or `nothing`"
  (atom nothing))


(defn with-gc-on
  "Create a graphics context on `drawable`, run `f`, and ensure the `gc` is disposed."
  [drawable f]
  (let [gc (GC. drawable)]
    (try
      (f gc)
      (finally (.dispose gc)))))

(defmacro doto-gc-on
  "Like with-gc-on, but executes `forms` inside a `doto` block on the `gc`."
  [drawable & forms]
  `(with-gc-on ~drawable
     (fn [gc#]
       (doto gc# ~@forms))))


;; =====================================================================================
;; Aaaaaand, here's the API!
;; =====================================================================================

(i/define-inits meta/swt-composites)
(i/define-inits meta/swt-widgets)
(i/define-inits meta/swt-items)


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
;; Hand-coded APIs

(defn |
  "Combine the specified SWT style bits for the \"style\" constructor parameter.  A synonym
  for `bit-or`."
  [& styles]
  (apply bit-or styles))


(defn tray-item
  "Define a system tray icon for the application.  Minimally, the `tray-item` needs
  an `:image` and a `:highlighted-image`.  Many applications will also define a `:menu`."
  [& inits]
  )


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
        (.open sh)
        sh))))


;; =====================================================================================
;; Specialized online docs

(require '[ui.gridlayout :as layout])

(def ^:private documentation
  {:package {:ui.SWT (meta/sorted-publics 'ui.SWT)
             :ui.gridlayout (meta/sorted-publics 'ui.gridlayout)}
   :swt {:SWT {SWT (meta/fields SWT)}
         :composites (meta/fn-names<- (conj meta/swt-composites Shell))
         :widgets (meta/fn-names<- meta/swt-widgets)
         :items (meta/fn-names<- meta/swt-items)
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
        dt (and (something? @display)
                (.getThread @display))]
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
  "Process a single event iff all shells aren't disposed.  Returns a pair
  [shells-disposed? event-queue-not-empty?]"
  [d]
  (let [disposed (empty? (.getShells d))]
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

        (loop [[disposed busy] (process-event d)]
          (when (not busy)
            (.sleep d))
          (when (not disposed)
            (recur (process-event d)))))

      (process-pending-events!))))


;;  Oddly, this throws ClassNotFoundException on Shell.
(defn background
  "Runs `f` in a background thread.  Returns the thread.  Propogates the context classloader to
  the new thread."
  [f]
  (let [cl (insideout.dynamo/dyn-classloader)
        job (fn []
              (.setContextClassLoader (Thread/currentThread) cl)
              (f))
        t (Thread. job)]
    (.start t)
    t))


(defn example-app []
  (ui-scale! 2)

  (application
   (shell "Browser"
          :layout (FillLayout.)

          #_(menu "&File"
                (menu-item "&Open..." (id! :cmd/open-file))
                (menu-item SWT/SEPARATOR)
                (menu-item "&Exit" (id! :cmd/exit)))

          (browser SWT/WEBKIT
                   :url (-> (swtdoc :swt :program 'Program) :result :eclipsedoc)
                   (id! :ui/editor)))

   (main
    (fn [props _]
      (println (:ui/editor @props))))))



(comment
  ;; Doesn't work. :-(
  #_(def t (background example-app))

  (example-app)
  (:editor @state)
  (ui (.dispose @display))

  ,)
