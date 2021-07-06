(ns insideout.boot
  "Shamelessly ripped-off from `boot.core', except that insideOut maintains
  state using a simple Map instead of Boot's filesystem abstraction."
  (:require
   [clojure.java.io              :as io]
   [clojure.set                  :as set]
   [clojure.walk                 :as walk]
   [clojure.repl                 :as repl]
   [clojure.string               :as string]
   [boot.cli                     :as cli2]
   [boot.util                    :as util]
   [boot.from.io.aviso.exception :as ex]
   [boot.from.clojure.tools.cli  :as cli]
   [boot.from.backtick           :as bt])
  (:import
   [java.util.concurrent LinkedBlockingQueue TimeUnit Semaphore ExecutionException]))

(def ^:dynamic *warnings* (atom 0))
(def ^:dynamic *default-state* {})

;; Defining Tasks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro deftask
  "Define a boot task."
  [sym & forms]
  (let [[heads [bindings & tails]] (split-with (complement vector?) forms)]
    `(do
       (when-let [existing-deftask# (resolve '~sym)]
         (when (= *ns* (-> existing-deftask# meta :ns))
           (let [msg# (delay (format "deftask %s/%s was overridden\n" *ns* '~sym))]
             (boot.util/warn (if (<= @util/*verbosity* 2)
                               @msg#
                               (ex/format-exception (Exception. ^String @msg#)))))))
       (cli2/defclifn ~(vary-meta sym assoc ::task true)
         ~@heads
         ~bindings
         (let [provided# (->> ~'*opts* keys set)
               optspec#  (->> #'~sym meta :arglists first second)
               allowed#  (->> optspec# :keys (map (comp keyword str)) set)
               unknown#  (set/difference provided# allowed#)]
           (when (seq unknown#)
             (util/warn "%s: unknown option(s): %s\n" '~sym (string/join ", " unknown#))))
         ~@tails))))

;; Boot Lifecycle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro cleanup
  "Evaluate body after tasks have been run. This macro is meant to be called
  from inside a task definition, and is provided as a means to shutdown or
  clean up persistent resources created by the task (eg. threads, files, etc.)"
  [& body]
  `(swap! @#'boot.core/cleanup-fns conj (fn [] ~@body)))

(defn- take-subargs [open close [x & xs :as coll]]
  (if (not= x open)
    [nil coll]
    (loop [[x & xs] xs depth 1 ret []]
      (if (not x)
        [ret []]
        (cond (= x open)  (recur xs (inc depth) (conj ret x))
              (= x close) (if (zero? (dec depth))
                            [ret xs]
                            (recur xs (dec depth) (conj ret x)))
              :else       (recur xs depth (conj ret x)))))))

(defn- construct-tasks
  "Given command line arguments (strings), constructs a task pipeline by
  resolving the task vars, calling the task constructors with the arguments
  for that task, and composing them to form the pipeline."
  [argv & {:keys [in-order]}]
  (loop [ret [] [op-str & args :as argv] argv]
    (if-not op-str
      (apply comp (filter fn? ret))
      (case op-str
        "--" (recur ret args)
        "["  (let [[argv remainder] (take-subargs "[" "]" argv)]
               (recur (conj ret (construct-tasks argv :in-order false)) remainder))
        (let [op (-> op-str symbol resolve)]
          (when-not (and op (:boot.core/task (meta op)))
            (throw (IllegalArgumentException. (format "No such task (%s)" op-str))))
          (let [spec   (:argspec (meta op))
                parsed (cli/parse-opts args spec :in-order in-order)]
            (when (seq (:errors parsed))
              (throw (IllegalArgumentException. (string/join "\n" (:errors parsed)))))
            (let [[opts argv] (if-not in-order
                                [args nil]
                                (#'cli2/separate-cli-opts args spec))]
              (recur (conj ret (apply (var-get op) opts)) argv))))))))

(defn- run-tasks
  "Given a task pipeline, builds the initial fileset, sets the initial build
  state, and runs the pipeline."
  [task-stack]
  (binding [*warnings* (atom 0)]
    (let [state *default-state*]
      (task-stack state))))

(defn boot
  "The REPL equivalent to the command line 'boot'. If all arguments are
  strings then they are treated as if they were given on the command line.
  Otherwise they are assumed to evaluate to task middleware."
  [& argv]
  (try @(future ;; see issue #6
          (util/with-let [_ nil]
            (run-tasks
              (cond (every? fn? argv)     (apply comp argv)
                    (every? string? argv) (construct-tasks argv :in-order true)
                    :else (throw (IllegalArgumentException.
                                   "Arguments must be either all strings or all fns"))))))
       (catch ExecutionException e
         (throw (.getCause e)))))

;; Low-Level Tasks, Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-pre-wrap
  "Given a binding and body expressions, constructs a task handler. The body
  expressions are evaluated with the current fileset bound to binding, and the
  result is passed to the next handler in the pipeline. The fileset obtained
  from the next handler is then returned up the handler stack. The body must
  evaluate to a fileset object. Roughly equivalent to:
      (fn [next-handler]
        (fn [binding]
          (next-handler (do ... ...))))
  where ... are the given body expressions."
  [bind & body]
  (let [bind (if (vector? bind) (first bind) bind)]
    `(fn [next-task#]
       (fn [fileset#]
         (let [~bind fileset#
               result# (do ~@body)]
           (next-task# result#))))))

(defmacro with-post-wrap
  "Given a binding and body expressions, constructs a task handler. The next
  handler is called with the current fileset, and the result is bound to
  binding. The body expressions are then evaluated for side effects and the
  bound fileset is returned up the handler stack. Roughly equivalent to:
      (fn [next-handler]
        (fn [fileset]
          (let [binding (next-handler fileset)]
            (do ... ...)
            binding)))
  where ... are the given body expressions."
  [bind & body]
  (let [bind (if (vector? bind) (first bind) bind)]
    `(fn [next-task#]
       (fn [fileset#]
         (let [result# (next-task# fileset#)
               ~bind   result#]
           ~@body
           result#)))))

(defmacro with-pass-thru
  "Given a binding and body expressions, constructs a task handler. The body
  expressions are evaluated for side effects with the current fileset bound
  to binding. The current fileset is then passed to the next handler and the
  result is then returned up the handler stack."
  [bind & body]
  (let [bind (if (vector? bind) (first bind) bind)]
    `(with-pre-wrap [fs#]
       (util/with-let [~bind fs#] ~@body))))

(defmacro fileset-reduce
  "Given a fileset, a function get-files that selects files from the fileset,
  and a number of reducing functions, composes the reductions. The result of
  the previous reduction and the result of get-files applied to it are passed
  through to the next reducing function."
  [fileset get-files & reducers]
  (if-not (seq reducers)
    fileset
    (let [each-reducer #(vector `((juxt identity ~get-files)) `(apply reduce ~%))]
      `(->> ~fileset ~@(mapcat each-reducer reducers)))))

;; Task Configuration Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro replace-task!
  "Given a number of binding form and function pairs, this macro alters the
  root bindings of task vars, replacing their values with the given functions.
  Example:
  (replace-task!
    [r repl] (fn [& xs] (apply r :port 12345 xs))
    [j jar]  (fn [& xs] (apply j :manifest {\"howdy\" \"world\"} xs)))"
  [& replacements]
  `(do ~@(for [[[bind task] expr] (partition 2 replacements)]
           `(alter-var-root (var ~task) (fn [~bind] ~expr)))))

(defmacro disable-task!
  "Disables the given tasks by replacing them with the identity task.
  Example:
      (disable-task! repl jar)"
  [& tasks]
  `(do ~@(for [task tasks]
           `(replace-task! [t# ~task] (fn [& _#] identity)))))

(defmacro task-options!
  "Given a number of task/map-of-curried-arguments pairs, replaces the root
  bindings of the tasks with their curried values. For example:
      (task-options!
        repl {:port     12345}
        jar  {:manifest {:howdy \"world\"}})
  You can update options, too, by providing a function instead of an option
  map. This function will be passed the current option map and its result will
  be used as the new one. For example:
      (task-options!
        repl #(update-in % [:port] (fnil inc 1234))
        jar  #(assoc-in % [:manifest \"ILike\"] \"Turtles\"))"
  [& task-option-pairs]
  `(do ~@(for [[task opts] (partition 2 task-option-pairs)]
           `(let [opt# ~opts
                  var# (var ~task)
                  old# (:task-options (meta var#))
                  new# (if (map? opt#) opt# (opt# old#))
                  arg# (mapcat identity new#)]
              (replace-task! [t# ~task] (fn [& xs#] (apply t# (concat arg# xs#))))
              (alter-meta! var# (fn [x#] (assoc x# :task-options new#)))))
       nil))

;; Task Utility Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro template
  "The syntax-quote (aka quasiquote) reader macro as a normal macro. Provides
  the unquote ~ and unquote-splicing ~@ metacharacters for templating forms
  without performing symbol resolution."
  [form]
  `(bt/template ~form))
