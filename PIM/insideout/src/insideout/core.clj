(ns insideout.core
  "InsideOut program launcher."
  (:require
   [clojure.java.io             :as io]
   [clojure.string              :as string]
   [clojure.pprint              :as pp]

   [boot.file                   :as file]
   [boot.util                   :as util]
   [boot.from.clojure.tools.cli :as cli]

   [insideout.tasks             :as tasks]
   [insideout.dynamo            :as dyn]
   [insideout.nrepl             :as nr]
   [ui.SWT                      :as swt]))


(def ^:dynamic *boot-opts* {})
(def ^:dynamic *boot-script* "")

(def cli-opts
  [["-a" "--asset-paths PATH"    "Add PATH to set of asset directories."
    :assoc-fn #(update-in %1 [%2] (fnil conj #{}) %3)]

   ["-b" "--boot-script"         "Print generated script for debugging."]
   ["-B" "--no-boot-script"      "Ignore boot script in current directory."]

   ["-C" "--no-colors"           "Remove ANSI escape codes from printed output."]

   #_["-E" "--exclusions SYM"      "Add the SYM dependency to the set of global exclusions."
      :assoc-fn #(update-in %1 [%2] (fnil conj #{}) (symbol %3))]
   ["-e" "--set-env KEY=VAL"     "Add KEY => VAL to project env map."
    :assoc-fn #(let [[k v] (string/split %3 #"=" 2)]
                 (update-in %1 [%2] (fnil assoc {}) (keyword k) v))]
   ["-i" "--init EXPR"           "Evaluate EXPR in the insideout.user context."
    :assoc-fn #(update-in %1 [%2] (fnil conj []) (read-string %3))]
   [nil  "--disable-watchers"    "Disable registering file watches (inotify/FSEvents) for constrained environments."]

   ["-f" "--file PATH"           "Evaluate PATH (implies -BP). Args and options passed to -main."]

   ["-h" "--help"                "Print basic usage and help info."]
   ["-o" "--offline"             "Don't attempt to access remote repositories." :id :offline?]

   ["-P" "--no-profile"          "Skip loading of profile.boot script."]

   ["-q" "--quiet"               "Suppress output."]

   ["-r" "--resource-paths PATH" "Add PATH to set of resource directories."
    :assoc-fn #(update-in %1 [%2] (fnil conj #{}) %3)]
   ["-s" "--source-paths PATH"   "Add PATH to set of source directories."
    :assoc-fn #(update-in %1 [%2] (fnil conj #{}) %3)]

   ["-v" "--verbose"             "More error info (-vv more verbose, etc.)"
    :assoc-fn (fn [x y _] (update-in x [y] (fnil inc 0)))]

   #_["-x" "--exclude-clojure"     "Add org.clojure/clojure to the set of global exclusions."]])


(defn- parse-cli-opts [args]
  ((juxt :errors :options :arguments)
   (cli/parse-opts args cli-opts :in-order true)))


(defn- with-comments [tag forms]
  (string/join
   "\n"
   [(format ";; start %s" tag)
    forms
    (format ";; end %s" tag)]))

(defn pr-boot-form [form]
  (if (<= @util/*verbosity* 1)
    (pr-str form)
    (let [[op & [msg & more]] form]
      (with-out-str (pp/write form :dispatch pp/code-dispatch)))))

(defn emit [boot? argv localscript bootscript inits]
  (let [boot-use '[boot.core boot.util boot.task.built-in]]
    (str
     (string/join
      "\n\n"
      (remove
       nil?
       [(pr-boot-form `(ns insideout.user (:use ~@boot-use)))
        (when localscript (with-comments "local profile" localscript))
        (when inits (with-comments "--init exprs" inits))
        (with-comments "boot script" bootscript)
        (pr-boot-form
         `(let [boot?# ~boot?]
            (if-not boot?#
              (when-let [main# (resolve 'insideout.user/-main)] (main# ~@argv))
              (tasks/boot ~@(or (seq argv) ["insideout.standard-tasks/help"])))))]))
     "\n")))

(defn shebang? [arg]
  (when (and (<= 0 (.indexOf arg (int \/))) (.exists (io/file arg)))
    (let [bang-line (str (first (string/split (slurp arg) #"\n")))
          full-path (System/getProperty "insideout.app.path")
          base-path (.getName (io/file full-path))
          full-pat  (re-pattern (format "^#!\\s*\\Q%s\\E(?:\\s+.*)?$" full-path))
          base-pat  (re-pattern (format "^#!\\s*/usr/bin/env\\s+\\Q%s\\E(?:\\s+.*)?$" base-path))]
      (or (re-find full-pat bang-line) (re-find base-pat bang-line)))))

(defn parse-bootignore [f]
  (when (.isFile f)
    (->> (string/split (slurp f) #"\n") (remove string/blank?) (map re-pattern) set)))


(defn -main [arg0 & args*]
  (let [[arg0 args args*] (if (seq args*)
                            [arg0 nil args*]
                            ["--help" nil ["--help"]])
        bootscript        (util/config "BOOT_FILE" "startup.clj")
        exists?           #(when (.isFile (io/file %)) %)
        have-bootscript?  (exists? bootscript)
        [arg0 args]       (cond
                            (shebang? arg0)  [arg0 args]
                            have-bootscript? [bootscript args*]
                            :else            [nil args*])
        boot?             (contains? #{nil bootscript} arg0)
        [errs opts args]  (if-not boot? [nil {} args] (parse-cli-opts args))
        opts              (if-let [x (:exclude-clojure opts)]
                            (-> (dissoc opts :exclude-clojure)
                                (update-in [:exclusions] (fnil conj #{}) 'org.clojure/clojure))
                            opts)
        arg0              (or (:file opts) (if (:no-boot-script opts) nil arg0))
        boot?             (and boot? (not (:file opts)))
        verbosity         (if (:quiet opts)
                            (* -1 @util/*verbosity*)
                            (or (:verbose opts) 0))
        watchers?          (if (:disable-watchers opts)
                             false
                             @util/*watchers?*)]

    (when (seq errs)
      (util/exit-error
        (println (apply str (interpose "\n" errs)))))

    (when (:no-colors opts)
      (reset! util/*colorize?* false))

    (swap! util/*verbosity* + verbosity)

    (reset! util/*watchers?* watchers?)

    (binding [*out*               (util/auto-flush *out*)
              *err*               (util/auto-flush *err*)
              *boot-opts*    opts
              *boot-script*  arg0]

      (util/exit-ok
       (let [localscript (exists? (io/file "profile.clj"))
             profile?    (not (:no-profile opts))
             bootstr     (some->> arg0 slurp)
             localstr    (when profile?
                           (some->> localscript slurp))
             initial-env (->> [:source-paths :resource-paths :asset-paths
                             :dependencies :exclusions :checkouts :offline?]
                            (reduce #(if-let [v (opts %2)] (assoc %1 %2 v) %1) {})
                            (merge {} (:set-env opts)))
             scriptstr   (binding [*print-meta* true]
                           (emit boot? args localstr bootstr (:init opts)))]

         (when (:boot-script opts) (util/exit-ok (print scriptstr)))

         (when (:version opts) (util/exit-ok (boot.App/printVersion)))

         (try (load-string scriptstr)
              (catch clojure.lang.Compiler$CompilerException cx
                (let [l (.-line cx)
                      c (.getCause cx)
                      m (.getMessage (or c cx))
                      x (or c cx)]
                  (throw (ex-info m (merge (sorted-map :line l) (ex-data c)) x))))))))))


;; TODO
;;
;; Grab Boot's task framework and -main argument parser.
;; Then `nrepl` becomes a task; `dynamo` (maybe) becomes a task.
;; Programmers can write their own tasks.  `params` is a
;; task for passing command-line arguments?

;; Find sources via searching `src/main/clojure` and `src`.
;; `app.clj` is the program's main.  Search for it in the
;; source folders, require it, and call its `app-main`.

;; Make a small fast native executable that communicates with
;; dynamo over a unix domain socket (?) for monitoring/operability?


#_(defn -main
    "public static void main..."
    [args]
    (dyn/resolve-sources)
    (nr/start!))
