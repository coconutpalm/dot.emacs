(ns boot.main
  (:require
    [clojure.java.io             :as io]
    [clojure.string              :as string]
    [clojure.pprint              :as pp]
    [boot.core              :as core]
    [boot.file                   :as file]
    [boot.util                   :as util]
    [boot.from.clojure.tools.cli :as cli]))

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

(defn emit [boot? argv userscript localscript bootscript import-ns inits]
  (let [boot-use '[boot.core boot.util boot.task.built-in]]
    (str
     (string/join
      "\n\n"
      (remove
       nil?
       [(pr-boot-form `(ns insideout.user (:use ~@import-ns)))
        (when userscript (with-comments "global profile" userscript))
        (when localscript (with-comments "local profile" localscript))
        (when inits (with-comments "--init exprs" inits))
        (with-comments "boot script" bootscript)
        (pr-boot-form
         `(let [boot?# ~boot?]
            (if-not boot?#
              (when-let [main# (resolve 'boot.user/-main)] (main# ~@argv))
              (core/boot ~@(or (seq argv) ["boot.task.built-in/help"])))))]))
     "\n")))

(defn shebang? [arg]
  (when (and (<= 0 (.indexOf arg (int \/))) (.exists (io/file arg)))
    (let [bang-line (str (first (string/split (slurp arg) #"\n")))
          full-path (System/getProperty "boot.app.path")
          base-path (.getName (io/file full-path))
          full-pat  (re-pattern (format "^#!\\s*\\Q%s\\E(?:\\s+.*)?$" full-path))
          base-pat  (re-pattern (format "^#!\\s*/usr/bin/env\\s+\\Q%s\\E(?:\\s+.*)?$" base-path))]
      (or (re-find full-pat bang-line) (re-find base-pat bang-line)))))

(defn parse-bootignore [f]
  (when (.isFile f)
    (->> (string/split (slurp f) #"\n") (remove string/blank?) (map re-pattern) set)))

(defn -main [arg0 & args*]
  (let [[arg0 args args*] (if (seq args*)
                            [arg0 [] args*]
                            ["--help" nil ["--help"]])
        bootscript        (App/config "BOOT_FILE" "startup.clj")
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
              core/*boot-opts*    opts
              core/*boot-script*  arg0]

      (util/exit-ok
       (let [userscript (exists? (io/file (App/getBootDir) "profile.clj"))
             localscript (exists? (io/file "profile.clj"))
             profile?    (not (:no-profile opts))
             bootstr     (some->> arg0 slurp)
             userstr     (when profile?
                           (some->> userscript slurp))
             localstr    (when profile?
                           (some->> localscript slurp))
             initial-env (->> [:source-paths :resource-paths :asset-paths
                             :dependencies :exclusions :checkouts :offline?]
                            (reduce #(if-let [v (opts %2)] (assoc %1 %2 v) %1) {})
                            (merge {} (:set-env opts)))
             import-ns   (export-task-namespaces initial-env)
             scriptstr   (binding [*print-meta* true]
                           (emit boot? args userstr localstr bootstr import-ns (:init opts)))]

         (when (:boot-script opts) (util/exit-ok (print scriptstr)))

         (when (:version opts) (util/exit-ok (boot.App/printVersion)))

         (reset! core/bootignore (parse-bootignore (io/file ".bootignore")))

         (#'core/init!)

         (apply core/set-env! (->> initial-env (mapcat identity) seq))
         (reset! @#'core/cli-base initial-env)
         (try (load-string scriptstr)
              (catch clojure.lang.Compiler$CompilerException cx
                (let [l (.-line cx)
                      c (.getCause cx)
                      m (.getMessage (or c cx))
                      x (or c cx)]
                  (throw (ex-info m (merge (sorted-map :line l) (ex-data c)) x))))))))))
