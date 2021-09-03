(ns insideout.core
  "InsideOut program launcher."
  (:require
   [clojure.java.io             :as io]
   [clojure.string              :as string]
   [clojure.pprint              :as pp]

   [insideout.dynamo            :as dyn]
   [insideout.nrepl             :as nr]
   [util.core                   :as u])
  (:import
   [java.io FileNotFoundException])
  (:gen-class :main true))


(def ^:dynamic *boot-script* nil)


#_(defn- with-comments [tag forms]
  (string/join
   "\n"
   [(format ";; start %s" tag)
    forms
    (format ";; end %s" tag)]))

#_(defn pr-boot-form [form]
  (if (<= @u/*verbosity* 1)
    (pr-str form)
    (let [[op & [msg & more]] form]
      (with-out-str (pp/write form :dispatch pp/code-dispatch)))))

#_(defn emit [boot? argv bootscript inits]
  (let [requires [['insideout.dynamo :as 'dynamo]]
        uses '[clojure.tools.namespace.repl clojure.repl]]
    (str
     (string/join
      "\n\n"
      (remove nil?
              [(pr-boot-form `(ns insideout.user (:require ~@requires) (:use ~@uses)))
               (when inits (with-comments "--init exprs" inits))
               (with-comments "boot script" bootscript)
               (pr-boot-form `(insideout.user/io-main ~@argv))
               #_(pr-boot-form
                  `(let [boot?# ~boot?]
                     (if-not boot?#
                       (when-let [main# (resolve 'insideout.core/start)] (main# ~@argv))
                       (when-let [main# (resolve 'insideout.core/start)] (main# ~@argv))
                       #_(boot ~@(or (seq argv) ["insideout.core/help"])))))]))
     "\n")))

#_(defn shebang? [arg]
  (when (and (<= 0 (.indexOf arg (int \/))) (.exists (io/file arg)))
    (let [bang-line (str (first (string/split (slurp arg) #"\n")))
          full-path (System/getProperty "insideout.app.path")
          base-path (.getName (io/file full-path))
          full-pat  (re-pattern (format "^#!\\s*\\Q%s\\E(?:\\s+.*)?$" full-path))
          base-pat  (re-pattern (format "^#!\\s*/usr/bin/env\\s+\\Q%s\\E(?:\\s+.*)?$" base-path))]
      (or (re-find full-pat bang-line) (re-find base-pat bang-line)))))

(defn parse-cli-opts [args]
  [[] {} args])


(defn default-script []
  (-> "default-script.clj" io/resource slurp))

(defn maybe-embedded-user-script []
  (some-> (u/config :startup-file) io/resource slurp))

#_(defn -main [& args*]
  (let [[arg0 args args*] (if (seq args*)
                            [nil nil args*]
                            ["--help" nil ["--help"]])
        bootscript        (u/config :startup-file)
        exists?           #(when (.isFile (io/file %)) %)
        have-bootscript?  (exists? bootscript)
        [arg0 args]       (cond
                            #_(shebang? arg0)  #_[arg0 args]
                            have-bootscript? [bootscript args*]
                            :else            [nil args*])
        boot?             (contains? #{nil bootscript} arg0)
        [errs opts args]  (if-not boot? [nil {} args] (parse-cli-opts args))
        arg0              (or (:file opts) (if (:no-boot-script opts) nil arg0))
        boot?             (and boot? (not (:file opts)))
        verbosity         (if (:quiet opts)
                            (* -1 @u/*verbosity*)
                            (or (:verbose opts) 0))
        watchers?          (if (:disable-watchers opts)
                             false
                             @u/*watchers?*)]

    (when (seq errs)
      (u/exit-error
       (println (apply str (interpose "\n" errs)))))

    (when (:no-colors opts)
      (reset! u/*colorize?* false))
    (swap! u/*verbosity* + verbosity)
    (reset! u/*watchers?* watchers?)

    (binding [*out*          (u/auto-flush *out*)
              *err*          (u/auto-flush *err*)
              *boot-script*  arg0]

      (u/exit-ok
       (let [bootstr     (or (maybe-embedded-user-script)
                             (some->> arg0 slurp)
                             (default-script))
             scriptstr   (binding [*print-meta* true]
                           (emit boot? args bootstr (:init opts)))]

         (when (:boot-script opts) (u/exit-ok (print scriptstr)))

         (when (:version opts) (u/exit-ok (u/info (u/settings :version))))

         (try (load-string scriptstr)
              (catch clojure.lang.Compiler$CompilerException cx
                (let [l (.-line cx)
                      c (.getCause cx)
                      m (.getMessage (or c cx))
                      x (or c cx)]
                  (throw (ex-info m (merge (sorted-map :line l) (ex-data c)) x))))))))))


;; TODO
;;
;; Find sources via searching `src/main/clojure` and `src`.
;; `app.clj` is the program's main.  Search for it in the
;; source folders, require it, and call its `app-main`.

;; Make a small fast native executable that communicates with
;; dynamo over a unix domain socket (?) for monitoring/operability?



(defn -main
  "public static void main..."
  [& args]
  (dyn/add-source-folders-to-classpath)

  (try
    (require '[insideout.user])
    (when-let [main (some-> 'insideout.user/-main resolve var-get)]
      (apply main (apply into [] args)))

    (System/exit 0)

    (catch FileNotFoundException e
      (println "Error: Namespace `insideout.user` could not be found.")
      (System/exit 1))))
