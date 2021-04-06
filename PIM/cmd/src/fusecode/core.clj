(ns fusecode.core
  "Where everything install and command-line related starts."
  (:require [clojure.tools.logging  :as log]
            [clojure.spec.alpha     :as s]
            [orchestra.core         :refer [defn-spec]]
            [orchestra.spec.test    :as st]
            [clojure.tools.cli      :refer [parse-opts]]
            [clojure.string         :as str]

            [fusecode.config          :as config]
            [fusecode.launch          :as launcher])
  (:import  [java.net InetAddress])
  (:gen-class))


(def cli-options
  [["-p" "--port         PORT" "Port.  (precedence= [command-line-port# config-file-port# default-port#])"
    :default 7000
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]

   ["-s" "--server       HOSTNAME" "Server host if connecting to an existing remote fusecode server."
    :default (InetAddress/getByName "localhost")
    :default-desc "localhost"
    :parse-fn #(InetAddress/getByName %)]

   ["-n" "--new-window" "Open a new browser window connected to the specified host and port."]

   ["-f" "--fusecode-dir PATH" "Override default FuseCode plugin repository directory.  Ignored if connecting."
    :default "~/.fusecode"
    :parse-fn identity]

   ["-t" "--tasks        ARG-STRING" "Tasks/args for the (boot <task> :arg val ...) call.  Ignored if connecting."
    :default "web-dev"
    :parse-fn identity]

   ["-o" "--offline" "Use whatever is cached locally and don't ask the Internet for anything. Ignored if connecting."]

   ["-h" "--help"]])


(s/def ::string-seq (s/* string?))


(defn-spec usage string? [options-summary string?]
  (->> ["Fuse coding, running, and debugging into a single conherent whole.  https://github.com/fuse-code"
        ""
        "Usage: fuse [options] [file1 [file2 ...]]"
        ""
        "Short, long options  Argument    Default      Description"
        options-summary]
       (str/join \newline)))


(defn-spec error-message string? [errors ::string-seq] (str/join \newline errors))


(defn exit [status message]
  (println message)
  (System/exit status))


;; Specify the result map more precisely?
(defn-spec parse-args map? [args ::string-seq]
  (let [parse-result                               (parse-opts args cli-options)
        {:keys [options arguments errors summary]} parse-result]
    (cond
      (:help options) {:exit-now (usage summary) :ok? true}
      errors          {:exit-now (error-message errors)}
      :else           parse-result)))


(defn -main
  "Fusecode Editor main method (for command-line execution)"
  [& args]

  (st/instrument)                       ; Enable type checking of type-annotated functions

  (let [what-to-do (parse-args args)]
    (when-let [failure-message (:exit-now what-to-do)]
      (exit 1 failure-message))

    (let [options (:options what-to-do)
          files-to-open (or (:arguments what-to-do) [])]
      (log/info "Preparing to fuse code")

      (let [{:keys [error-string attached-opened?]} (launcher/attach-open options files-to-open)]
        (cond
          error-string     (exit 1 error-string)
          attached-opened? (exit 0 ""))

        (log/info "No running server; launching a new one")
        (config/create-or-read {:file-path (:fusecode-dir options)})
        (launcher/start options files-to-open)))))
