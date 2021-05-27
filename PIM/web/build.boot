;;
;; Requires Java 11
;;
(set-env!
 :dependencies '[[nrepl                     "0.8.3" :scope "test"]
                 [adzerk/boot-cljs          "2.1.5" :scope "test"]
                 [adzerk/boot-cljs-repl     "0.4.0" :scope "test"]
                 [adzerk/boot-reload        "0.6.0" :scope "test"]
                 [deraen/boot-sass          "0.5.3" :scope "test"]
                 [cider/piggieback          "0.5.2" :scope "test"]
                 [weasel                    "0.7.1" :scope "test" :exclusions [org.clojure/clojurescript]]
                 [org.slf4j/slf4j-nop       "1.7.13" :scope "test"]

                 ;; Both front/back-end
                 [org.clojure/core.async    "1.3.610"]
                 [juji/editscript           "0.5.4"]

                 ;; Front-end
                 [org.clojure/clojurescript "1.10.773"]
                 [hoplon/hoplon             "7.2.0"]
                 [hoplon/javelin            "3.9.0"]
                 [prismatic/dommy           "1.1.0"]
                 [binaryage/oops            "0.7.0"]   ; Switch to goog.object/get, etc.
                 #_[clj-commons/secretary     "1.2.4"] ; Needed if browser-side routing gets more complex

                 ;; Server-side dependencies
                 [org.clojure/clojure       "1.10.3"]
                 [clj-commons/pomegranate   "1.2.0"]
                 [compojure                 "1.6.2"]
                 [ring                      "1.9.1"]
                 [ring/ring-defaults        "0.3.2"]
                 [info.sunng/ring-jetty9-adapter "0.15.0"]]


 :source-paths #{"src"}
 :asset-paths  #{"assets"}
 :resource-paths #{"assets" "src"})


(require
 '[boot.pod :as pod]
 '[boot.task.built-in :refer [target]]
 '[boot.core :as boot]
 '[clojure.java.io :as io]

 '[adzerk.boot-cljs         :refer [cljs]]
 '[adzerk.boot-cljs-repl    :refer [cljs-repl cljs-repl-env start-repl]]
 '[adzerk.boot-reload       :refer [reload]]
 '[hoplon.boot-hoplon       :refer [hoplon]]
 '[deraen.boot-sass         :refer [sass]])


(deftask ui-scale [m multiplier VAL str "The user interface scale multiplier for Swing and JavaFX"]
  (System/setProperty "sun.java2d.uiScale" multiplier)
  (System/setProperty "glass.gtk.uiScale" multiplier)
  identity)


(deftask cider
  "Add dependencies for development tools that expect Cider/IDE middleware in the REPL."
  []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[cider/cider-nrepl "0.26.0-SNAPSHOT"]
                  [refactor-nrepl "2.5.1"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
                  cider.piggieback/wrap-cljs-repl
                  refactor-nrepl.middleware/wrap-refactor])
  identity)


(deftask reveal
  "Make the repl display a 'reveal' data browser"
  []
  #_(System/setProperty "vlaaad.reveal.prefs" ":theme :dark")
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[vlaaad/reveal "1.3.196"]
                  [lambdaisland/deep-diff2 "2.0.108"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[vlaaad.reveal.nrepl/middleware])
  identity)



(deftask kaocha
  "Run tests with Kaocha."
  [c config-file     FILE    file "Config file to read."
   H test-help               bool "Display Kaocha-boot usage information."
   _ print-config            bool "Print out the fully merged and normalized config, then exit."
   _ print-test-plan         bool "Load tests, build up a test plan, then print out the test plan and exit."
   _ print-result            bool "Print the test result map as returned by the Kaocha API."
   _ fail-fast               bool "Stop testing after the first failure."
   _ color                   bool "Enable ANSI color codes in output."
   _ no-color                bool "Disable ANSI color codes in output."
   _ watch                   bool "Watch filesystem for changes and re-run tests."
   _ no-watch                bool "Don't watch filesystem for changes."
   _ reporter        SYMBOL  sym  "Change the test reporter, can be specified multiple times."
   _ plugin          KEYWORD [kw] "Load the given plugins."
   _ version                 bool "Print version information and quit."
   s suite           KEYWORD [kw] "Test suite(s) to run."
   o options         EDN     edn  "Extra command line flags"]


  (let [deps        '[[lambdaisland/kaocha           "1.0.829" :exclusions [org.clojure/clojure]]
                      [lambdaisland/kaocha-cucumber  "0.0-53"]
                      [lambdaisland/kaocha-cloverage "1.0.75"]]

        ;; Turn the middleware symbols into strings to prevent an attempt to
        ;; resolve the namespaces when the list is processed in the Boot pod.
        ;; Boot's middleware configuration is read outside of the pod to ensure
        ;; that the user-specific Boot configuration is also captured.
        middlewares (conj (map str @@(resolve 'boot.repl/*default-middleware*)) 'list)

        pod         (pod/make-pod (update-in (boot/get-env) [:dependencies]
                                             into deps))

        test!       (fn []
                      (pod/with-eval-in pod

                        (require '[kaocha.config :as config]
                                 '[kaocha.plugin :as plugin]
                                 '[kaocha.api :as api]
                                 '[kaocha.jit :refer [jit]]
                                 '[kaocha.runner :as runner]
                                 '[kaocha.classpath :as classpath]
                                 '[clojure.set :as set]
                                 '[clojure.string :as str]
                                 '[clojure.tools.cli :as cli]
                                 '[slingshot.slingshot :refer [try+]])

                        (defn plugin-option-summary [specs]
                          (apply str
                                 "\n"
                                 "  -o, --options EDN                  EDN map of additional options.\n\n"
                                 "Plugin-specific options can be specified using map syntax, e.g.\n\n"
                                 "   boot kaocha --options '{:randomize false}'\n\n"
                                 "These additional options are recognized:\n\n"
                                 (interpose "\n"
                                            (map (fn [{:keys [id required desc]}]
                                                   (format "   %-30s %s" (str id " " (or required "BOOL")) desc))
                                                 (#'cli/compile-option-specs specs)))))

                        (try+
                         (with-redefs [classpath/add-classpath boot.pod/add-classpath]
                           (let [options           (merge (cond-> ~*opts*
                                                            ~no-color (assoc :color false)
                                                            ~no-watch (assoc :watch false)
                                                            :always   (dissoc :no-color :no-watch))
                                                          ~options)
                                 config            (config/load-config (:config-file options "tests.edn"))
                                 plugin-chain      (plugin/load-all (concat (:kaocha/plugins config) ~plugin))
                                 plugin-options    (plugin/run-hook* plugin-chain :kaocha.hooks/cli-options [])
                                 {:keys [summary]} (cli/parse-opts [] @#'runner/cli-options)
                                 config            (-> config
                                                      (config/apply-cli-opts options)
                                                      (config/apply-cli-args ~suite))
                                 exit-code
                                 (plugin/with-plugins plugin-chain
                                   (runner/run {:config  config
                                                :options options
                                                :summary (str "USAGE:\n\nboot kaocha [OPTIONS]...\n\n"
                                                              "  -s, --suite SUITE                  Test suite(s) to run.\n"
                                                              summary (plugin-option-summary plugin-options))
                                                :suites  ~suite}))]
                             (when (not= 0 exit-code)
                               (System/exit exit-code))))

                         (catch :kaocha/early-exit {exit-code :kaocha/early-exit}
                           (when (not= 0 exit-code)
                             (System/exit exit-code))))))]

    (boot/with-pre-wrap fileset
      (test!)
      fileset)))



(deftask dev
  "Build for local development."
  []
  (comp
   (cider)
   (watch)
   (speak)
   (kaocha)
   (reload)
   (sass :source-map true)
   (repl
    :port 8008
    :server true
    :init-ns 'server.handler
    :eval '(-main))
   (hoplon)
   (cljs-repl-env)
   (cljs :optimizations :none)))


(deftask prod
  "Build for production deployment."
  []
  (comp
   (sass :output-dir "assets/css"
         :output-style "compressed")
   (hoplon)
   (cljs :optimizations :none)
   (target :dir #{"target"})))
