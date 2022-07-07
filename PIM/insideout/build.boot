(set-env!
 :dependencies '[[nrepl "0.8.3" :scope "test"]
                 [org.slf4j/slf4j-nop "1.7.13" :scope "test"]
                 [adzerk/boot-jar2bin "1.2.0" :scope "test"]
                 [adzerk/boot-reload "0.6.1" :scope "test"]

                 [javax.inject/javax.inject "1" :scope "provided"]
                 [org.eclipse.sisu/org.eclipse.sisu.inject "0.3.4" :scope "provided"]

                 ;; Client and server
                 #_ [org.clojure/core.async "1.3.610"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.slf4j/slf4j-simple "1.7.5"]
                 [juji/editscript "0.5.4"]

                 ;; Server-side dependencies
                 [org.clojure/clojure "1.10.3"]
                 [ns-tracker "0.4.0"]
                 [potemkin "0.4.3"]

                 ;; Git

                 ;; Maven resolver / Pomegranate
                 [org.tcrawley/dynapath "1.0.0"]
                 [org.apache.maven.resolver/maven-resolver-api "1.6.3"]
                 [org.apache.maven.resolver/maven-resolver-spi "1.6.3"]
                 [org.apache.maven.resolver/maven-resolver-util "1.6.3"]
                 [org.apache.maven.resolver/maven-resolver-impl "1.6.3"]
                 [org.apache.maven.resolver/maven-resolver-transport-file "1.6.3"]
                 [org.apache.maven.resolver/maven-resolver-transport-http "1.6.3"]
                 [org.apache.maven.resolver/maven-resolver-transport-wagon "1.6.3"]
                 [org.apache.maven.resolver/maven-resolver-connector-basic "1.6.3"]
                 ;; https://mvnrepository.com/artifact/org.apache.maven/maven-aether-provider
                 [org.apache.maven/maven-aether-provider "3.3.9"]
                 [org.apache.maven.wagon/wagon-provider-api "3.3.2" :exclude [org.codehaus.plexus/plexus-utils]]
                 [org.apache.maven.wagon/wagon-http "3.3.4"]
                 [org.apache.maven.wagon/wagon-ssh "3.3.4"]
                 [org.apache.httpcomponents/httpclient "4.5.8"]
                 [org.apache.httpcomponents/httpcore "4.4.11"]

                 ;; P2 repo utilities
                 ;; OSGi processing
                 [com.ibm.sbt/com.ibm.commons "9.0.0"]
                 [org.eclipse.platform/org.eclipse.osgi "3.15.100"]

                 ;; Checksums
                 [commons-codec/commons-codec "1.13"]

                 ;; Compressed remote streams
                 [org.apache.commons/commons-compress "1.19"]
                 [org.tukaani/xz "1.8"]]


 :repositories #(conj %
                      '["artifactory.openntf.org" {:url "https://artifactory.openntf.org/openntf"}]
                      '["eclipse.org.p2" {:url "https://download.eclipse.org/releases/2021-09"
                                          :layout "p2"}])

 :source-paths #{"src" "test" "resources"}
 :resource-paths #{"src" "resources" "lib"})

(require
 '[boot.pod :as pod]
 '[boot.task.built-in :refer :all]
 '[adzerk.boot-jar2bin :refer :all]
 '[boot.core :as boot]
 '[clojure.java.io :as io]

 '[adzerk.boot-reload       :refer [reload]])

(task-options!
 pom {:project 'insideout
      :version "0.1.0"})


(deftask ui-scale [m multiplier VAL str "The user interface scale multiplier for Swing and JavaFX"]
  (System/setProperty "sun.java2d.uiScale" multiplier)
  (System/setProperty "glass.gtk.uiScale" multiplier)
  identity)


(deftask cider
  "Add dependencies for development tools that expect Cider/IDE middleware in the REPL."
  []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[cider/cider-nrepl "0.27.2"]
                  [refactor-nrepl "3.0.0"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
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
   (notify :audible true :visual true :theme "woodblock")
   (javac)
   (kaocha)
   (reload)
   (repl
    :port 8008
    :server true
    :init-ns 'insideout.core
    :eval '(-main))))


(deftask prod
  "Build for production deployment."
  []
  (comp #_(aot :namespace #{'insideout.boot})
     (javac)
     (pom)
     (uber)
     (jar :main 'loader.Main :file "io.jar")
     (bin :output-dir "bin")))
