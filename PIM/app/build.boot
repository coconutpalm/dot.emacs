;;
;; Hoplon CLJ(S) application skeleton with SASS support
;;
(set-env!
 :dependencies '[[nrepl                     "0.8.3" :scope "test"]
                 [adzerk/boot-cljs          "2.1.5" :scope "test"]
                 [adzerk/boot-cljs-repl     "0.4.0" :scope "test"]
                 [adzerk/boot-reload        "0.6.0" :scope "test"]
                 [deraen/boot-sass          "0.5.3" :scope "test"]
                 [cider/piggieback          "0.5.2" :scope "test"]
                 [weasel                    "0.7.1" :scope "test" :exclusions [org.clojure/clojurescript]]
                 [lambdaisland/kaocha-boot  "0.0-20" :scope "test"]
                 [org.slf4j/slf4j-nop       "1.7.13" :scope "test"]

                 ;; Both front/back-end
                 [org.clojure/core.async    "1.3.610"]
                 [juji/editscript           "0.5.4"]

                 ;; Front-end
                 [org.clojure/clojurescript "1.10.758"]
                 [hoplon/hoplon             "7.2.0"]
                 [hoplon/javelin            "3.9.0"]
                 [prismatic/dommy           "1.1.0"]
                 [binaryage/oops            "0.7.0"]   ; Switch to goog.object/get, etc.
                 #_[clj-commons/secretary     "1.2.4"] ; Needed if browser-side routing gets more complex

                 ;; Server-side dependencies
                 [org.clojure/clojure       "1.10.3"]
                 [stylefruits/gniazdo       "1.2.0" :exclusions [org.eclipse.jetty.websocket/websocket-client]]
                 [compojure                 "1.6.2"]
                 [ring                      "1.8.2"]
                 [ring/ring-defaults        "0.3.2"]
                 [info.sunng/ring-jetty9-adapter "0.14.2"]]


 :source-paths #{"src"}
 :asset-paths  #{"assets"}
 :resource-paths #{"assets" "src"})


(require
 '[adzerk.boot-cljs         :refer [cljs]]
 '[adzerk.boot-cljs-repl    :refer [cljs-repl cljs-repl-env start-repl]]
 '[adzerk.boot-reload       :refer [reload]]
 '[hoplon.boot-hoplon       :refer [hoplon]]
 '[deraen.boot-sass         :refer [sass]]
 '[kaocha.boot-task         :refer [kaocha]])


(deftask ui-scale [m multiplier VAL str "The user interface scale multiplier"]
  (System/setProperty "sun.java2d.uiScale" multiplier)
  (System/setProperty "glass.gtk.uiScale" multiplier)
  identity)


(deftask cider
  "Add dependencies for development tools that expect Cider/IDE middleware in the REPL."
  []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[cider/cider-nrepl "0.25.8"]
                  [refactor-nrepl "2.5.0"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
                  cider.piggieback/wrap-cljs-repl
                  refactor-nrepl.middleware/wrap-refactor])
  identity)


(deftask reveal
  "Make the repl speak 'reveal'"
  []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[vlaaad/reveal "1.3.196"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[vlaaad.reveal.nrepl/middleware])
  identity)


(deftask dev
  "Build for local development."
  []
  (comp
   (cider)
   (watch)
   (speak)
   #_(kaocha)                           ; Test runner; NPEs :-(
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
