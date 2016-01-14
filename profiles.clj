{:base {:jvm-opts ["-Xmx12g" "-Xms512m" "-XX:+UseParallelGC" "-Dlog4j.configurationFile=log4j2-subproject.xml"]}

 :user {:plugins [[cider/cider-nrepl "0.11.0-SNAPSHOT"]
                  [org.clojure/tools.nrepl "0.2.12"]
                  [lein-gorilla "0.3.5"]]}

 :dependencies [[cider/cider-nrepl "0.10.0"]
                [org.clojure/tools.nrepl "0.2.12"]]

 :repl-options {:nrepl-middleware
                [cider.nrepl.middleware.apropos/wrap-apropos
                 cider.nrepl.middleware.classpath/wrap-classpath
                 cider.nrepl.middleware.complete/wrap-complete
                 cider.nrepl.middleware.debug/wrap-debug
                 cider.nrepl.middleware.format/wrap-format
                 cider.nrepl.middleware.info/wrap-info
                 cider.nrepl.middleware.inspect/wrap-inspect
                 cider.nrepl.middleware.macroexpand/wrap-macroexpand
                 cider.nrepl.middleware.ns/wrap-ns
                 cider.nrepl.middleware.pprint/wrap-pprint
                 cider.nrepl.middleware.refresh/wrap-refresh
                 cider.nrepl.middleware.resource/wrap-resource
                 cider.nrepl.middleware.stacktrace/wrap-stacktrace
                 cider.nrepl.middleware.test/wrap-test
                 cider.nrepl.middleware.trace/wrap-trace
                 cider.nrepl.middleware.undef/wrap-undef]}}
