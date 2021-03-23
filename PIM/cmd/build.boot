;; Dependency management: Use this?
;; https://github.com/borkdude/boot-bundle

(set-env!
 :resource-paths #{"resources"}
 :source-paths   #{"src" "java" "test"}

 :dependencies '[[org.clojure/clojure            "1.10.3"]
                 [org.clojure/tools.cli          "1.0.206"]
                 [orchestra                      "2021.01.01-1"]
                 [http-kit                       "2.5.3"]

                 [org.clojure/tools.logging      "1.1.0"]
                 #_[ch.qos.logback/logback-classic "1.1.3"]

                 [clj-jgit                       "1.0.1"]

                 [cpmcdaniel/boot-copy           "1.0"   :scope "test"]
                 [adzerk/boot-test               "1.2.0"]
                 #_[boot-clj/boot                  "2.8.3" :scope "test"]
                 [adzerk/boot-jar2bin            "1.2.0" :scope "test"]
                 #_[coconutpalm/boot-boot          "LATEST" :scope "test"]])


;; Require boot-boot tasks
(require
 '[adzerk.boot-jar2bin  :refer [bin]]
 '[adzerk.boot-test     :refer [test]]
 #_'[clj-boot.core        :refer :all]

 '[clojure.java.io      :as io])


(deftask copy-bootstrap-to-resources
  "copy bootstrap.jar from the target folder to the bin folder"
  []
  (fn [next-handler]
    (fn [fileset]
      (next-handler fileset)
      (io/copy (io/file "target/bootstrap.jar") (io/file "resources/bootstrap.jar")))))


(deftask remove-bootstrap-binary
  "Remove bin/bootstrap since it's now embedded inside the \"fuse\" binary."
  []
  (fn [next-handler]
    (fn [fileset]
      (next-handler fileset)
      (io/delete-file "bin/bootstrap" true))))


(deftask boot-bin []
  (comp
   (javac)
   (jar :file "bootstrap.jar" :main 'boot.Boot)
   (target)
   (copy-bootstrap-to-resources)))


(deftask fuse-bin []
  (comp
   (aot :all true)
   (pom)
   (uber)
   (jar)
   (target)
   (bin)
   (remove-bootstrap-binary)))


(deftask all [] (comp (boot-bin) (fuse-bin) (remove-bootstrap-binary)))


(task-options!
 pom {:project     'fusecode/main
      :description "A programmers' editor intended to be embedded inside of build tools.  This is the main launcher/build-tool plugin."
      :version     "0.3.0"
      :scm         {:url "https://github.com/fusion-editor/fusion-main"}}
 bin {:output-dir "bin"}
 jar {:main 'fusion.core :file "fuse.jar"}
 aot {:namespace #{'fusion.core}})
