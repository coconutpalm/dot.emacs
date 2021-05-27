#!/usr/bin/env goclj
;;"DEPS='clj-time=0.15.1;some-other-dep=3.4.5'"

(ns user
  #_(:require [clj-time.core :as t]))

(defn -main [& args]
  (prn :args args))

(defn hello []
  (println "Hello, world"))

(ns test
  (:require [clojure.test :refer [deftest is testing are]]
            [user]))

(deftest main-test
  (is (some? (user/-main 1 2)) "FIXME"))
