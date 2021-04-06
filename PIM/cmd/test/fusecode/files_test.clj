(ns fusecode.files-test
  (:require [clojure.test :refer [deftest is]]
            [fusecode.files :refer :all]))


(deftest expand-path-test
  (is (= (str home "/dir") (expand-path "~/dir")) "Expected user.dir to be substituted for ~")
  (is (= "666888" (expand-path "666888"))         "Expected no substitutions"))
