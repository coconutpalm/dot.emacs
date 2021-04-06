(ns fusecode.oo-test
  (:require [clojure.test :refer [deftest is]]

            [clojure.spec.alpha :as s]
            [orchestra.spec.test :as st]
            [orchestra.core :refer [defn-spec]]

            [fusecode.patterns :refer [let-map letfn-map]]
            [fusecode.oo :refer :all]))


(defn-spec arithmetic map?
  "Constructor for arithmetic"
  []
  (letfn-map [(alwaysone [self] 1)
              (plusfive [self x] (+ x 5))]))


(deftest =>-map-of-fns
  (let [o (arithmetic)]

    (is 1 (=> o :alwaysone))
    (is 6 (=> o :plusfive 1))))


(s/def ::first-name string?)
(s/def ::last-name string?)
(s/def ::person-name (s/keys :req-un [::first-name ::last-name]))


(defn-spec person-name ::person-name
  "Constructor for person-name"
  [first string?, last string?]
  (let-map [first-name first
            last-name last

            methods (letfn-map
                     [(full-name [self] (str first-name " " last-name))])]))


(deftest =>-object-with-methods_uniform-property-and-method-access
  (let [o (person-name "Donald" "Duck")]
    (is (s/valid? ::person-name o))
    (is "Donald" (=> o :first-name))
    (is "Donald Duck" (=> o :full-name))))
