(ns clj-foundation.oo-test
  (:require [clj-foundation.oo :refer :all]
            [clj-foundation.patterns :refer [let-map letfn-map]]

            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(def arithmetic
  (letfn-map [(alwaysone [self] 1)
              (plusfive [self x] (+ x 5))]))


(t/deftest =>-map-of-fns
  (let [o arithmetic]

    (t/is 1 (=> o :alwaysone))
    (t/is 6 (=> o :plusfive 1))))


(defn person-name
  "Constructor for person-name"
  [first last]
  (let-map [first-name first
            last-name last

            methods (letfn-map
                     [(full-name [self] (str first-name " " last-name))])]))


(t/deftest =>-object-with-methods_uniform-property-and-method-access
  (let [o (person-name "Donald" "Duck")]
    (t/is "Donald" (=> o :first-name))
    (t/is "Donald Duck" (=> o :full-name))))


(comment
  (t/run-tests))
