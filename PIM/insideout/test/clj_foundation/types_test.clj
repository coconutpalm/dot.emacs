(ns clj-foundation.types-test
  (:require [clj-foundation.types :as testee :refer [T]]
            [clojure.test :as t :refer [deftest testing is run-tests]])

  (:import [clj_foundation.types TypeCtorError]))


(deftest positional-errs--test
  (testing "Argument count mismatch returns failure"
    (is (instance? TypeCtorError (testee/positional-errs [number? string?] ["number" "string?"] [5]))))

  (testing "Success--returns input value(s)"
    (is (= [] (testee/positional-errs [] [] [])))
    (is (= [5] (testee/positional-errs [number?] ["number?"] [5])))
    (is (= [2006 "Go" :cubs "go!"]
           (testee/positional-errs [integer? string? keyword? string?] ["integer?" "string?" "keyword?" "string?"]
                                   [2006 "Go" :cubs "go!"]))))

  (testing "Failure--returns TypeCtorError capturing (0-based) failure position"
    (is (instance? TypeCtorError (testee/positional-errs [string?] ["string?"] [5])))

    (let [e (testee/positional-errs [number? ratio? string?] ["number?" "ratio?" "string?"] [5 3 "Go!"])]
      (is (instance? TypeCtorError e))
      (is (= [1] (.errorPositions e))))))


(defn assert-illinois-drinking-ages [drinking-age]
  (is (= 30 (drinking-age 30)))
  (is (= 21 (drinking-age 21)))
  (is (instance? TypeCtorError (drinking-age 20)))
  (is (instance? TypeCtorError (drinking-age 10))))

(deftest T--test
  (testing "Positional T /"
    (let [first-middle-last (T [string? string? string?])]
      (testing "Success--returns input value(s)"
        (is (= ["Charles" "M." "Brown"] (first-middle-last ["Charles" "M." "Brown"]))))

      (testing "Failure--returns TypeCtorError capturing (0-based) failure positions"
        (let [e (first-middle-last [:charlie "M." 'brown])]
          (is (instance? TypeCtorError e))
          (is (= [0 2] (.errorPositions e)))))))

  (testing "Predicated T /"
    (testing "named"
      (let [drinking-age-illinois? (fn [age] (and (number? age)
                                                 (>= age 21)))
            drinking-age (T drinking-age-illinois?)]
        (assert-illinois-drinking-ages drinking-age)))

    (testing "anonymous (fn [x])"
      (let [drinking-age-illinois (T (fn [age] (>= age 21)))]
        (assert-illinois-drinking-ages drinking-age-illinois)))

    (testing "anonymous #(f %)"
      (let [drinking-age-illinois (T #(>= % 21))]
        (assert-illinois-drinking-ages drinking-age-illinois))))

  (testing "Nested type constructors /"
    (testing "Success"
      (let [first-middle-last (T [string? string? string?])
            address-2-lines (T [first-middle-last string?])]
        (is (= [["First" "M." "Last"] "Line2"]
               (address-2-lines [["First" "M." "Last"] "Line2"])))))

    (testing "Sadness"
      (let [first-middle-last (T [string? string? string?])
            address-3-lines (T [first-middle-last string? string?])
            e (address-3-lines [["First" :m :last] "Line2" :line3])]
        (is (= [{0 [1 2]} 2] (.errorPositions e)))))))


(deftest ctor->pred--test
  (let [drinking-age-illinois (T (fn [age] (>= age 21)))
        dai? (testee/ctor->pred drinking-age-illinois)]
    (is (dai? 21))
    (is (not (dai? 20)))))

(run-tests)
