(remove-ns 'clj-foundation.data-test)

(ns clj-foundation.data-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-foundation.unit-test-common :as common]
            [clj-foundation.patterns :refer [nothing]]
            [clj-foundation.data :refer :all]))


(common/register-fixtures)


(deftest elem-satisfies?-test
  (testing "One element, no match"
    (is (not (elem-satisfies? even? [1]))))

  (testing "One element, matches"
    (is (= [2] (elem-satisfies? even? [2]))))

  (testing "Two elements, no match"
    (is (nil? (elem-satisfies? even? [1 3]))))

  (testing "Two elements, second matches"
    (is (elem-satisfies? even? [1 4])))

  (testing "Five elements, no match"
    (is (nil? (elem-satisfies? even? [1 3 5 7 9]))))

  (testing "Five elements, second-to-last matches"
    (is (elem-satisfies? even? [1 3 5 8 9])))

  (testing "Five elements, multiple matches"
    (is (elem-satisfies? even? [1 10 5 8 9]))))


(deftest replace-if-test
  (testing "binary form"
    (is (= ""            (replace-if "replacement" (constantly ""))))
    (is (= "replacement" (replace-if "replacement" (constantly false)))))

  (testing "terniary form - replace"
    (is (= "replacement" (replace-if "foo" #{"foo" "bar" "baz"} "replacement")))
    (is (= "replacement" (replace-if nil #(nil? %) "replacement"))))

  (testing "terniary form - do not replace"
    (is (= "original" (replace-if "original" #{"New!"} "replacement")))
    (is (= "original" (replace-if "original" (constantly false) "replacement")))))


(deftest replace-nil-test
  (is (= "replacement" (replace-nil "replacement" nil)))
  (is (= "data" (replace-nil "replacement" "data"))))


(deftest replace-nothing-test
  (is (= "replacement" (replace-nothing "replacement" nothing)))
  (is (= "replacement" (replace-nothing "replacement" nil)))
  (is (= "data" (replace-nothing "replacement" "data"))))


(deftest nothing->identity-test
  (let [to-valid-value (partial nothing->identity (*))]
    (is (= 5 (* 5 (to-valid-value nothing))))
    (is (= 10 (* 5 (to-valid-value 2))))))


(deftest identity->nil-test
  (testing "nothing is always considered an identity"
    (is (nil?                 (identity->nil nothing))))

  (testing "Numbers use zero? as their default identity predicate"
    (is (nil?                (identity->nil 0)))
    (is (= 42                (identity->nil 42))))

  (testing "Other values depend on (empty? value) to determine emptiness"
    (is (nil?                (identity->nil "")))
    (is (= "The quick brown" (identity->nil "The quick brown")))
    (is (nil?                (identity->nil [])))
    (is (= [:something]      (identity->nil [:something]))))

  (testing "The second parameter overrides the identity function"
    (is (nil?                (identity->nil 1         #{1})))
    (is (= 42                (identity->nil 42        #{1})))
    (is (nil?                (identity->nil "nil"     #{"nil" "none" " "})))
    (is (nil?                (identity->nil " "       #{"nil" "none" " "})))
    (is (nil?                (identity->nil "none"    #{"nil" "none" " "})))
    (is (= "Success"         (identity->nil "Success" #{"nil" "none" " "})))))


(deftest getter-test
  (testing "camelCase to getter"
    (is (= "getName"        (getter "name")))
    (is (= "getFirstName"   (getter "firstName"))))

  (testing "underscore_names to getter"
    (is (= "getFirstName"   (getter "first_name"))))

  (testing "hyphenated-names to getter"
    (is (= "getFirstName"   (getter "first-name")))
    (is (= "getThisIsATest" (getter "this-is-a-test")))))


(deftest keywordize-test
  (testing "'Some Pig' -> :some-pig"
    (is (= :some-pig             (keywordize "Some Pig"))))

  (testing "ThisIsATest -> :this-is-a-test and similar"
    (is (= :this-is-a-test       (keywordize "ThisIsATest")))
    (is (= :this-is-a-test       (keywordize "thisIsATest"))))

  (testing "' ', _, and / treated as delimiters and replaced with -"
    (is (= :this-is-a-test       (keywordize "This_Is/A/ test")))
    (is (= :thi$-is-a-test-123   (keywordize "thi$_Is/A/ test 123"))))

  (testing "nested.propertyName -> :nested.property-name"
    (is (= :nested.property-name (keywordize "nested.propertyName")))))
