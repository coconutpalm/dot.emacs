(ns clj-foundation.unit-test-common
  "A common place to put code that we always want to run before / after tests."
  (:require [clojure.test :refer :all]
            [clj-foundation.errors :refer [log]])
  (:import  [java.util TimeZone]))


(defn common-once-setup
  "Setup code applicable to all unit tests.  By default we turn Specs library function validation on."
  []
  ;; Set time zone to UTC to normalize timestamps, remove the need for DST conversions,
  ;; and to allow unit tests to run with the same timezone as jobs / application code
  (TimeZone/setDefault (TimeZone/getTimeZone "UTC")))


(defn common-once-cleanup
  "Tear down / cleanup code common to all unit tests"
  [])


(defn common-each-setup
  "Setup code applicable to all unit tests"
  [])


(defn common-each-cleanup
  "Setup code applicable to all unit tests"
  [])


(defn common-once-test-fixture
  "A common ONCE unit test fixture suitable to wrap all unit tests with
   initial one time setup and a final one time cleanup"
  [f]
  (common-once-setup)
  (f)
  (common-once-cleanup))


(defn common-each-test-fixture
  "A common EACH unit test fixture suitable to wrap any / all unit tests with
   setup and teardown that run for each test"
  [f]
  (common-each-setup)
  (f)
  (common-each-cleanup))


; Fixture registration: runs when the file is included / compiled

(defn register-fixtures []
  (use-fixtures :once common-once-test-fixture)
  (use-fixtures :each common-each-test-fixture))
