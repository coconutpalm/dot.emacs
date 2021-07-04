(ns clj-foundation.io-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.unit-test-common :as common]
            [clj-foundation.io :refer :all])

  (:import [java.io File FileNotFoundException]
           [java.net URL]
           [clojure.lang ExceptionInfo]))


(common/register-fixtures)


(deftest with-err-str-test
  (testing "Printing to *out* doesn't wind up in with-err-str"
    (is (= "" (with-err-str (print "to *out*")))))

  (testing "Printing to *err* is returned in with-err-str"
    (is (= "to *err*" (with-err-str (print-writer *err* "to *err*"))))))


(deftest string-output-writer-test
  (testing "string-output-writer captures output"
    (is (= "the output." (let [str-out (string-output-writer)]
                           (print-writer str-out "the output.")
                           (.toString str-out))))))


(deftest normalize-file-input-test
  (testing "If a string input references a resource, a resource URL is returned."
    (is (instance? URL (normalize-file-input "_test-config.edn")))
    (is (= "/some/file/path" (normalize-file-input "/some/file/path"))))

  (testing "Return the default value as a resource when the envar is not defined"
    (is (instance? URL (normalize-file-input ["CONFIG_PRODDD" "_test-config.edn"]))))

  (testing "Return the envar value as a file if it is defined"
    (is (instance? File (normalize-file-input ["CONFIG_PROD" "_test-config.edn"]))))

  (testing "If a resource or file is specified explicitly, an appropriate URL or File is returned"
    (is (instance? URL (normalize-file-input {:resource "_test-config.edn"})))
    (is (instance? File (normalize-file-input {:file "/etc/passwd"})))
    (is (thrown? RuntimeException (normalize-file-input {})))
    (is (thrown? Exception (normalize-file-input {:foo "bar"})))))


(deftest read-file-test
  (io/copy (-> "_test-config-prod.edn" io/resource slurp) (io/file "/tmp/_test-config-prod.edn"))

  (testing "Reading a file returns its contents"
    (is (< 0 (count (read-file "/etc/passwd")))))

  (testing "Reading a nonexistant file throws an exception"
    (is (thrown? FileNotFoundException (read-file "/foo/bar/baz.quux"))))

  (testing "A Malformed FileLocation parameter throws an exception"
    (is (thrown? RuntimeException (read-file {})))
    (is (thrown? Exception (read-file {:foo "bar"}))))

  (testing "Read a file using extended syntax returns its contents"
    (is (< 0 (count (read-file {:file "/etc/passwd"})))))

  (testing "Read a resource using extended syntax returns its contents"
    (is (< 0 (count (read-file {:resource "_test-config.edn"}))))
    (is (< 0 (count (read-file ["CONFIG_PROOOOD" "_test-config.edn"])))))

  (testing "Reading a resource overridden by a file ENVAR returns the file's contents"
    (let [contents (read-file ["CONFIG_PROD" "_test-config.edn"])]
      (is (< 0 (count contents)))
      (is (str/includes? contents "salut")))))


(deftest resource-as-string-test
  (testing "Reading a resource returns its contents"
    (let [contents (resource-as-string "_test-config.edn")]
      (is (< 0 (count contents)))
      (is (str/includes? contents "bonjour"))))

  (testing "Reading an overridden resource returns its contents"
    (let [contents (resource-as-string "CONFIG_PROOOOOD" "_test-config.edn")]
      (is (< 0 (count contents)))
      (is (str/includes? contents "bonjour")))

    (let [contents (resource-as-string "CONFIG_PROD" "_test-config.edn")]
      (is (< 0 (count contents)))
      (is (str/includes? contents "salut"))))

  (testing "An incorrect number of arguments throws an exception"
    (is (thrown? IllegalArgumentException (resource-as-string)))
    (is (thrown? IllegalArgumentException (resource-as-string "foo" "bar" "baz")))))


(deftest read-template-test
  (io/copy (-> "_test-config-prod.edn" io/resource slurp) (io/file "/tmp/_test-config-prod.edn"))

  (testing "Reading a template without variables returns its contents"
    (is (< 0 (count (read-template ["CONFIG_PROD" "_test-config.edn"])))))

  (testing "Reading a template with variables but not supplying values throws an exception"
    (is (thrown? ExceptionInfo (read-template {:resource "_test-config.edn"}))))

  (testing "Reading a template but supplying variables returns the contents with the variables substituted."
    (let [file-content (read-template {:resource "_test-config.edn"} :ENGLISH-GREETING "heyo")]
      (is (< 0 (count file-content)))
      (is (str/includes? file-content "heyo"))))

  (testing "Reading a template using embedded extended file syntax returns the correct contents"
    (let [file-content (read-template "CONFIG_PROOOD" "_test-config.edn" :ENGLISH-GREETING "heyo")]
      (is (< 0 (count file-content)))
      (is (str/includes? file-content "heyo")))

    (let [file-content (read-template "CONFIG_PROD" "_test-config.edn" :ENGLISH-GREETING "heyo")]
      (is (< 0 (count file-content)))
      (is (str/includes? file-content "yo!")))))
