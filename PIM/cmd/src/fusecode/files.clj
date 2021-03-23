(ns fusecode.files
  (:require [clojure.string :as str]
            [orchestra.core :refer [defn-spec]])
  (:import [java.io File]))


(def home (System/getProperty "user.home"))


(defn-spec expand-path string?
  "~ characters in p are substituted with (System/getProperty \"user.dir\") else p is returned
    unchanged."
  [p string?]
  (if (str/includes? p "~")
    (str/replace p "~" home)
    p))


(defn-spec exists boolean?
  "Returns truthy if the file specified by path p exists and falsey otherwise.  p must be a String."
  [p string?]
  (.exists (File. p)))
