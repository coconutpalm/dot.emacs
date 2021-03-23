(ns fusecode.launch-test
  (:require [clojure.test :refer [deftest is]]

            [clojure.spec.alpha :as s]
            [orchestra.spec.test :as st]
            [orchestra.core :refer [defn-spec]]

            [fusecode.launch :refer :all]
            [fusecode.patterns :refer [let-map letfn-map]]
            [fusecode.oo :refer :all])
  (:import [java.util ArrayList]))


(deftest download-or-update-boot-plugin_test
  (let [install-count (atom 0)
        update-count (atom 0)

        reset-tests (fn []
                      (reset! install-count 0)
                      (reset! update-count 0))

        plugin-manager-common
        (letfn-map
         [(plugin-dir [self])

          (secure-credentials-present? [self])

          (secure-credentials [self])

          (install [self origin] (swap! install-count inc))

          (update [self]         (swap! update-count inc))])

        plugin-manager_dir-exists (let-map [plugin-dir-exists (constantly true)
                                            methods plugin-manager-common])

        plugin-manager_dir-absent (let-map [plugin-dir-exists (constantly false)
                                            methods plugin-manager-common])]

    (reset-tests)

    (download-or-update-boot-plugin plugin-manager_dir-exists)

    (is (= 0 @install-count) "Expected no install when dir exists")
    (is (= 1 @update-count) "Expected update when dir exists")

    (reset-tests)

    (download-or-update-boot-plugin plugin-manager_dir-absent)

    (is (= 1 @install-count) "Expected install when dir absent")
    (is (= 0 @update-count) "Expected no update when dir absent")))
