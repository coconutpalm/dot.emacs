(ns util.atomic.core
  (:require  #?(:clj  [clojure.core.async :refer [<! put! chan go]]
                :cljs [cljs.core.async :refer [<! put! chan]])
             [editscript.core :as script]
             [editscript.edit :as e]
             [util.maps :refer [let-map]]

             [clojure.test :refer [deftest is testing run-tests]]))


(defn deep-diff
  "Like script/diff but diffs strings by default in addition to nested data
  structures.  `opts` are given as alternating keys and values and don't need
  to be placed into a `Map`, `Vector`, etc.

  Useful particularly for synchronizing multi-user edit state"
  [old new & opts]
  (script/diff old new
               (merge {:str-diff? true}
                      (apply hash-map opts))))

(defn- edit-script
  "Return the edit script vector for the diff between `old` and `new`."
  [old new]
  (e/get-edits (deep-diff old new)))


;; Eventually need lists by insurance company
(deftest deep-diff-test
  (let [test-data
        (let-map [old       {:dev [["savory" "tasty"]
                                   ["worn-down" "exhasted"]]
                             :lower-env [["savory" "tasty"]
                                         ["worn-down" "exhasted"]]}

                  fix-typos {:dev [["savory" "tasty"]
                                   ["worn-down" "exhausted"]]
                             :lower-env [["savory" "tasty"]
                                         ["worn-down" "exhausted"]]}

                  new-elem  {:dev [["savory" "tasty"]
                                   ["worn-down" "exhausted"]
                                   ["pooped-out" "exhausted"]]
                             :lower-env [["savory" "tasty"]
                                         ["worn-down" "exhausted"]]}])]

    (testing "Detects and fixes typos in strings"
      (let [expected-edits
            [[[:dev 1 1]       :s [4 [:+ "u"] 4]]
             [[:lower-env 1 1] :s [4 [:+ "u"] 4]]]]

        (is (= expected-edits (edit-script (:old test-data) (:fix-typos test-data))))))

    (testing "Adds new elements"
      (let [expected-edits
            [[[:dev 1 1]       :s [4 [:+ "u"] 4]]
             [[:dev 2]         :+ ["pooped-out" "exhausted"]]
             [[:lower-env 1 1] :s [4 [:+ "u"] 4]]]]

        (is (= expected-edits (edit-script (:old test-data) (:new-elem test-data))) )))))


(def ^:private delta-channel-default-opts
  {:watch-key :atomic/delta-channel
   :diff-fn deep-diff
   :channel-factory chan
   :put!-fn put!})

(defn delta-channel
  "Create a channel on `a` that will contain the diffs between oldValue and newValue as `a` changes.
  `a` must be watchable using `clojure.core/add-watch`.  The following options are recognized:

  `:watch-key`       The keyword used to register the watch.  Default is `:atomic/delta-channel`.
  `:diff-fn`         A function [old new] => diff to create the diff.  Default is `util.atomic.core/deep-diff`.
  `:channel-factory` Function used to construct the \"channel\" object.  Default is `chan`.
  `:put!-fn`         Function used to `put!` the diff onto the \"channel\".  Default is `put!`."
  ([a]
   (apply delta-channel a (flatten (vec delta-channel-default-opts))))

  ([a & opts]
   (let [resolved-opts (merge delta-channel-default-opts (apply hash-map opts))
         {:keys [watch-key diff-fn channel-factory put!-fn]} resolved-opts
         deltas (channel-factory)]
     (add-watch a watch-key
                (fn [k r old new]
                  (when (= watch-key k)
                    (put!-fn deltas (diff-fn old new)))))
     deltas)))


(deftest delta-channel-test
  (testing "Changes to atom propogate to channel"
    (let [test-data
          (let-map [start {:dev [["savory" "tasty"]
                                 ["worn-down" "exhasted"]]
                           :lower-env [["savory" "tasty"]
                                       ["worn-down" "exhasted"]]}

                    current (atom start)
                    sync-to (delta-channel current
                                           :channel-factory #(atom start) ; Stub the channel type for testing
                                           :put!-fn (fn [chan diff]
                                                      (swap! chan
                                                             #(script/patch % diff))))

                    expect-fixed-typos {:dev [["savory" "tasty"]
                                              ["worn-down" "exhausted"]]
                                        :lower-env [["savory" "tasty"]
                                                    ["worn-down" "exhausted"]]}

                    expect-new-elem {:dev [["savory" "tasty"]
                                           ["worn-down" "exhausted"]
                                           ["pooped-out" "exhausted"]]
                                     :lower-env [["savory" "tasty"]
                                                 ["worn-down" "exhausted"]]}])]

      (testing "String changes propogate to channel"
        (swap! (:current test-data) #(update-in % [:dev 1 1] (constantly "exhausted")))
        (swap! (:current test-data) #(update-in % [:lower-env 1 1] (constantly "exhausted")))

        (is (= (:expect-fixed-typos test-data) @(:sync-to test-data))))

      (testing "Adding new element propogates to channel"
        (swap! (:current test-data) #(update-in % [:dev 2] (constantly ["pooped-out" "exhausted"])))

        (is (= (:expect-new-elem test-data) @(:sync-to test-data)))))))


(comment                                ; Test forms to eval in the REPL
  (def watched (atom {:dev [["savory" "tasty"]
                            ["worn-down" "exhasted"]]
                      :lower-env [["savory" "tasty"]
                                  ["worn-down" "exhasted"]]}))

  (def deltas (delta-channel watched))

  (go (loop [] (println (<! deltas)) (recur)))

  (swap! watched #(update-in % [:dev 1 1] (constantly "exhausted")))
  (swap! watched #(update-in % [:dev 2] (constantly ["pooped-out" "exhausted"])))

  (run-tests)
  ,)
