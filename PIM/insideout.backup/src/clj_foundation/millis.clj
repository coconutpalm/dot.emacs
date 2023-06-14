(ns clj-foundation.millis
  "Convert various time values to milliseconds and back.  Decompose millis to days, hours, minutes, and seconds."
  (:require [clojure.string :as str :refer [trimr]]
            [clj-foundation.patterns :refer [let-map]]
            [clj-foundation.math :refer [->MixedNumber INumberParts round]])
  (:import [java.util Date]))

(defn <-seconds
  "Seconds to milliseconds"
  [s]
  (* s 1000))

(defn <-minutes
  "Minutes to milliseconds"
  [m]
  (->> m
    (* 60)
    (<-seconds)))

(defn <-hours
  "Hours to milliseconds"
  [h]
  (->> h
    (* 60)
    (<-minutes)))

(defn <-days
  "Days to milliseconds"
  [d]
  (->> d
    (* 24)
    (<-hours)))

(defn ->seconds
  "Milliseconds to seconds"
  [m]
  (-> m (/ 1000) ->MixedNumber))

(defn ->minutes
  "Milliseconds to minutes"
  [m]
  (-> m ->seconds .number
      (/ 60) ->MixedNumber))

(defn ->hours
  "Milliseconds to hours"
  [m]
  (-> m ->minutes .number
      (/ 60) ->MixedNumber))

(defn ->days
  "Milliseconds to days"
  [m]
  (-> m ->hours .number
      (/ 24) ->MixedNumber))

(defrecord dhms [millis]
  INumberParts
  (decompose [this]
    (let-map [millis (:millis this)
              days (->days millis)
              hours (->hours (* (:frac (.decompose days)) 24 60 60 1000))
              minutes (->minutes (* (:frac (.decompose hours)) 60 60 1000))
              seconds (->seconds (* (:frac (.decompose minutes)) 60 1000))]))

  (toString [this]
    (let [parts (.decompose this)]
      (trimr
       (str/join
        (map (fn [part]
               (let [whole (:whole (.decompose (part parts)))]
                 (if (pos? whole)
                   (str whole (second (str part)) " ")
                   "")))
             [:days :hours :minutes :seconds]))))))

(defn remaining-millis
  "Given a start time in millis, the total number of steps, and the current
  step number, estimates the remaining time in millis."
  [start-time number-of-steps current-step]
  (if (> current-step 0)
    (let [elapsed-time (- (System/currentTimeMillis) start-time)]
      (long (* (/ elapsed-time current-step)
               (- number-of-steps current-step))))
    (Long/MAX_VALUE)))

(defn progress-report
  "Calculate a progress report map containing the `:estimated-dmhs` along with the `:complete%`
  to complete a process beginning at `start-time` with a total `number-of-steps`
  that is about to process `current-step`."
  [start-time number-of-steps current-step]
  (let-map [estimated-dhms (->dhms (remaining-millis start-time number-of-steps current-step))
            complete%      (round 2 (* (/ (double current-step) (double number-of-steps))))]))
