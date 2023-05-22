(ns clj-foundation.math
  "Math abstractions.  Currently defines a protocol and type for mixed numbers.  Provides a sane
  replacement for clojure.core.rationalize! that always returns a rational number.

  MixedNumber can then more rationally render Clojure's Rational type as strings, but can also be
  used to decompose decimals or rationals > 1 into mixed numbers with easy access to the whole and
  fractional parts."

  (:require [clj-foundation.patterns :refer [let-map]])

  (:import [clojure.lang Ratio Numbers]))


(defn round
  "Round a decimal `d` to the specified precision `p` (number of significant digits)"
  [p d]
  (let [factor (Math/pow 10 p)]
    (/ (Math/round (* d factor)) factor)))


(defprotocol INumberParts
  "A protocol for numbers that can be split into parts."
  (decompose [this] "Returns a map consisting of the parts that make up 'this' number."))


(def MixedNumberParts
  "Formally define a MixedNumber's Map representation as returned by the (.decompose num)
   method."
  {:whole :s/Num
   :frac :s/Num})


(defn rationalize!
  "Like clojure.core.rationalize, but always returns a Ratio, even if the number can be reduced to n/1
  See also: http://dev.clojure.org/jira/browse/CLJ-1435?page=com.atlassian.jira.plugin.system.issuetabpanels:all-tabpanel"
  [n]
  (Numbers/toRatio (rationalize n)))


(deftype MixedNumber [number]
  INumberParts
  (decompose [this]
    (let [r (rationalize! number)
          n (numerator r)
          d (denominator r)]
      (if (> n d)
        (let-map
            [whole (long (/ n d))
             frac (/ (- n (* whole d)) d)])
        {:whole 0
         :frac r})))

  (toString [this]
    (let [mixed-number-parts (.decompose this)]
      (str (:whole mixed-number-parts) " " (:frac mixed-number-parts)))))
