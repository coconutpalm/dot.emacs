(ns clj-foundation.data
  "Convert data between various forms; provide useful guard functions.  For example:
  * any? - Return coll if any element in coll satisfies predicate
  * replace-nil - If value is not nil, return it, otherwise return replacement
  * keywordize - Turn string or named value into an idiomatic Clojure :keyword
  * ->SNAKE_CASE - Turn string or named value into a string in SNAKE_CASE form
  * Various functions for parsing and processing XML"
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str]
            [clj-foundation.patterns :as p :refer [f]]
            [clj-foundation.errors :as err]
            [potemkin :refer [def-map-type]])
  (:import [java.io StringReader]
           [clojure.lang Named])
  (:gen-class))


;; Predicates and data conversion functions -------------------------------------------------

(defn elem-satisfies?
  "Returns coll if any element in coll satisfies predicate."
  [predicate coll]
  (when (not-empty (filter predicate coll))
    coll))


(defn replace-if
  "In the binary form, replace value with the result of running predicate against value
   if that result is truthy.  In the terniary form, replace value with replacement if
   the result of running predicate against value is truthy.

   An idiomatic way to use this function is to pass a set of disallowed values as
   predicate, and a replacement value as replacement."

  ([value predicate]
   (let [maybe-result (predicate value)]
     (or
       maybe-result
       value)))

  ([value predicate replacement]
   (if (predicate value)
     replacement
     value)))


(defn replace-nil
  "Accepts a value that cannot be nil; if it is not nil it returns it, else it
  returns its replacement."
  [maybe-nil replacement]
  (if (nil? maybe-nil)
    replacement
    maybe-nil))


(defn nothing->identity
  "Takes nil or Nothing to the specified identity value for the type and computation in context,
  otherwise returns value.  An identity value can be applied to a value of the given type under the
  operation in context without affecting the result.  For example 0 is the identity value for rational
  numbers under addition.  The empty string is the identity value for strings under concatination.

  Note that Nothing is already an identity value for maps and seqs.  This function is only useful
  for types where the Nothing type is ill-behaved (e.g.: Strings, Numbers, ...) for a given operation.

  Another name for this concept is the monadic zero for the type/operation."
  [identity-value value]

  (if (p/something? value)
    value
    identity-value))


(defn identity->nil
  "Synopsis:
     (identity->nil [])                                  --> nil
     (identity->nil \"\")                                --> nil
     (identity->nil 1 #{1})                              --> nil   ; e.g.: Under multiplication
     (identity->nil \"none\" #{\"nil\" \"none\" \" \"})  --> nil

  If value is empty (for its type's natural definition of empty), returns nil.  Otherwise returns
  value.

  * Non-numeric values are empty iff (empty? value).
  * Numbers default to zero as their identity value.
  * The identity predicate may optionally be overridden in the second parameter."
  ([value identity-p #_(=> [Any] Bool)]
   (when-not (identity-p value) value))

  ([value]
   (cond
     (number? value) (identity->nil value zero?)
     :else           (identity->nil value empty?))))


(defn translate-nothingness
  "If value is nil or an instance of Nothing, run f and return its result.  Else, return value."
  [value f]
  (if (or (nil? value)
          (instance? (p/Nothing!) value))
    (f value)
    value))


(defn translate-something
  "If value is not Nothing return value, else run f and return its result."
  [value f]
  (if (p/something? value)
    value
    (f value)))



(defn undasherize
  "Replace all instances of '-' or '_' with replacement"
  [replacement value]
  (str/replace (name value) #"[\-_]" replacement))


(defn ->PascalCase
  "Translate s to PascalCase.  Handles hyphenated-names and underscore_names as well as
  names that are already PascalCase."
  [s]
  (->> (str/split s #"[\_-]")
     (map (f part =>
             (str (str/upper-case (first part))
                  (apply str (rest part)))))
     (str/join)))


(defn ->camelCase
  "Translate argument to camelCase.  Handles hyphenated-names and underscore_names as well as
  names that are already camelCase."
  [s]
  (let [s' (->PascalCase s)]
    (str (str/lower-case (first s'))
         (apply str (rest s')))))


(defn getter
  "Translate property-name to its Java getter syntax.  Handles hyphenated-names and underscore_names as well as
  names that are already camelCase or PascalCase."
  [property-name]
  (->> property-name
     ->PascalCase
     (str "get")))


(defn setter
  "Translate property-name to its Java setter syntax.  Handles hyphenated-names and underscore_names as well as
  names that are already camelCase or PascalCase."
  [property-name]
  (->> property-name
     name
     ->PascalCase
     (str "set")))


(defn ->SNAKE_CASE
  "Convert any Named or String object to SNAKE_CASE.  Does not handle camelCase."
  [value]
  (str/upper-case (undasherize "_" (name value))))


(defn ->uppercase-with-spaces
  "Convert - or _ to ' ' and captialize string."
  [value]
  (str/upper-case (undasherize " " (name value))))


(defn dasherize
  "Replace all instance of match with '-' in value."
  [match value]
  (str/replace (str value) match "-"))


(defn ->kebab-case
  "Convert to kebab-case.

  Ex. camelCase          -> :camel-case
      some_name          -> :some-name
      customer.firstName -> :customer.first-name"
  [name]
  (->> name
     (re-seq #"([A-Z _/]*[a-z1-9$\.]*)") ; Seq of tokens: Leading delimeter + following chars
     (map first)                         ; Take 1st element of each tuple in match seq
     (map #(str/replace % #"[ _/]" ""))  ; Eliminate explicit delimeter characters
     (filter #(not (empty? %)))          ; Some tokens will now be empty; throw them out
     (str/join "-")                      ; Back to a string, joined by "-"
     (str/lower-case)))                  ; ...


(defn keywordize
  "Return dasherized keyword from camelCase underscore_names, namespaced/names, etc.
  See the unit tests for the complete set of supported cases.

  Ex. camelCase          -> :camel-case
      some_name          -> :some-name
      customer.firstName -> :customer.first-name"
  [name]
  (->> name
     (->kebab-case)
     (keyword)))


(defn string->keyword
  "Convert string name to a keyword respecting naming-exceptions.
  naming-exceptions is a predicate (or set or other predicate-like thing).
   Ex. some_name -> :some-name"
  ([name naming-exceptions]
   (if-let [name-exception (naming-exceptions name)]
     name-exception
     (keywordize name)))
  ([name]
   (keywordize name)))


(defn string-list->keywords
  "Convert a list of strings to a list of keywords"
  ([list]
   (map (fn [string]
         (string->keyword (name string))) list))
  ([list naming-exceptions]
   (map (fn [string]
         (string->keyword (name string) naming-exceptions)) list)))


(defn constant-seq
  "Return an infinite lazy seq of cs"
  [c]
  (lazy-seq (cons c (constant-seq c))))


(defn set-map-entries
  "Returns a new copy of m where for all [k v] => if k is in entries, v is set to new-value."
  [m entries new-value]
  (let [new-entries (zipmap entries (constant-seq new-value))]
    (merge m new-entries)))


(defn remove-header-row
  "When processing CSV, frequently we want to throw out the header row."
  [data]
  (rest data))


(defn parse-xml
  "Parse XML from a String"
  [xml]
  (xml/parse (StringReader. xml)))


(defn xml-element->kv-pair
  "Return key-value pair where key is the tag name,
   and value is content of the xml element."
  [element]
  [(:tag element)
   (first (:content element))])


(defn xml-elements->map
  "Return a map of key-value pairs from the given xml elements."
  [elements]
  (into {} (map xml-element->kv-pair (:content elements))))


(defn extract-root-xml-element
  "Extract root xml element from the given XML document."
  [root-element xml]
  (let [initial-tag (:tag xml)]
    (if (= root-element initial-tag)
      (:content (first (:content xml)))
      (throw (java.lang.IllegalStateException. (str "Invalid initial XML tag: " initial-tag))))))
