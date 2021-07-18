(ns clj-foundation.types)


;; Traceability ---------------------------------------------------------------------

(defmacro trace
  "Like str but prepends the namespace and line/column of the call site."
  [& more]
  (let [line-col (vec (meta &form))
        s        (apply str *ns* (seq line-col) ": " (apply pr-str more))]
    `~s))


(defrecord SatisfyError [x errors msg]
  Object
  (toString [this] (:msg this)))

(defn positional-errs
  "Ensure xs satisfies positional predicates in `types'.  Returns xs or a SatisfyError"
  [types types-strs xs]
  (letfn [(pairs [as bs] (partition 3 (interleave as bs (range))))]
    (if (not= (count types) (count xs))
      (SatisfyError. xs
                     [(str "Expected: " types-strs)]
                     (str "Expected " types-strs))
      (let [preds  (pairs types types-strs)
            checks (pairs preds xs)
            errors (mapcat (fn [ [[p? pstr] x position] ]
                             (if (p? x)
                               []
                               [(str position ":(" pstr " " (pr-str x) ")")]))
                           checks)]
        (if (empty? errors)
          xs
          (SatisfyError. xs
                         (vec errors)
                         (apply str (->> (vec errors)
                                       (interpose ",")))))))))

(comment
  (positional-errs [] [] [])
  (positional-errs [number?] ["number?"] [5])
  (positional-errs [string?] ["string?"] [5])
  (positional-errs [number? string?] ["number" "string?"] [5])
  (positional-errs [number? string?] ["number" "string?"] [5 "Go!"])
  (positional-errs [number? ratio? string?] ["number?" "ratio?" "string?"] [5 3 "Go!"])
  ,)


(defmacro positional-types
  "Specify positional types expected inside sequences.  Returns a function that checks a given
  sequence.  If the function detects error(s), it returns a SatisfyError specifying the
  error(s) detected.  Otherwise the function returns its original input."
  [predicates]
  (let [types-strs (vec (map name predicates))]
    `(partial positional-errs ~predicates ~types-strs)))

(comment
  (def address? (positional-types [string? string?]))
  (address? ["Hello" "world"])
  (address? [:one "world"])
  ,)


(defmacro T
  "Because computing failures is more useful than asking if a value is `specs/valid?`.

  Here we reimagine a type constructor as a function that returns its input
  for valid arguments or that returns a SatisfyError for invalid arguments.

  Type constructors of this style can integrate seamlessly with ordinary Lisp
  since their behavior is transparent to downstream operations.  They also encourage
  rich error checking/reporting and integrate well with other predicate-based
  \"type systems\" in Clojure.

  Constructor arguments are determined to be valid iff `(predicate args)` is true,
  but with a twist:

  `predicate` can be a function like in specs.  Or to validate fixed-length vectors
  positionally it can be a vector of functions where each function is predicate
  corresponding with a position to be verified.

  This macro returns a type constructor function as defined above.

  If the value(s) passed to the type constructor conforms to `predicate`, the
  type construct function returns the original value as if it were the identity function.

  Otherwise it returns a `SatisfyError` with information on the failure.

  The result can be checked by asserting that the output of the type constructor
  is the same as its input.

  SatisfyError is also defined as a `failure?` below."
  [predicate]
  (cond
    (seqable? predicate) (let [types-strs (vec (map name predicate))]
                           `(partial positional-errs ~predicate ~types-strs))
    (symbol? predicate)  (let [type-str (name predicate)]
                           `(fn [x#] (if (~predicate x#)
                                      x#
                                      (let [type-err# (str "(" ~type-str " " x# ")")]
                                        (SatisfyError. x#
                                                       [(str "(" ~type-str " " (pr-str x#) ")")]
                                                       (str "(" ~type-str " " (pr-str x#) ")"))))))
    :default             (throw (ex-info (trace "Predicate must be sequable or a `fn?`") {:predicate predicate}))))


(comment
  (def name (T [string? string?]))
  (name ["Hello" "world"])
  (name [:one "world"])

  (def first-name (T string?))
  (first-name "Dave")
  (first-name :dave)
  ,)
