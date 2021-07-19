(ns clj-foundation.types
  "A very Lispy way to imagine types.

  * Built on the idea that any predicate implicitly defines a type--a set of \"things\".
  * Not a framework - Just some functions / macros that stand on their own.
  * Doesn't try to \"boil the ocean\" or be the One Type Library to Rule Them All.
  * Coexists well and enhances other Clojure \"type\" libraries.
  * Totally transparent to the rest of your code.
  * Integrates well with :pre, :post, and (assert ...).
  * Implemented in barely a page of code with 0 dependencies.

  See the `T` macro for more.")

(definterface ICtorError
  (prependPath [pos type-str])
  (errorPositions))

(defrecord TypeCtorError [x errors msg path]
  ICtorError
  (prependPath [^TypeCtorError e pos type-str]
    (TypeCtorError.
     (:x e)
     (:errors e)
     (:msg e)
     (conj (seq (:path e)) (str pos ":" type-str))))

  (errorPositions [this]
    (let [offset (fn [e] (-> e (.split ":") first Integer/parseInt))
          errors (:errors this)]
      (vec (map (fn [e] (cond
                         (string? e) (offset e)
                         (instance? TypeCtorError e) {(-> e :path first offset) (.errorPositions e)}
                         :else (str (type e))))
                errors))))

  (toString [this]
    (if (first (:path this))
      (str "{\"" (apply str (conj (vec (interpose "/" (:path this))) "\" [" (:msg this))) "]}")
      (:msg this))))


(defn ctor->pred [predicate-or-type-ctor]
  "Accepts either a predicate or a type-ctor function and returns a predicate."
  (fn [x]
    (let [result (predicate-or-type-ctor x)]
      (if (instance? TypeCtorError result)
        false
        result))))


(defn ^:public-for-testability maybe-type-error
  "Runs (type-test x).  Returns an empty vector when the result is not an error.
  If the result is a TypeCtorError or is falsey, returns a vector containing the TypeCtorError
  or containing a String with an error message."
  [type-test type-str x & [maybe-pos]]
  (let [result (type-test x)]
    (if (instance? TypeCtorError result)
      [(if maybe-pos
         (-> result (.prependPath maybe-pos type-str))
         result)]
      (if result
        []
        [(str (if maybe-pos (str maybe-pos ":") "") "(" type-str " " (pr-str x) ")")]))))


(defn ^:public-for-testability positional-errs
  "Ensure xs satisfies positional predicates in `types'.  Returns xs or a TypeCtorError"
  [types types-strs xs]
  (letfn [(pairs [as bs] (partition 3 (interleave as bs (range))))] ; (range) adds position-index to (a,b,position-index)

    (if (not= (count types) (count xs))
      (TypeCtorError. xs
                     [(str "Expected: " types-strs)]
                     (str "Expected " types-strs)
                     '())

      (let [preds  (pairs types types-strs)
            checks (pairs preds xs)
            errors (mapcat (fn [ [[p? pstr] x position] ]
                             (maybe-type-error p? pstr x position))
                           checks)]
        (if (empty? errors)
          xs
          (TypeCtorError. xs
                         (vec errors)
                         (apply str (->> (vec errors)
                                       (interpose ", ")))
                         '()))))))


(defn x-or-err [type type-str x]
  (let [error-result (first (maybe-type-error type type-str x))]
    (cond
      (nil? error-result)    x
      (string? error-result) (TypeCtorError. x [error-result] error-result '())
      :else                  error-result)))


(defmacro T
  "Because computing failures is more useful than asking if a value is `specs/valid?`.

  Here we reimagine a type constructor as a function that returns its input
  for valid arguments or that returns a TypeCtorError for invalid arguments.

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

  Otherwise it returns a `TypeCtorError` with information on the failure.

  The result can be checked by asserting that the output of the type constructor
  is the same as its input.

  TypeCtorError is also defined as a `failure?` below."
  [type]
  (let [line-col (vec (meta &form))
        trace    (fn [& xs] (apply str *ns* (seq line-col) ": " (apply pr-str xs)))]
    (cond
      (map? type)     (throw (ex-info (trace "Not implemented") {:type type}))
      (vector? type)  (let [types-strs (vec (map name type))]
                        `(partial positional-errs ~type ~types-strs))

      ;; Named function
      (symbol? type)  (let [type-str (name type)]
                        `(partial x-or-err ~type ~type-str))

      ;; Anonymous functions
      (list? type)    (let [type-str (pr-str type)]
                        `(partial x-or-err ~type ~type-str))
      (fn* type)      (let [type-str (pr-str type)]
                        `(partial x-or-err ~type ~type-str))

      :default        (throw (ex-info (trace "Unrecognized type constructor \"predicate\"") {:type type})))))


(defn- hash-list-fn? [x] (fn* x))

(def ^:experimental ctor-ctor (T (some-fn map? vector? symbol? list? hash-list-fn?)))


(defn valid?
  "A replacement for specs/valid? for use with type constructors.  Loses errors subsequent
  to the initial one."
  [& cv's]
  {:pre [(not (nil? cv's))
         (even? (count cv's))]}
  (let [try-ctor (fn [[ctor v]]
                   (let [v' (ctor v)]
                     (if (= v v')
                       []
                       [[v v' (str "(= " v " " v' ")")]])))
        errors (mapcat try-ctor (partition 2 cv's))]
    (map (fn [[v v' e]] (assert (= v v') e)) errors)))


(comment
  (def fn-name (T symbol?))
  (valid? fn-name 'hello
          fn-name "Hello")
  ,)
