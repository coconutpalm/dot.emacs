(ns clj-foundation.types
  "A very Lispy way to imagine types.

  * Built on the idea that any predicate implicitly defines a type--a set of \"things\".
  * Not a framework - Just some functions / macros that stand on their own.
  * Doesn't try to \"boil the ocean\" or be the One Type Library to Rule Them All.
  * Coexists well with and enhances other Clojure \"type\" libraries, particularly Specs.
  * Totally transparent to the rest of your code.
  * Integrates well with :pre, :post, and (assert ...).
  * Implemented in barely a page of code with 0 dependencies.

  See the `T` macro for more."
  (:require [clojure.set :as set]))


;; Error types ========================================================================

(definterface ICtorError
  (prependPath [pos type-str])
  (errorPositions))

;;FieldErrorT {(Opt. :pos) int? :msg string?}
(defrecord FieldErr [pos msg]
  Object
  (toString [this]
    (if pos
      (str pos ":" msg)
      msg)))


(defrecord TypeCtorError [x errors msg path]
  :load-ns true

  ICtorError
  (prependPath [^TypeCtorError e pos type-str]
    (TypeCtorError.
     (:x e)
     (:errors e)
     (:msg e)
     (conj (seq (:path e)) (str pos ":" type-str))))

  (errorPositions [this]
    (letfn [(pos [e] (if (:pos e) (:pos e) 0))]
      (vec (map (fn [e] (cond
                         (instance? TypeCtorError e) {(-> e :path first pos) (.errorPositions e)}
                         (map? e)                    (pos e)
                         :else                       (str (type e))))
                (:errors this)))))

  Object
  (toString [this]
    (if (first (:path this))
      (str "{ "
           (apply str "path://" (conj (vec (interpose "/" (:path this))) "/"))
           " [" (:msg this) "]"
           " }")
      (str (:msg this)))))


(defn T->pred
  "Accepts either a predicate or a type-ctor function and returns a traditional predicate."
  [predicate-or-type-ctor]
  (fn [x]
    (let [result (predicate-or-type-ctor x)]
      (if (instance? TypeCtorError result)
        false
        result))))


;; Test one thing =====================================================================

(defn ^:public-for-testability maybe-type-error
  "Runs (type-test x) where `type-test` is a \"predicate\" indicating if `x` satisfies `type`.

  Returns a vector of 0 or 1 element depending on if an error was detected or not.

  Truthy values except for (instance? TypeCtorError value) satisfy (type-test x) and do
  not return an error value.  (They return an empty vector.)

  If type-test returns a TypeCtorError and is positional (maybe-pos is not nil), the
  result is a copy of the TypeCtorError with the positional information prepended
  to its path.

  If type-test returns a TypeCtorError and no positional information is supplied,
  the result is the TypeCtorError.

  If type-test returns falsey, the result is a String containing an error message."
  [type-test type-str x & [maybe-pos]]
  (let [result (type-test x)]
    (if (instance? TypeCtorError result)
      [(if maybe-pos
         (-> result (.prependPath maybe-pos type-str))
         result)]
      (if result
        []
        [(let [msg (str "(" type-str " " (pr-str x) ")")]
           (FieldErr. maybe-pos msg))]))))


;; Map type constructors ==============================================================


(defrecord Opt [key])

(defn ^:public-for-testability map-err-T
  "Ensure `m` satisfies k/v predicates in `kv-types` map.  Returns `m` or a TypeCtorError."
  [kv-types]
  (let [required-keys (->> kv-types
                         (seq)
                         (map first)
                         (filter #(not (instance? Opt %)))
                         (set))
        predicates    (into {}
                            (map (fn [[k v]]
                              (if (instance? Opt k)
                                [(:key k) v]
                                [k v]))
                            kv-types))]

    (fn [m]
      (let [missing-keys  (set/difference required-keys (set (keys m)))]
        (if-not (empty? missing-keys)
          (let [errors (->> missing-keys
                          (map (fn [k]
                                 (let [type-str (:type-str (get predicates k))]
                                   (FieldErr. k type-str))))
                          (vec))
                msg (->> missing-keys
                       (map #(get predicates %))
                       (map :type-str)
                       (interpose ", ")
                       (apply str "Missing k/v(s): "))]
            (TypeCtorError. m errors msg '()))

          (let [errors (vec (mapcat (fn [[k v]]
                                      (let [{:keys [pred type-str]} (get predicates k)]
                                        (maybe-type-error pred type-str v k)))
                                    m))]
            (if (empty? errors)
              m
              (TypeCtorError. m
                              errors
                              (->> errors
                                 (interpose ", ")
                                 (apply str))
                              '()))))))))


;; Sequential type constructors =======================================================

(defn ^:public-for-testability positional-errs
  "Ensure xs satisfies positional predicates in `types'.  Returns xs or a TypeCtorError."
  [types types-strs xs]
  (letfn [(pairs [as bs] (partition 3 (interleave as bs (range))))] ; (range) adds position-index to (a,b,position-index)

    (if (not= (count types) (count xs))
      (let [msg (str "(count types): " (count types) " (count xs): " (count xs) " types: " types-strs)]
        (TypeCtorError. xs [(FieldErr. nil msg)] msg '()))

      (let [preds  (pairs types types-strs)
            checks (pairs preds xs)
            errors (mapcat (fn [ [[p? pstr] x position] ]
                             (maybe-type-error p? pstr x position))
                           checks)]
        (if (empty? errors)
          xs
          (TypeCtorError. xs
                          (vec errors)
                          (->> (vec errors)
                             (interpose ", ")
                             (apply str))
                          '()))))))

(defn seq-of'
  [p? p?-str]
  (fn [xs]
    (let [indexed-xs (partition 2 (interleave xs (range)))
          errors     (mapcat (fn [[x pos]]
                               (maybe-type-error p? p?-str x pos))
                             indexed-xs)]
      (if (empty? errors)
        xs
        (TypeCtorError. xs
                        (vec errors)
                        (->> (vec errors)
                           (interpose ", ")
                           (apply str))
                        '())))))

(defmacro seq-of [p?]
  (let [p?-str (pr-str p?)]
    `(seq-of' ~p? ~p?-str)))


;; Predicate type constructors ========================================================

(defn x-or-err [type type-str x]
  (let [error-result (first (maybe-type-error type type-str x))]
    (cond
      (nil? error-result)  x
      (map? error-result)  (TypeCtorError. x [error-result] error-result '())
      :else                error-result)))


;; The `T` macro ======================================================================

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
  positionally it can be a vector of functions where each function is a predicate
  that validates the value in its corresponding position.

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
      (map? type)     (let [kv-types (->> type
                                        (map (fn [[k v]]
                                               [k {:pred v
                                                   :type-str (str (pr-str k) " " (pr-str v))}]))
                                        (into {}))]
                        `(map-err-T ~kv-types))

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
(def type-ctor? (some-fn map? vector? symbol? list? hash-list-fn?))


(defn valid?
  "Asserts that `type-ctor` appled to `x` yields a valid value.  Returns `x` if successful."
  [type-ctor x]
  (let [maybe-error (type-ctor x)]
     (if (= x maybe-error)
       x
       (throw (AssertionError. (.toString maybe-error))))))


(comment
  (def fn-name (T symbol?))
  (valid? fn-name 'hello
          fn-name "Hello")
  ,)
