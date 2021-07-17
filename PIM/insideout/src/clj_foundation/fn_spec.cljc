(ns clj-foundation.fn-spec
  "Support for making function specs DRYer to write."
  (:require
   [clj-foundation.errors :refer [resolved]]
   [clojure.walk :refer [prewalk-replace]]))


(defn args
  "Return f's argument lists.  Public for testability only."
  [f]
  (-> f meta :arglists))


(defn validations
  "public for testability only"
  [symbols specs symbols-str]
  (assert (= (count symbols) (count specs))
          (str "(count arguments) != (count specs): " symbols-str "/" specs))

  (let [spec-symbol-pairs (partition 2 (interleave specs symbols))]
    (map (fn [[spec symbol]] `(valid? ~spec ~symbol)) spec-symbol-pairs)))


(defn- symbol->spec [symbols specs]
  (apply assoc {} (interleave symbols specs)))


(defn spec? [a] (or (seqable? a) (instance? clojure.lang.IFn a)))


(defn- arglist-str    [arglist] (pr-str arglist))
(defn- arglist-vector [arglist] (flatten arglist))
(defn- argspec        [arglist parameter-specs] (prewalk-replace (symbol->spec (arglist-vector arglist)
                                                                               parameter-specs)
                                                                 arglist))
(defn- all-valid?     [arglist parameter-specs] (validations (arglist-vector arglist)
                                                             parameter-specs
                                                             (arglist-str arglist)))
(defn- type-str       [arglist parameter-specs return-spec] (str "(=> "
                                                                 (argspec arglist parameter-specs)
                                                                 " "
                                                                 return-spec
                                                                 ")"))
(defn- typed-docs     [docstring type-str] (str (if (empty? docstring) "" (str docstring "\n")) type-str))


(defn typed-fn
  "Return a defn statement with :pre and :post conditions using specs to typecheck parameters and return value."
  [f parameter-specs return-spec more]
  (let [[docstring
         arglist
         body]   (if (string? (first more))
                   [(first more) (second more) (rest (rest more))]
                   [""           (first more)  (rest more)])
        new-docs (typed-docs docstring (type-str arglist parameter-specs return-spec))]

    (assert (valid? (s/coll-of spec?) arglist))

    `(defn ~f ~new-docs ~arglist
       {:pre  [~@(all-valid? arglist parameter-specs)]
        :post [(valid? ~return-spec ~(symbol "%"))]}
       ~@body)))


(defn annotate-fn
  "Rename the existing f; make a new f with type checking/docs; delegate to the renamed f."
  [f parameter-specs return-spec]
  (let [resolved-f (resolve f)
        f-renamed (symbol (str f `-renamed#))
        docstring (-> resolved-f meta :doc)
        arglist (-> resolved-f args first)
        new-docs (typed-docs docstring (type-str arglist parameter-specs return-spec))]

    (assert (valid? (s/coll-of spec?) arglist))

    `(do
       (def ~f-renamed ~f)
       (defn ~f ~new-docs [& args#]
         {:post [(valid? ~return-spec ~(symbol "%"))]}

         (let [~arglist args#]
           ~@(map (fn [condition] `(assert ~condition))
                  (all-valid? arglist parameter-specs)))

         (apply ~f-renamed args#)))))


(defmacro def-tfn
  "Define a typed function or redefine an existing function to be typed.

  The function's name and type signature are first.  If nothing else follows, the specified
  function must already exist and it will be redefined with the pre/post conditions specified
  by the parameter specs and return spec.  If a function definition (in defn form) follows,
  emits new defn containing pre/post conditions as specified by the parameter specs and return
  spec.

  The metadata map is supplied by this macro and cannot currently be overridden/supplemented.
  Only handles a single argument list.

  In all cases, includes the derived function type in the function's docstring.

  f -
  The function to define.

  parameter-specs -
  A vector of the specs to use to validate each (destructured) argument.  For example, if the
  argument list is [[x y] color] then the parameter spec must have three elements, corresponding
  to x, y, and color.  e.g.: [number? number? keyword?]  The parameter spec is always flattened,
  with the arguments listed in the order in which they appear in the parameter list.

  return-spec -
  A spec to validate the return value.

  more -
  If present, in the form: docstring? [arglist-vector] & statements.  If empty, then annotates
  an existing function f with a type defined by parameter-specs => return-spec."
  [f parameter-specs return-spec & more]

  {:pre [(valid? symbol? f)
         (valid? (s/coll-of spec?) parameter-specs)
         (valid? spec? return-spec)]}

  (if (empty? more)
    (annotate-fn f parameter-specs return-spec)
    (typed-fn f parameter-specs return-spec more)))
