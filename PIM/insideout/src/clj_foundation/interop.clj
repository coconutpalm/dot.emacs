(ns clj-foundation.interop
  "Simplify Java object interop"
  (:require [clojure.string :as str]
            [clj-foundation.data :refer [keywordize getter]]))



(defmacro set-fields!
  "Set the Java object `obj` fields to the corresponding values in `fields-kvs`."
  [obj & field-kvs]
  (letfn [(set-field [o [f v]] `(set! (. ~o ~f) ~v))]
    (let [x (gensym "x")
          setters (map (partial set-field x) (partition 2 field-kvs))]
      `(let [~x ~obj]
         ~@setters
         ~x))))


(defn array
  "Return a Java array: `clazz`[] {`elements`...}

  Optionally, `clazz` may be a single-element vector (for syntactic sugar) as in:
    (array [Integer] 1 2 3)"
  [clazz & elements]
  (into-array (if (vector? clazz) (first clazz) clazz) elements))


(defn package-name
  "Returns the package name for the specified Class"
  [clazz]
  (->> (.split (.getName clazz) "\\.")
       reverse
       rest
       reverse
       (interpose ".")
       (apply str)))


(defn class-name
  "Returns the unqualified class name for the specified Class"
  [clazz]
  (.getSimpleName clazz))


(defn arity
 "Returns the maximum parameter count of each invoke method found by reflection
  on the input instance. The returned value can be then interpreted as the arity
  of the input function. The count does NOT detect variadic functions."
  [f]
  (let [invokes (filter #(= "invoke" (.getName %1)) (.getDeclaredMethods (class f)))]
    (apply max (map #(alength (.getParameterTypes %1)) invokes))))


;; Bean things

(defn- prop-name [prop] (if (keyword? prop) (name prop) (str prop)))

(defn- getter-seq [prop]
  (let [property-name (prop-name prop)]
    (map symbol (map getter (str/split property-name #"\.")))))


(defmacro bean-props
  "Like (bean... but allows specifying the properties to convert and allows chained nested properties.
  Property names are translated to idiomatic hyphenated Clojure keywords in the resulting Map.

  e.g.: Given the following beans:
  (def jd-address (Address. \"42 Computer Blvd.\" \"\" \"Acme\" \"AZ\" \"99999\"))
  (def john-doe (Person. \"John\" \"Doe\" jd-address))

  then:

  (bean-props john-doe :firstName :lastName :address)
  ==>
  {:first-name          \"John\"
   :last-name           \"Doe\"
   :address             <some address object>}

  One step farther:

  (bean-props john-doe :firstName :lastName :address.street :address.street2 :address.city :address.state :address.postalCode)
  ==>
  {:first-name          \"John\"
   :last-name           \"Doe\"
   :address.street      \"42 Computer Blvd.\"
   :address.street2     \"\"
   :address.city        \"Acme\"
   :address.state       \"AZ\"
   :address.postal-code \"99999\"}"
  [object & props]
  (->> (if (seq props) props [])
       (map (fn [prop] [(keywordize (prop-name prop)) `(.. ~object ~@(getter-seq prop) )]))
       (into {})))


#_(defmacro object
  "Create the Java object specified by clazz using the symbol in the map as the class name
  and the vector it maps to as the constructor arguments.  Keyword/value elements in the map
  are mapped to JavaBean style property setters.

  Wherever literal values are supplied, they are type checked by the macro at compile time.

  e.g.:

  (object {Customer [ctor-parm-1 ctorm-param-n] :first-name \"John\" :last-name \"Doe\"})"

    [obj-map])


#_(defmacro set-fields!
  "Set the JavaBean style fields of the specified object to the kvs specified in the
  field-values map.

  The keys in field-values may be :camelCase (without \"set\" at the beginning) or
  Clojure-style :hyphenated-words."
  [object field-values])


#_(defmacro set-obj-map
  [a & r]
  "Close...  But code would look like:

  (set-obj-map TransactionRequest. .amount 10.00 .orderId \"user42\")"
  `(doto (~a) ~@(partition 2 r)))
