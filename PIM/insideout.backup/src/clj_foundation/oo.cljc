(ns clj-foundation.oo
  "Functions and macros making it easier to write objects and invoke methods in Clojure using
  maps of functions.  Important design principles include:

  * => works the same way for retrieving both property values and invoking methods.  (Uniform access principle.)
  * Use existing let-map and letfn-map for constructing objects.
  * Prototype-based inheritence similar to Javascript.

  See the tests for various examples of constructing objects and invoking methods using let-map,
  letfn-map, and => below.")


(defn- find-method [o method-key]
  (let [root-value (get o method-key)]
    (if root-value
      root-value
      (when (:methods o)
        (find-method (:methods o) method-key)))))


(defn =>
  "Given a map where (some of it's) keys refer to functions, look up the function and
  invoke it passing the map as its initial argument followed by any additional arguments passed
  here.  Returns whatever the function returns."
  [object-map method-key & arguments]
  (let [args (concat [object-map] (if arguments arguments []))
        f    (find-method object-map method-key)]
    (if (and f (fn? f))
      (apply f args)
      (if-not (nil? f)
        f
        (throw (IllegalArgumentException. (str method-key " is undefined")))))))
