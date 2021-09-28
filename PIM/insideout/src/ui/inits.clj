(ns ui.inits
  "Defines API for defining and manipulating init functions.  An init function is a function
  where the initial argument is the object to init and subsequent arguments (if any)
  define the values used for initialization."
  (:require [ui.internal.reflectivity :as meta]
            [clj-foundation.patterns :refer [nothing]]
            [clj-foundation.interop :refer [array]]
            [clj-foundation.data :refer [->camelCase ->kebab-case setter nothing->identity]])
  (:import [clojure.lang IFn Keyword Reflector]
           [java.lang.reflect Modifier]
           [org.eclipse.swt.widgets Composite]))

(defn run-inits
  "Initialize the specified control using the functions in the `inits` seq."
  [props control inits]
  (doall (map #(apply % props control []) inits)))


(defmulti ->init
  "Convert first and second arguments (from front of vararg list) into an init function.  Returns the
  function and the number of arguments consumed from the arglist."
  (fn [arg1 _]
    (class arg1)))

(defmethod ->init
  IFn [arg1 _]
  [arg1 1])

(defmethod ->init
  String [arg1 _]
  (letfn [(set-text-on [_ control]
            (.setText control arg1))]
    [set-text-on 1]))

(defmethod ->init
  Keyword [arg1 arg2]
  (letfn [(set-property [_ o]
            (let [field-name  (->camelCase arg1)
                  field       (->> (.getClass o)
                                 (.getDeclaredFields)
                                 (filter (fn [field]
                                           (and (= field-name (.getName field))
                                                (not= 0 (bit-and (.getModifiers field) Modifier/PUBLIC))
                                                (Reflector/paramArgTypeMatch (.getType field) (class arg2)))))
                                 (first))]
              (if field
                (Reflector/setInstanceField o field-name arg2)
                (Reflector/invokeInstanceMethod o
                 (setter arg1)
                 (array [Object] arg2)))))]
    [set-property 2]))

(defn args->inits
  "Convert a vararg parameter list into a seq of init functions."
  [args]
  (letfn [(process-args [inits args]
            (let [arg1        (first args)
                  arg2        (second args)
                  [next-init
                   args-used] (->init arg1 arg2)]
              (cond
                (nil? arg2)                 (conj inits next-init)
                (= args-used (count args)) (conj inits next-init)
                (= 1 args-used)            (process-args (conj inits next-init) (rest args))
                (= 2 args-used)            (process-args (conj inits next-init) (rest (rest args))))))]
    (when (first args)
      (process-args [] args))))

(defn extract-style-from-args
  [args]
  (let [maybe-style (first args)]
    (if (instance? Integer maybe-style)
      [maybe-style (rest args)]
      [nothing args])))

(defmacro widget*
  "Meta-construct the specified SWT class derived from Widget."
  [^Class clazz style args]
  `(fn [props# ^Composite parent#]
    (let [child# (new ~clazz parent# ~style)
          inits# (args->inits ~args)]
      (run-inits props# child# inits#)
      child#)))

(defn widget-classes->inits [classes]
  (map (fn [clazz]
         (let [name (.getName clazz)
               doc (str "Construct a " name "\n\n" (ui.internal.docs/eclipsedoc-url name))
               name-sym (symbol name)
               fn-name (symbol (-> (.getSimpleName clazz) ->kebab-case))]
           `(defn ~fn-name
              ~doc
              [& inits#]
              (let [[style#
                     inits#] (extract-style-from-args inits#)
                    style# (nothing->identity SWT/NULL style#)]
                (widget* ~name-sym style# (or inits# []))))))
       classes))

(defmacro  composite-inits []
  (let [inits (widget-classes->inits meta/swt-composites)]
    `[~@inits]))

(defmacro widget-inits []
  (let [inits (widget-classes->inits meta/swt-widgets)]
    `[~@inits]))
