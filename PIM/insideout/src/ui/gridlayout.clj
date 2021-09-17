(ns ui.gridlayout
  (:require [ui.inits :refer [run-inits args->inits]])
  (:import [org.eclipse.swt SWT]
           [org.eclipse.swt.layout GridLayout GridData]))


(defn grid-layout
  "An init function for org.eclipse.swt.layout.GridLayout. Arguments in the form
  :field1 val1 :field2 val2, etc., set the layout object's properties."
  [& more]
  (let [inits (args->inits more)]
    (fn [parent]
      (let [l (GridLayout.)]
        (run-inits l inits)
        (.setLayout parent (GridLayout.))))))

(defn- grid-data
  [control]
  (let [parent        (.getParent control)
        parent-layout (.getLayout parent)
        layout-data   (or (.getLayoutData control) (GridData.))]
    (when-not (instance? GridLayout parent-layout)
      (throw (ex-info (str (.getSimpleName parent) ".layout = "
                           (if parent-layout
                             (.getSimpleName (class parent-layout))
                             "nil")
                           " but must be GridLayout."))))
    (.setLayoutData control layout-data)
    layout-data))

(defmacro set-fields!
  [obj & field-kvs]
  (letfn [(set-field [o [f v]] `(set! (. ~o ~f) ~v))]
    (let [x (gensym "x")
          setters (map (partial set-field x) (partition 2 field-kvs))]
      `(let [~x ~obj]
         ~@setters
         ~x))))

(defn cell-horizontal-fill []
  (fn [control]
    (let [layout-data (grid-data control)]
      (set-fields!
       layout-data
       horizontalAlignment SWT/FILL
       verticalAlignment SWT/CENTER
       grabExcessHorizontalSpace true
       grabExcessVerticalSpace false))))
