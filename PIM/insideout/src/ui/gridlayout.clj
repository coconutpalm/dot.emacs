(ns ui.gridlayout
  "Helpers for using SWT GridaLayout and GridData."
  (:require [ui.inits :refer [run-inits args->inits]]
            [clj-foundation.interop :refer [set-fields!]])
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

(defn- grid-data*
  [inits control]
  (let [parent        (.getParent control)
        parent-layout (.getLayout parent)
        layout-data   (or (.getLayoutData control) (GridData.))]
    (when-not (instance? GridLayout parent-layout)
      (throw (ex-info (str (.getSimpleName parent) ".layout = "
                           (if parent-layout
                             (.getSimpleName (class parent-layout))
                             "nil")
                           " but must be GridLayout."))))
    (run-inits layout-data inits)
    (.setLayoutData control layout-data)
    layout-data))

(defn grid-data
  "Construct and initialize a GridData on the specified control.  A GridLayout must
  have previously been set on the control's parent."
  [& more]
  (partial grid-data* (args->inits more)))

(defn cell-left-hgrab [& more]
  (fn [control]
    (let [layout-data (grid-data* [] control)]
      (set-fields!
       layout-data
       horizontalAlignment SWT/LEFT
       verticalAlignment SWT/CENTER
       grabExcessHorizontalSpace true
       grabExcessVerticalSpace false)
      (run-inits layout-data (args->inits more)))))

(defn cell-center-hgrab [& more]
  (fn [control]
    (let [layout-data (grid-data* [] control)]
      (set-fields!
       layout-data
       horizontalAlignment SWT/CENTER
       verticalAlignment SWT/CENTER
       grabExcessHorizontalSpace true
       grabExcessVerticalSpace false)
      (run-inits layout-data (args->inits more)))))

(defn cell-right-hgrab [& more]
  (fn [control]
    (let [layout-data (grid-data* [] control)]
      (set-fields!
       layout-data
       horizontalAlignment SWT/RIGHT
       verticalAlignment SWT/CENTER
       grabExcessHorizontalSpace true
       grabExcessVerticalSpace false)
      (run-inits layout-data (args->inits more)))))

(defn cell-vgrab [& more]
  (fn [control]
    (let [layout-data (grid-data* [] control)]
      (set-fields!
       layout-data
       horizontalAlignment SWT/FILL
       verticalAlignment SWT/FILL
       grabExcessHorizontalSpace false
       grabExcessVerticalSpace true)
      (run-inits layout-data (args->inits more)))))

(defn cell-grab-both [& more]
  (fn [control]
    (let [layout-data (grid-data* [] control)]
      (set-fields!
       layout-data
       horizontalAlignment SWT/FILL
       verticalAlignment SWT/FILL
       grabExcessHorizontalSpace true
       grabExcessVerticalSpace true)
      (run-inits layout-data (args->inits more)))))

(defn cell-left [& more]
  (fn [control]
    (let [layout-data (grid-data* [] control)]
      (set-fields!
       layout-data
       horizontalAlignment SWT/FILL
       verticalAlignment SWT/FILL
       grabExcessHorizontalSpace false
       grabExcessVerticalSpace false)
      (run-inits layout-data (args->inits more)))))

(defn cell-center [& more]
  (fn [control]
    (let [layout-data (grid-data* [] control)]
      (set-fields!
       layout-data
       horizontalAlignment SWT/CENTER
       verticalAlignment SWT/FILL
       grabExcessHorizontalSpace false
       grabExcessVerticalSpace false)
      (run-inits layout-data (args->inits more)))))

(defn cell-right [& more]
  (fn [control]
    (let [layout-data (grid-data* [] control)]
      (set-fields!
       layout-data
       horizontalAlignment SWT/RIGHT
       verticalAlignment SWT/FILL
       grabExcessHorizontalSpace false
       grabExcessVerticalSpace false)
      (run-inits layout-data (args->inits more)))))
