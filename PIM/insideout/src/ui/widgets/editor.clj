(remove-ns 'ui.widgets.editor)

(ns ui.widgets.editor
  (:refer-clojure :exclude [list])
  (:require [ui.inits :as i]
            [ui.SWT :refer :all]
            [clj-foundation.data :refer [nothing->identity]])
  (:import [org.eclipse.swt SWT]
           [org.eclipse.swt.browser Browser]
           [org.eclipse.swt.graphics Rectangle]
           [org.eclipse.swt.events ModifyListener VerifyListener]
           [org.eclipse.swt.widgets Composite Menu]
           [org.eclipse.swt.layout FillLayout]))


(definterface Editor
  (setFocus [this])
  (setLayoutData [this ^Object data])
  (setMenu [this  ^Menu menu])
  (setParent [this ^Composite parent])
  (setSelection [this ^Rectangle bounds])
  (clearSelection [this])
  (setText [this ^String text])
  (getLayoutData [this])
  (getMenu [this])
  (getParent [this])
  (getSelection [this])
  (getText [this])
  (addModifyListener [this ^ModifyListener l])
  (addVerifyListener [this ^VerifyListener l])
  (removeModifyListener [this ^ModifyListener l])
  (removeVerifyListener [this ^VerifyListener l])
  (cut [this])
  (copy [this])
  (paste [this]))


(defn composite-editor [parent style]
  (let [browser (ref nil)
        composite (proxy [Composite Editor] [parent style]
                    (setFocus [this]
                      (.setFocus @browser))
                    (setSelection [this ^Rectangle bounds])
                    (clearSelection [this])
                    (setText [this ^String text])
                    (getSelection [this])
                    (getText [this])
                    (addModifyListener [this ^ModifyListener l])
                    (addVerifyListener [this ^VerifyListener l])
                    (removeModifyListener [this ^ModifyListener l])
                    (removeVerifyListener [this ^VerifyListener l])
                    (cut [this])
                    (copy [this])
                    (paste [this]))
        b (new Browser composite SWT/WEBKIT)]
    (reset! browser b)
    composite))


(defn codemirror
  "Returns Code Mirror 6 inside a SWT Composite.  Implements the Editor protocol"
  [& more]
  (let [[style
         args] (i/extract-style-from-args more)
        style (nothing->identity SWT/NULL style)
        args (or args [])]
    (fn [props ^Composite parent]
      (let [child (composite-editor parent style)
            inits (i/args->inits args)]
        (i/run-inits props child inits)
        child))))
