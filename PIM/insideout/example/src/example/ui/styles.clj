(ns example.ui.styles
  (:require [ui.SWT :refer :all]
            [ui.gridlayout :as l]
            [clojure.pprint :refer [pprint]])
  (:import [org.eclipse.swt SWT]
           [org.eclipse.swt.graphics Font FontData]
           [org.eclipse.swt.widgets Display]
           [org.eclipse.swt.layout FillLayout]))


(def grid-margins 20)
(def elem-separation (* 2 grid-margins))

(def margin 10)

(defn system-color [name]
  (ui (-> (Display/getDefault) (.getSystemColor name))))

(defn system-font []
  (ui (-> (Display/getDefault) (.getSystemFont))))


(def title-font
  (memoize (fn []
             (ui (let [system-font-data (-> (system-font) (.getFontData) (first))]
                   (Font. (Display/getDefault)
                          (FontData. (.getName system-font-data)
                                     (* 2 (.getSize system-font-data))
                                     SWT/NONE)))))))


(defn grid-layout [cols are-equal-width]
  (l/grid-layout
   :num-columns cols
   :make-columns-equal-width are-equal-width
   :margin-top grid-margins
   :margin-bottom grid-margins
   :margin-left grid-margins
   :margin-right grid-margins
   :horizontal-spacing elem-separation
   :vertical-spacing elem-separation))
