(ns example.ui.styles
  (:require [ui.SWT :refer :all]
            [ui.gridlayout :as l]
            [clojure.pprint :refer [pprint]]
            [insideout.nrepl :as nrepl-server]
            [insideout.dynamo :as dynamo]
            [insideout.reload :as reload]
            [example.ui.core  :as ui])
  (:import [org.eclipse.swt SWT]
           [org.eclipse.swt.graphics Font FontData Image]
           [org.eclipse.swt.widgets Display]
           [org.eclipse.swt.layout FillLayout]))


(def icon (ui (Image. (Display/getDefault) "resources/sidebar.png")))

(def grid-margins 20)
(def elem-separation (* 2 grid-margins))

(def margin 10)

(defn system-color [name]
  (-> (Display/getDefault) (.getSystemColor name)))

(defn system-font []
  (-> (Display/getDefault) (.getSystemFont)))

(defn system-font-data [] (-> (system-font) (.getFontData) (first)))

(defn title-font [] (Font. (Display/getDefault)
                           (FontData. (.getName system-font-data)
                                      (* 2 (.getSize system-font-data))
                                      SWT/NONE)))

(defn grid-layout [cols equal-width]
  (l/grid-layout
   :num-columns cols
   :make-columns-equal-width equal-width
   :margin-top grid-margins
   :margin-bottom grid-margins
   :margin-left grid-margins
   :margin-right grid-margins
   :horizontal-spacing elem-separation
   :vertical-spacing elem-separation))
