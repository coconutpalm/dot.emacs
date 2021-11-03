(ns example.ui.core
  {:on-pre-reload 'pre-reload
   :on-post-reload 'post-reload}
  (:refer-clojure :exclude [list])
  (:require [ui.SWT :refer :all]
            [ui.gridlayout :as layout])
  (:import [org.eclipse.swt SWT]
           [org.eclipse.swt.graphics Font FontData]
           [org.eclipse.swt.widgets Display]
           [org.eclipse.swt.layout FillLayout]))


(def margin 15)

(defn system-color [name]
  (-> (Display/getDefault) (.getSystemColor name)))

(defn system-font []
  (-> (Display/getDefault) (.getSystemFont)))

(defn system-font-data [] (-> (system-font) (.getFontData) (first)))

(defn title-font [] (Font. (Display/getDefault)
                           (FontData. (.getName system-font-data)
                                      (* 2 (.getSize system-font-data))
                                      SWT/NONE)))

(defn add-content []
  (sash-form SWT/HORIZONTAL (id! :ui/main-sash)
             :sash-width (int (/ margin 2))
             (view-form (| SWT/BORDER SWT/FLAT)
                        :margin-height margin
                        :margin-width margin
                        :background (system-color SWT/COLOR_INFO_BACKGROUND)
                        :foreground (system-color SWT/COLOR_INFO_FOREGROUND)

                        (composite (id! :ui/sidebar-title)
                                   :layout (FillLayout.)
                                   :background (system-color SWT/COLOR_TITLE_BACKGROUND)
                                   (label "Contexts && Things"
                                          ;; :font (title-font)
                                          :foreground (system-color SWT/COLOR_TITLE_FOREGROUND)))
                        :top-left :ui/sidebar-title)
             #_(text (| SWT/MULTI SWT/V_SCROLL) (id! :ui/textpane)
                     (on-modify-text [props _] (println (.getText (:ui/textpane @props)))))

             (browser SWT/WEBKIT (id! :ui/editor)
                      :javascript-enabled true
                      :url "http://www.google.com" #_(-> (swtdoc :swt :program 'Program) :result :eclipsedoc))

             :weights [25 75]))


(defn pre-reload []
  (ui
   (let [props (first (root-props))
         sash (:ui/main-sash @props)]
     (.dispose sash))))

(defn post-reload []
  (ui
   (let [props (first (root-props))
         shell (first (.getShells (Display/getDefault)))]
     (child-of shell props (add-content))
     (.layout shell))))
