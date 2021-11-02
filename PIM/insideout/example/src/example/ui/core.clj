(ns example.ui.core
  {:on-pre-reload 'pre-reload
   :on-post-reload 'post-reload}
  (:refer-clojure :exclude [list])
  (:require [ui.SWT :refer :all]
            [ui.gridlayout :as layout])
  (:import [org.eclipse.swt SWT]
           [org.eclipse.swt.widgets Display]
           [org.eclipse.swt.layout FillLayout]))

(defn add-content []
  (sash-form SWT/VERTICAL (id! :ui/main-sash)
             (text (| SWT/MULTI SWT/V_SCROLL) (id! :ui/textpane)
                   (on-modify-text [props _] (println (.getText (:ui/textpane @props)))))

             (browser SWT/WEBKIT (id! :ui/editor)
                      :javascript-enabled true
                      :url (-> (swtdoc :swt :program 'Program) :result :eclipsedoc))

             :weights [20 80]))


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
