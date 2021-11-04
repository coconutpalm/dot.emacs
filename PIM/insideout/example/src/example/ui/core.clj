(ns example.ui.core
  {:on-pre-reload 'pre-reload
   :on-post-reload 'post-reload}
  (:refer-clojure :exclude [list])
  (:require [ui.SWT :refer :all]
            [ui.gridlayout :as layout]
            [example.ui.console :as console]
            [example.ui.styles :as s])
  (:import [org.eclipse.swt SWT]
           [org.eclipse.swt.graphics Font FontData]
           [org.eclipse.swt.widgets Display]
           [org.eclipse.swt.layout FillLayout]))


(comment
  "Evolve into defcompopnent?"

(defn add-contents! [parent component]
  (let [props (atom {})]
    (component props parent)))

  ,)


(defn add-content []
  (sash-form SWT/HORIZONTAL (id! :ui/main-sash)
             :sash-width (int (/ s/margin 2))
             (view-form (| SWT/BORDER SWT/FLAT)
                        :margin-height s/margin
                        :margin-width s/margin
                        :background (s/system-color SWT/COLOR_INFO_BACKGROUND)
                        :foreground (s/system-color SWT/COLOR_INFO_FOREGROUND)

                        (composite (id! :ui/sidebar-title)
                                   :layout (FillLayout.)
                                   :background (s/system-color SWT/COLOR_TITLE_BACKGROUND)
                                   (label "Contexts && Things"
                                          ;; :font (title-font)
                                          :foreground (s/system-color SWT/COLOR_TITLE_FOREGROUND)))
                        :top-left :ui/sidebar-title)

             (text (| SWT/MULTI SWT/V_SCROLL) (id! :ui/textpane)
                   (on-modify-text [props _] (println (.getText (:ui/textpane @props)))))

             #_(browser SWT/WEBKIT (id! :ui/editor)
                        :javascript-enabled true
                        (fn [_ parent] (console/init parent))
                        #_(-> (swtdoc :swt :program 'Program) :result :eclipsedoc))

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
