(ns shell.workbench
  (:refer-clojure :exclude [list])
  (:require [ui.SWT :refer :all]
            [clj-foundation.interop :refer [array]])
  (:import [org.eclipse.swt.layout FillLayout]
           [org.eclipse.swt SWT]))


(defn open
  [window-title setup-ui]
  (application
   (shell window-title (id! :ui/shell)
          :layout (FillLayout.)

          (sash-form
           (composite (id! :ui/sidebar) :layout (FillLayout.))
           (composite (id! :ui/body)    :layout (FillLayout.))
           :weights (array [Integer/TYPE] 20 80))

          (on-shell-closed [props event] (when-not (:closing @props)
                                           (set! (. event doit) false)))

          (menu SWT/POP_UP (id! :ui/tray-menu)
                (menu-item SWT/PUSH "&Quit"
                           (on-widget-selected [props _]
                                               (swap! props #(update-in % [:closing] (constantly true)))
                                               (.close (:ui/shell @props))))))

   (tray-item (id! :ui/tray)
              (on-menu-detected [props _]   (.setVisible (:ui/tray-menu @props) true))
              (on-widget-selected [props _] (let [s (:ui/shell @props)]
                                              (if (.isVisible s)
                                                (.setVisible s false)
                                                (.setVisible s true)))))

   (defmain [props parent]
     (setup-ui props))))


(comment
  (open
   "Test"
   (fn [props]
     (let [p       @props
           sidebar (:ui/sidebar p)
           body    (:ui/body p)]

       (child-of [body props]
                 (ctab-folder
                  (id! :ui.body/folder)

                  (browser SWT/WEBKIT (id! :ui/editor)
                           :javascript-enabled true
                           :url "http://localhost:8002")
                  (ctab-item "Editor"
                             :control :ui/editor)

                  :selection 0)))))

  ,)
