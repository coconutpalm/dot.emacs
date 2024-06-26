(ns insideout.user
  (:refer-clojure :exclude [list])
  (:require [ui.SWT :refer :all]
            [ui.gridlayout :as layout]
            [clojure.pprint :refer [pprint]]
            [insideout.nrepl :as nrepl-server]
            [insideout.dynamo :as dynamo]
            [insideout.reload :as reload]
            [example.ui.core  :as ui])
  (:import [org.eclipse.swt SWT]
           [org.eclipse.swt.graphics Image]
           [org.eclipse.swt.widgets Display]
           [org.eclipse.swt.layout FillLayout]))


(defn -main [& args]
  (let [app-icon (Image. (Display/getDefault) "resources/img/sidebar.png")]
    (println "Starting...")

    (pprint (nrepl-server/start! :cider))
    (println "nrepl started.")

    (reload/reload-classpath-dirs)
    (println "Watching classpath directories for source changes.")

    (ui-scale! 2)                       ;Needed for older JREs

    (application
     (shell "FuseCode" (id! :ui/shell)
            :image app-icon
            :layout (FillLayout.)

            (ui/add-content)

            (on-shell-closed [props event]
                             (when-not (:closing @props)
                               (set! (. event doit) false))
                             (.setVisible (first (.getShells (Display/getDefault))) false))

            (menu SWT/POP_UP (id! :ui/tray-menu)
                  (menu-item SWT/PUSH "&Quit"
                             (on-widget-selected [props _]
                                                 (swap! props #(update-in % [:closing] (constantly true)))
                                                 (.close (:ui/shell @props))))))

     (tray-item
      (fn [props parent]
        (.setImage parent app-icon)
        (.setHighlightImage parent app-icon))
      (on-menu-detected [props _]   (.setVisible (:ui/tray-menu @props) true))
      (on-widget-selected [props _] (let [s (:ui/shell @props)]
                                      (if (.isVisible s)
                                        (.setVisible s false)
                                        (.setVisible s true)))))

     (defmain [props parent]
       ;; Bind data layer to UI or...
       (println (str (:ui/editor @props) " " parent))))))
