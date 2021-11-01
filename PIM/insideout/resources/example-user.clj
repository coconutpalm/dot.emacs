(ns insideout.user
  (:refer-clojure :exclude [list])
  (:require [clojure.pprint :refer [pprint]]
            [insideout.nrepl :as nrepl-server]
            [insideout.dynamo :as dynamo]
            [insideout.reload :as reload]
            [ui.SWT :refer :all]
            [ui.gridlayout :as layout])
  (:import [org.eclipse.swt SWT]))


(defn -main [& args]
  (println "Starting...")

  (pprint (nrepl-server/start! :cider))
  (println "nrepl started.")

  (reload/start-reload-watcher)
  (println "Watching classpath directories for source changes.")

  (ui-scale! 2)                         ;Needed for older JREs

  (application
   (shell "Browser" (id! :ui/shell)
          :layout (FillLayout.)

          (sash-form SWT/VERTICAL
                     (text (| SWT/MULTI SWT/V_SCROLL) (id! :ui/textpane)
                           (on-modify-text [props _] (println (.getText (:ui/textpane @props)))))

                     (browser SWT/WEBKIT (id! :ui/editor)
                              :javascript-enabled true
                              :url (-> (swtdoc :swt :program 'Program) :result :eclipsedoc))

                     :weights [20 80])

          (on-shell-closed [props event] (when-not (:closing @props)
                                           (set! (. event doit) false)))

          (menu SWT/POP_UP (id! :ui/tray-menu)
                (menu-item SWT/PUSH "&Quit"
                           (on-widget-selected [props _]
                                               (swap! props #(update-in % [:closing] (constantly true)))
                                               (.close (:ui/shell @props))))))

   (tray-item
    (on-menu-detected [props _]   (.setVisible (:ui/tray-menu @props) true))
    (on-widget-selected [props _] (let [s (:ui/shell @props)]
                                    (if (.isVisible s)
                                      (.setVisible s false)
                                      (.setVisible s true)))))

   (defmain [props parent]
     ;; Bind data layer to UI or...
     (println (str (:ui/editor @props) " " parent)))))
