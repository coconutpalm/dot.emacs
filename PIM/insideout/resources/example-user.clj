(ns insideout.user
  (:require [clojure.pprint :refer [pprint]]
            [insideout.nrepl :as nrepl-server]
            [insideout.dynamo :as dynamo]
            [ui.SWT :refer [display]]))

(defn ui-scale! [factor]
  (let [multiplier (str factor)]
    (System/setProperty "sun.java2d.uiScale" multiplier)
    (System/setProperty "glass.gtk.uiScale" multiplier)))


(defn -main [& args]
  (println "Starting...")

  (ui-scale! 2)

  (pprint (nrepl-server/start! :cider :reveal))
  (println "nrepl started.")

  (loop []
    (Thread/sleep 1000)
    (recur)))
