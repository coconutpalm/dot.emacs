(ns main.core
  "Generic program launcher."
  (:require
   [clojure.java.io                :as    io]
   [clojure.core.async             :refer [go <! put! chan]]

   [util.dynamo                    :as    dyn]
   [ui.SWT                         :as    swt]

   [util.maps                      :refer [letfn-map]]
   [util.jobs                      :refer :all]))


(defn start! []
  (println "Starting..."))

(defn -main
  "public static void main..."
  [] (start!))
