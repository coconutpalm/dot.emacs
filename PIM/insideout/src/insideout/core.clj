(ns insideout.core
  "InsideOut program launcher."
  (:require
   [clojure.java.io                :as    io]
   [clojure.core.async             :refer [go <! put! chan]]

   [insideout.dynamo               :as    dyn]
   [insideout.nrepl                :as    nr]
   [ui.SWT                         :as    swt]

   [util.maps                      :refer [letfn-map]]
   [util.jobs                      :refer :all]))


;; TODO
;;
;; Grab Boot's task framework and -main argument parser.
;; Then `nrepl` becomes a task; `dynamo` (maybe) becomes a task.
;; Programmers can write their own tasks.  `params` is a
;; task for passing command-line arguments?

;; Find sources via searching `src/main/clojure` and `src`.
;; `app.clj` is the program's main.  Search for it in the
;; source folders, require it, and call its `app-main`.

;; Make a small fast native executable that communicates with
;; dynamo over a unix domain socket (?) for monitoring/operability?


(defn -main
  "public static void main..."
  [args]
  (dyn/resolve-sources)
  (nr/start!))
