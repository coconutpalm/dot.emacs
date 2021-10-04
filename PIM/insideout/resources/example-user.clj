(ns insideout.user
  (:refer-clojure :exclude [list])
  (:require [clojure.pprint :refer [pprint]]
            [insideout.nrepl :as nrepl-server]
            [insideout.dynamo :as dynamo]
            [ui.SWT :refer :all]
            [ui.gridlayout :as layout])
  (:import [org.eclipse.swt SWT]))


(defn -main [& args]
  (println "Starting...")

  (pprint (nrepl-server/start! :cider :reveal))
  (println "nrepl started.")

  (ui-scale! 2)

  (application
   (shell "Example SWT app"
          (layout/grid-layout :num-columns 2 :make-columns-equal-width false)

          (label "A. Label"
                 (layout/align-left))
          (combo SWT/BORDER
                 :items ["one" "two" "three" "Default value" "four"]
                 :select 1
                 (layout/hgrab))

          (group "Example group"
                 (id! :name)
                 (layout/align-left :horizontal-span 2)

                 (layout/grid-layout :num-columns 2 :make-columns-equal-width false)
                 (label "A. Label"
                        (layout/align-left))
                 (text SWT/BORDER "Default text"
                       (layout/align-left)
                       (id! :default-text))))

   (main
    (fn [props _]
      (let [t (:default-text @props)]
        ;; Set up event handlers, etc...
        (println t))))))
