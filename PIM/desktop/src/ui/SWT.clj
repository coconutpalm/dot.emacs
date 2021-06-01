(ns ui.SWT
  (:require
   [clojure.core :refer :all]
   [util.dynamo :as dynamo]))


(defonce os-code
  (let [os-fullname (System/getProperty "os.name")]
    (-> os-fullname
       (.substring 0 3)
       (.toLowerCase))))


(def platform-swt-lib
  "The Maven coordinates of the current platform's SWT library."
  (cond
    (= os-code "lin") '[org.eclipse.swt/org.eclipse.swt.gtk.linux.x86_64 "4.3"]
    (= os-code "mac") '[org.eclipse.swt/org.eclipse.swt.cocoa.macosx.x86_64 "4.3"]
    (= os-code "win") '[org.eclipse.swt/org.eclipse.swt.win32.win32.x86_64  "4.3"]
    :else              (throw (ex-info (str "Unsupported OS: " (System/getProperty "os.name"))))))


(defonce swt-lib-resolution
  (dynamo/import-dependencies [platform-swt-lib]
                              ['[org.eclipse.swt.widgets Display Shell]
                               '[org.eclipse.swt SWT]]))

(defonce display
  (if-not (= os-code "mac")
    (Display.)))


(comment
  (clojure.core/require
   '[clojure.core :refer :all]
   '[clojure.repl :as repl]
   '[ui.SWT :as swt])

  swt/platform-swt-lib

  (dynamo/import-dependencies [platform-swt-lib]
                              '[org.eclipse.swt.widgets Display Shell])

  (dynamo/import-dependencies [platform-swt-lib]
                              '[[org.eclipse.swt.widgets Display Shell]
                                [org.eclipse.swt SWT]])

  (add-dependencies :coordinates (conj [] platform-swt-lib)
                    :repositories cemerick.pomegranate.aether/maven-central)
  ,)
