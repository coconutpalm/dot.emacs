(ns ui.SWT
  (:require
   [clojure.core :refer :all]
   [util.dynamo :as dynamo]))

(def platform-swt-lib
  "The Maven coordinates of the current platform's SWT library."
  (let [os-fullname (System/getProperty "os.name")
        os (-> os-fullname
              (.substring 0 3)
              (.toLowerCase))]
    (cond
      (= os "lin") '[org.eclipse.swt/org.eclipse.swt.gtk.linux.x86_64    "4.3"]
      (= os "mac") '[org.eclipse.swt/org.eclipse.swt.cocoa.macosx.x86_64 "4.3"]
      (= os "win") '[org.eclipse.swt/org.eclipse.swt.win32.win32.x86_64  "4.3"]
      :else        (throw (ex-info (str "Unsupported OS: " os-fullname))))))

(defonce swt-lib-resolution
  (dynamo/import-dependencies [platform-swt-lib]
                              '[[org.eclipse.swt.widgets Display Shell]
                                [org.eclipse.swt SWT]]))

#_(defonce display (Display.))



(comment
  platform-swt-lib

  (dynamo/import-dependencies [platform-swt-lib]
                              '[org.eclipse.swt.widgets Display Shell])

  (dynamo/import-dependencies [platform-swt-lib]
                              '[[org.eclipse.swt.widgets Display Shell]
                                [org.eclipse.swt SWT]])

  (add-dependencies :coordinates (conj [] platform-swt-lib)
                    :repositories cemerick.pomegranate.aether/maven-central)
  ,)
