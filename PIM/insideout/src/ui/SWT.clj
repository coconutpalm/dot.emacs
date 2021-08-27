(ns ui.SWT
  (:require
   [clojure.core :refer :all]
   [insideout.dynamo :as dynamo]))


;; ============================================================================
;; Dynamically resolve/load SWT lib when this namespace is required
;; ============================================================================

(defonce os-code
  (let [os-fullname (System/getProperty "os.name")]
    (-> os-fullname
       (.substring 0 3)
       (.toLowerCase))))

;; Sure would be nice to have a newer version available through Maven...
(def swt-version "4.3")

(defn swt-coordinates [platform]
  [(symbol "org.eclipse.swt" (str "org.eclipse.swt." (str platform))) swt-version])

(def swt-libs
  {"lin" (swt-coordinates 'gtk.linux.x86_64)
   "mac" (swt-coordinates 'cocoa.macosx.x86_64)
   "win" (swt-coordinates 'win32.win32.x86_64)})

(def swt-lib (get swt-libs os-code
                  (str "Unsupported OS: " (System/getProperty "os.name"))))

(defonce swt-lib-resolution
  (dynamo/import-libs [swt-lib]
                      ['[org.eclipse.swt.widgets Display Shell]
                       '[org.eclipse.swt SWT]]))

(def display (Display/getDefault))

;; ============================================================================
;; UI DSL for SWT based on XSWT and XScalaWT
;; ============================================================================

;; f: [parent setups] child

(comment
  (clojure.core/require
   '[clojure.core :refer :all]
   '[clojure.repl :as repl]
   '[ui.SWT :as swt])

  swt/platform-swt-lib

  (dynamo/import-libs [platform-swt-lib]
                              '[org.eclipse.swt.widgets Display Shell])

  (dynamo/import-libs [platform-swt-lib]
                              '[[org.eclipse.swt.widgets Display Shell]
                                [org.eclipse.swt SWT]])

  ,)
