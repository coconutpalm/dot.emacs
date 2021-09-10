(ns ui.SWT-deps
  (:require
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

(def reflections-lib '[org.reflections/reflections "0.9.12"])

(defonce lib-resolutions
  (dynamo/resolve-libs [swt-lib reflections-lib]))
