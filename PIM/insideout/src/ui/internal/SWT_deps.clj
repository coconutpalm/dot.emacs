(remove-ns 'ui.internal.SWT-deps)

(ns ui.internal.SWT-deps
  "Dynamically resolve/load SWT subsystem dependencies when this namespace is required"
  (:require
   [insideout.dynamo :as dynamo]))


(def platform-lib-suffix
  (let [suffixes {"lin" 'gtk.linux.x86_64
                  "mac" 'cocoa.macosx.x86_64
                  "win" 'win32.win32.x86_64}
        os-code (-> (System/getProperty "os.name")
                   (.substring 0 3)
                   (.toLowerCase))]
    (str (get suffixes os-code "-unsupported-"))))

(defn ->platform-lib
  "Returns the full library dependency given a qualified group/archive symbol"
  [ga-symbol]
  (symbol (namespace ga-symbol) (str (name ga-symbol) "." platform-lib-suffix)))


;; P2 repositories -----------------------------------------------------------------

(defn eclipse-repo
  [& YYYY-MM]
  {"eclipse" {:id   "org.eclipse.p2.repo"
              :url  (str "https://download.eclipse.org/releases/" (or (first YYYY-MM) "2021-09") "/")
              :type "p2"}})

(def cef-chromium-repo
  {"chromium" {:id   "com.make"
               :url  "https://dl.maketechnology.io/chromium-cef/rls/repository/"
               :type "p2"}})


;; SWT and dependencies ------------------------------------------------------------

(def ^:dynamic *swt-version* "3.117.0")
(def swt-lib [(->platform-lib 'org.eclipse.platform/org.eclipse.swt) *swt-version*])

;; (def chromium [(->platform-lib 'com.make/com.make.chromium.cef) "0.4.0.202005172227"])
(def chromium '[com.make/com.make.chromium.cef.feature "0.4.0.202005172227"])
(def reflections-lib '[org.reflections/reflections "0.9.12"])

(defonce lib-resolutions
  (binding [dynamo/*extra-repositories* [cef-chromium-repo]]
    (dynamo/resolve-libs [swt-lib chromium reflections-lib])))


;; org.eclipse.swt.browser.chromium.

;; http://dl.maketechnology.io/chromium-cef/rls/repository/plugins/com.make.chromium.cef.gtk.linux.x86_64_0.4.0.202005172227.jar
;; http://dl.maketechnology.io/chromium-cef/rls/repository/plugins/com.make.chromium.cef.cocoa.macosx.x86_64_0.4.0.202005172227.jar
;; http://dl.maketechnology.io/chromium-cef/rls/repository/plugins/com.make.chromium.cef.win32.win32.x86_64_0.4.0.202005172227.jar


;; com.make.chromium.cef.feature_0.4.0.202005172227
