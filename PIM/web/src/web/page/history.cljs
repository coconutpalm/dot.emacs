(ns web.page.history
  (:require [goog.events :as events]
            [javelin.core :as j])
  (:import [goog History Uri]
           [goog.history Html5History EventType]))

(defn mkhistory [opts]
  (if (Html5History.isSupported)
    (doto (Html5History.)
      (.setPathPrefix  (:prefix   opts ""))
      (.setUseFragment (:fragment opts true)))
    (History.)))

(def ^:dynamic *history* (mkhistory {}))

(defn history-cell
  "A cell analagous to hoplon.core/route-cell, using Google Closure History API.
  Optionally supports `:prefix` which will set an Html5History prefix.
  Optionally supports `:fragment` which will include page fragments in HTML5 browsers, default `true`.
  This keeps routing backwards compatible with older browsers."
  [& {:keys [history prefix fragment] :as opts}]
  (let [history  (or history *history*)
        historyc (j/cell (.getToken history))]
    (j/with-let [_ (j/cell= historyc (fn [token] (.setToken history token)))]
      (events/listen history EventType.NAVIGATE
                     (fn [event] (reset! historyc (.-token event))))
      (.setEnabled history true))))
