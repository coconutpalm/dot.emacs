(ns server.handler
  "A service-oriented network hub and switch."
  (:require
   [clojure.java.io                :as    io]
   [compojure.core                 :refer [defroutes GET]]
   [compojure.route                :refer [resources not-found]]
   [ring.middleware.defaults       :refer [wrap-defaults api-defaults]]
   [ring.middleware.anti-forgery   :refer [wrap-anti-forgery]]
   [ring.middleware.resource       :refer [wrap-resource]]
   [ring.util.response             :refer [content-type resource-response]]
   [ring.adapter.jetty9            :as j9]
   [clojure.core.async             :refer [go <! put! chan]]

   [util.maps                      :refer [letfn-map]]
   [util.jobs                      :refer :all]))


;; Websocket implementation
(defonce ws-to-web-tier (atom nil))

;; Async channels (lightweight threads) forwarding messages to-somewhere
(defonce to-web
  (let [conn (chan)]
    (go
      (loop []
        (let [next-msg (<! conn)]
          (when @ws-to-web-tier
            (j9/send! @ws-to-web-tier next-msg)))
        (recur)))
    conn))


(comment
  (start!)
  @ws-to-web-tier
  (j9/send! @ws-to-web-tier "{:message [\"Hello\"] }")
  (put! to-web "{:message [\"Hello\"] }")
  (map (fn [n] {(keyword (str "k" n)) (Math/round (* 100 (Math/random)))}) (range 1 10))
  (stop!)
  ,)


(defonce to-backend (chan))


(def web-tier-ws-events
  (letfn-map
   [(on-error
     [ws e]
     (println "WEB error:" e))

    (on-connect
     [ws]
     (println "WEB connect")
     (reset! ws-to-web-tier ws))

    (on-close
     [ws status-code reason]
     (println "WEB close" status-code reason)
     (reset! ws-to-web-tier nil))

    (on-text
     [ws text-message]
     (put! to-backend text-message))

    (on-bytes
     [ws bytes offset len]
     (println (str "WEB binary: not implemented.  (" offset len ")")))]))


;; --------------------------------------------------------------------------------

;; HTTP server endpoints
(defroutes app-routes
  (GET "/" req (-> "index.html"
                  (resource-response)
                  (content-type "text/html")))

  (resources "/" {:root ""})

  (not-found (or (io/resource "public/404.html")
                 "Oups! This page doesn't exist! (404 error)")))


;; HTTP universal request handler
(def app
  (-> app-routes
     (wrap-defaults api-defaults)
     (wrap-anti-forgery)
     (wrap-resource "public")))

;; The web-tier's state + start/stop
(defonce web-tier (atom nil))


(defn start! []
  (when-not @web-tier
    (reset! web-tier (j9/run-jetty app {:join? false :port 8000 :websockets {"/ws/" web-tier-ws-events}}))))

(defn stop! []
  (when @web-tier
    (swap! web-tier (fn [web-tier] (.stop web-tier) nil))))

(defn -main
  "public static void main..."
  [] (start!))
