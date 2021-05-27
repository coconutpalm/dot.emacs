(ns web.page.websocket
  "Simple websocket library around Google Closure's WebSocket"
  (:require
   [javelin.core :as j :refer [defc defc= cell cell=]]
   [clojure.edn :as edn]
   [oops.core :refer [oget oset! ocall oapply ocall! oapply!
                      oget+ oset!+ ocall+ oapply+ ocall!+ oapply!+]]))


;; Override this to change the server URL entirely
(defn server-host [] (.. js/location -host))
(defn server-url [] (str "ws://" (server-host) "/ws/"))

;; Cells tracking the connection state and the latest message/response
(defc connection-state [:start "Starting"])
(defc= state (first connection-state))
(def connection (atom nil))


;; The raw message event
(defc message-as-string "")

;; Semi-processed input
(def malformed-edn :websocket/malformed-edn)

(defc= message-as-edn (try
                        (edn/read-string message-as-string)
                        (catch js/Object e {:websocket/malformed-edn e})))


(defn start! []
  (let [websocket (js/WebSocket. (server-url))]
    (println "opening connection")
    (reset! connection-state [:connect (server-url)])

    (oset! websocket "onmessage" (clj->js (fn [e] (reset! message-as-string (.-data e)) (println e))))
    (oset! websocket "onopen" (clj->js #(reset! connection-state [:connected %])))
    (oset! websocket "onclose" (clj->js #(reset! connection-state [:offline %])))
    (oset! websocket "onerror" (clj->js #(println (.-message %))))

    (swap! connection
           (fn [old-ws]
             (when old-ws (.close old-ws))
             websocket))

    websocket))


(defn stop! []
  (swap! connection
         (fn [old-ws]
           (reset! connection-state [:offline "Disconnecting"])
           (.close old-ws))))



;; Output
(defn send! [message]
  (if (= :connected (first @connection-state))
    (.send @connection (pr-str message))
    (println (str "Can't send: Status: [" (first @connection-state) "]: " message))))
