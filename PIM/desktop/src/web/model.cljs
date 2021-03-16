(ns web.model
  "A reactively-designed model that coordinates server communication and various
  parts of the UI."
  (:require
   [clojure.core.async :refer [go <!]]
   [javelin.core :refer [defc defc= cell= cell]]
   [oops.core :refer [oget oset! ocall oapply ocall! oapply!
                      oget+ oset!+ ocall+ oapply+ ocall!+ oapply!+]]
   [util.jobs :as job]
   [web.page.websocket :as ws]
   [web.page.history :as h]))


;; -----------------------------------------------------------------------------------
;; This is purely documentation illustrating the data structures used in the file tree

; (defonce file-tree-data-format
;   [(* (or :file-node-format :dir-node-format))])

; (defonce file-node-format
;   {:type "file"
;    :name "looping-purely.md"
;    :path "/src/looping-purely.md"})

; (defonce dir-node-format
;   {:type "dir"
;    :name "src"
;    :path "/src"
;    :children :file-tree-data-format})

;; ------------------------------------------------------------------------------------

(defn sec->millis [s] (* s 1000))

; Yeah I'm too lazy to translate this to cljs
(js/eval
 (str
  "function uuidv4() {\n"
  "  return ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>\n"
  "    (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)\n"
  "  );\n"
  "}"))


(defonce client-id (js/uuidv4))

(defonce websocket-connection
  (job/start-and-monitor
   :start-fn          ws/start!
   :is-started        #(= @ws/state :connected)
   :patience-timeout  (sec->millis 10)
   :stop-fn           ws/stop!
   :cooldown-millis   (sec->millis 5)
   :heartbeat-millis  (sec->millis 5)
   :state             (cell :stopped)))

(defc= monitor-conn (println websocket-connection))


;; The stuff we display in the directory tree goes here.  Data format is above.
(defc file-tree [])

(def server-file {::file #{:name :path}})
(def server-dir {::dir #{:name :path :children}})


(defc ping-jstimeout nil)
(def ping-heartbeat-millis (sec->millis 10))


(defn kill-heartbeat! []             (swap! ping-jstimeout
                                            (fn [timeout]
                                              (when timeout
                                                (js/clearTimeout timeout))
                                              nil)))

(defn start-heart! [next-heartbeat]  (swap! ping-jstimeout
                                          (fn [timeout]
                                            (when timeout
                                              (js/clearTimeout timeout))
                                            (job/on-timeout ping-heartbeat-millis next-heartbeat))))

(defn heartbeat! []                  (swap! ping-jstimeout
                                            (fn [timeout]
                                              (ws/send! [:ping!])
                                              nil)))

(defc= keepalive-ping
  (if (= (first ws/connection-state) :connected)  ; Don't try to ping when not connected
    (letfn [(send-heartbeat! []
              (heartbeat!)
              (start-heart! send-heartbeat!))]
      (send-heartbeat!))
    (kill-heartbeat!)))


(defc= ws-just-opened
  (when (= (first ws/connection-state) :connected)
    (println "Asking for initial UI state")
    (ws/send! [:get-initial-state])))


;; The SimpleMDE/CodeMirror instance.  Since this is a cell, when this value is set
;; at the beginning of the program, that automatically kicks off the rest of the
;; model initialization.
(defc editor nil)

(defc= append-server-messages
  (when (and editor ws/message-as-edn)
    (let [cm (.-codemirror editor)]
      (.setValue cm (str (.getValue cm) "\n```edn\n" ws/message-as-edn "\n```\n")))))

;; The file-node-format of the file currently open for editing or an empty map
;; for an untitled file.
(defc editor-content {})

;; A spreadsheet formula that automatically pulls the short filename out of editor-content.
;; Used in the UI.
(defc= document-name (or (-> editor-content :name) "Untitled"))


(defn request-server-file
  "Load a file from the server into the editor via the cells above."
  [path]
  (when (= @websocket-connection :running)
    (println "Load file from server")
    (ws/send! [:get {:java.io/file #{[:path path]
                                     :name :path :contents}}])))


;; Markdown with Scala code blocks can be processed by Mdoc to produce results inline on save
;; This is where those save results live where they're picked up by the Markdown previewer below
(defc mdocfile "")


(defn save-server-file!
  "Save an edited file to the server."
  [path content]
  (when (= @websocket-connection :running)
    (println "Saving: " path)
    (ws/send! [:put {:java.io/file #{[:path path :contents content]}}])))


(def autosave-pause-millis 1000)

(defn init-autosave! [editor codemirror]
  (let [current-file-path (cell= (:path editor-content))
        noop (fn [])
        cancel-autosave-if-pending (atom noop)]

    (letfn [(on-document-change [next-thing]
              (.on (.getDoc codemirror) "change" next-thing))

            (save! []
              (println "Autosave: " @current-file-path)
              (when @current-file-path
                (save-server-file! current-file-path (.value editor))
                (reset! cancel-autosave-if-pending noop)))

            (save-on-typing-pause! []
              (on-document-change (fn [] (let [new-autosave-job (job/on-timeout autosave-pause-millis save!)]
                                          (@cancel-autosave-if-pending)
                                          (reset! cancel-autosave-if-pending
                                                  (fn [] (println "Cancelling autosave")
                                                    (js/clearTimeout new-autosave-job)))))))]

      (save-on-typing-pause!))))



;; Init this model and editor autosave.
;;
;; Since this is a cell that depends on the editor's cell, it automatically
;; initializes after the editor does.
(cell= (when (and editor (exists? editor) (exists? (.-codemirror editor)))
         (init-autosave! editor (.-codemirror editor))))


;; Respond to user requests to load a new file into the editor


;; A cell that tracks the browser's URL hash/query string and triggers file loads
(defonce route-history (h/history-cell))

;; Formula cell: triggered when the user types a file path into the URL bar or clicks a tree item
(cell= (println route-history)
       (request-server-file route-history))

;; Called on <enter> in the tree search field
;; TODO: Unify this with route-history
(defn file-selected [tree-item]
  (println tree-item)
  (request-server-file (:path tree-item)))
