(ns web.component.busy
  (:require [hoplon.core :refer [div pre text]]
            [javelin.core :refer [defc defc= cell=]]))


(defc product "")
(defc busy? false)
(defc busy-msg (str "$ " (or product "application") "\nLoading"))

(defn- unbusy [& [then]]
  (reset! busy? false)
  (when then (then)))

(defn show-busy
  "Run a unary function accepting a continuation while displaying the busy window."
  [f msg & [then]]
   (reset! busy? true)
   (reset! busy-msg msg)
   (f #(unbusy then)))


(def twirly-chars ["|" "/" "-" "\\"])
(defc twirl "\\")
(defn twirly [ch] (reset! twirl ch))

(cell= (when busy?
         (letfn [(twirl-on [[next-twirl & more]]
                   (twirly next-twirl)
                   (when busy?
                     (let [next-twirly (if (first more) more twirly-chars)]
                       (.setTimeout js/window #(twirl-on next-twirly) 100))))]

           (twirl-on twirly-chars))))


;; Put busy-twirly somewhere in the UI
(defonce twirly-pane (div :class "twirly mui-panel mui--z5"
                          (pre :class "monospace-font" (text "~{busy-msg}...~{twirl}") )))
(defc= busy-twirly (when busy? twirly-pane))
