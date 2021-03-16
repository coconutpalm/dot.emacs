(ns util.jobs
  (:require #?(:clj  [clojure.core.async :refer [<! put! chan go]]
               :cljs [cljs.core.async :refer [<! put! chan]])
            #?(:cljs [javelin.core :refer [defc defc= cell= cell]]))

  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))


;; A central place to get in-progress task information for all UIs
#?(:cljs (defc current-job-name "")
   :clj  (def current-job-name (atom "")))

#?(:cljs (defc current-task-number 0)
   :clj  (def current-task-number (atom 0)))

#?(:cljs (defc number-of-tasks 0)
   :clj  (def number-of-tasks (atom 0)))


(def job-schema
  {:name string?
   :initial-size number?
   :steps [#_(-> nil ['continuation-fn])]})


(defn with-blocking
  "Run (f args), supplying all but the last argument of f (for functions where the last argument of f is the function
  to call with f's result).  Automatically calls f, blocks, captures f's results, and returns a sequence containing
  the arguments passed to f's continuation function.  If nil is the result passed to f's continuation function,
  \"<<SUCCESS>>\" is returned instead."
  [f & args]
  (let [pf (if args (apply partial f args) f)
        continuation (chan)]
    (go
      (pf (fn [& result] (put! continuation (or result "<<SUCCESS>>"))))
      (<! continuation))))


(defn- run-tasks [tasks on-complete]
  (if-let [task (first tasks)]
    (do
      (swap! current-task-number inc)
      (task #(run-tasks (rest tasks) on-complete)))
    (on-complete)))


(defn- run-job [job on-complete]
  (reset! current-job-name (:name job))
  (reset! current-task-number 0)
  (reset! number-of-tasks (:initial-size job))

  (println "Running job: " (:name job))
  (run-tasks
   (:steps job)
   #(do
      (reset! current-job-name "")
      (reset! current-task-number 0)
      (reset! number-of-tasks 0)
      (on-complete))))


(defonce jobs
  (let [js (chan)]
    (go
      (loop []
        (with-blocking run-job (<! js))
        (recur)))
    js))


(defn submit
  "Submit a job.  job-name is a String identifying the job in the UI.  step-fns may either be
  a 1-ary function accepting a continuation function or a vector of these functions."
  [job-name step-fns]
  (let [steps (cond
                (fn? step-fns)         [step-fns]
                (sequential? step-fns) step-fns
                :else                  (throw (str "Expected a fn or a vector of fns but found " (type step-fns))))
        job {:name job-name
             :initial-size (count steps)
             :steps steps}]
    (put! jobs job)))


(defn sec->millis [s] (* s 1000))

(defn on-timeout
  "Delay for timeout millis, then run (f args)."
  [millis f & args]
  #?(:clj
     (future
       (Thread/sleep millis)
       (apply f args))

     :cljs
     (.setTimeout js/window #(apply f args) millis)))


(defn wait-for
  "Println the msg, wait for pred-fn to be truthy, then calls then."
  [msg pred-fn then & {:keys [debounce-delay]}]

  (println msg)
  (cond
    (pred-fn)  (if debounce-delay
                 (on-timeout 500 then)
                 (then))
    :else      (on-timeout 500 #(wait-for msg pred-fn then))
    (and pred-fn debounce-delay) (on-timeout 500 then)))


(defn start-and-monitor
  "Run start-fn and wait.  If is-started doesn't become true within patience-timeout millis, call
  stop-fn.  Wait cooldown-millis and then try again from the beginning.  Once is-started is true,
  recheck is-started every heartbeat-millis interval.  If is-started becomes false then wait
  cooldown-millis and repeat again from the beginning.  Returns a cell clients can use to monitor
  status.  Possible values are :stopped :starting :run"
  [& {:keys [start-fn is-started patience-timeout stop-fn cooldown-millis heartbeat-millis state]}]

  (when-not (and start-fn is-started patience-timeout stop-fn cooldown-millis heartbeat-millis state)
    (throw "A required named parameter is missing.  Parameters: [start-fn is-started patience-timeout stop-fn cooldown-millis heartbeat-millis state]"))

  (reset! state :stopped)
  (letfn [(monitor-state
            []
            (case @state
              :stop!    (do (stop-fn)
                            (reset! state :terminated))
              :stopped  (do (start-fn)
                            (reset! state :starting)
                            (on-timeout patience-timeout monitor-state))
              :starting (if (is-started)
                          (do (reset! state :running)
                              (on-timeout heartbeat-millis monitor-state))
                          (do (reset! state :stopped)
                              (stop-fn)
                              (on-timeout cooldown-millis monitor-state)))
              :running  (if (is-started)
                          (on-timeout heartbeat-millis monitor-state)
                          (do (reset! state :stopped)
                              (stop-fn)
                              (on-timeout cooldown-millis monitor-state)))))]
    (on-timeout cooldown-millis monitor-state)
    state))
