(ns clj-foundation.errors
  "What is an error?  Is nil an error?  (Not always, but...)  How can we describe functions
  that might return a result or that might fail?  What about exceptions--they break referential
  transparency, but lots of code throws them anyway.  What about functions that might need to
  retry because some external actor (e.g.: the network) might have intermittent failures?

  Error handling is rarely clean, but this namespace provides some handy utilities to
  centralize some of the concerns and solve them once, reliably.  ;-)"
  (:require [clojure.string          :as str]
            [boot.from.io.aviso.exception      :as prettyexception]
            [clj-foundation.patterns :refer :all]
            [clj-foundation.millis   :as millis])

  (:import [java.util Date]))


;; Traceability ---------------------------------------------------------------------


(defmacro trace
  "Like str but prepends the namespace and line/column of the call site."
  [& more]
  (let [line-col (vec (meta &form))
        s        (apply str *ns* line-col " " more)]
    `~s))


;; Extensible failure objects / test multimethod -------------------------------------


(defmulti failure?
  "A multimethod that determines if a computation has resulted in a failure.
  This allows the definition of what constitutes a failure to be extended
  to new types by the consumer.

  An example of how this can function can be extended to new error types
  exists in this namespace where we extend failure? to include timeout errors."
  (fn [val] [(type val) val]))


(defmethod failure? [nil nil]
  [_]
  "Nil is not a failure."
  false)


(defmethod failure? [(Nothing!) NO-RESULT-ERROR]
  [_]
  "The 'error' value of the Nothing type is a failure."
  true)


(defmethod failure? :default
  [val]
  "Ordinary objects are only failures if they are Throwable."
  (instance? Throwable val))


(defn exception<-
  "If x is already Throwable return it, else convert it into an exception using ex-info.  The
  (:error-object result) will be the original value.  This is intended--though not strictly
  required--to be used for values where (failure? value) is true."
  [x]
  (cond
    (instance? Throwable x) x
    :else                   (ex-info (str x) {:error-obj x})))


(defn seq<-
  "Converts failures into seqs of exceptions.  If the failure is already an exception (the common case),
  it returns a seq starting with the root exception, and recursively including (.getCause e)
  until there are no more causes.

  If the failure is a seq, ensures that the result is a seq of excetions.

  If the failure isn't already an exception or a seq, it is converted into one first using ex-info.  In this case,
  the :cause in the ex-info map will be the original failure object."
  [failure]
  (cond
    (seq? failure)                (map exception<- failure)
    (instance? Throwable failure) (if (instance? Iterable failure)
                                    (seq failure)
                                    (lazy-seq (cons failure (seq<- (.getCause failure)))))
    (not (nil? failure))          (seq<- (exception<- failure))))


(def exception-seq
  "Deprecated.  Use errorseq<- instead."
  seq<-)


(defn stack-trace<-
  "Returns the stack trace(s) associated with e and its (.getCause)s as a String."
  [e]
  (binding [prettyexception/*fonts* nil
            prettyexception/*traditional* true]
    (->> e
         (seq<-)
         (map prettyexception/format-exception)
         (str/join "\n==>\n"))))


(defmacro try*
  "A variant of try that translates exceptions into return values or a
  specified default value.  Note that body must be a single statement.
  If you need more than that, then wrap your statement inside a \"do\". "
  ([body]
   `(try ~body (catch Throwable e# e#)))
  ([body default-value-if-failure]
   `(try ~body (catch Throwable e# ~default-value-if-failure))))


;; (dis)allowed values ------------------------------------------------------------------

(defn not-nil
  "If value is not nil, returns it, else throws IllegalArgumentException with
  the message \"${name} cannot be nil\""
  [value name]
  (if (nil? value)
    (throw (java.lang.IllegalArgumentException. (str name " cannot be nil")))
    value))


(defn not-failure
  "If value is not a failure, returns it, else throws IllegalStateExceoption with
  the specified message"
  [value message]
  (if (nil? value)
    (if (instance? Throwable value)
      (throw (java.lang.IllegalStateException. message value))
      (throw (java.lang.IllegalStateException. (str "[" value "]: " message))))
    value))


(defn throw-or
  "If value is a failure, wrap and throw it in an IllegalStateException
  with the specified message, else run function on the value and return the
  result"
  [value message f]
  (cond
    (failure? value) (throw (IllegalStateException. message value))
    :else            (f value)))


(defmacro must-be
  "If body is truthy returns result of evaluating body, else throws IllegalArgumentException with message."
  [message & body]
  (let [line-col (vec (meta &form))
        ns       *ns*]
    `(let [result# (do ~@body)]
       (if result#
         result#
         (throw (IllegalArgumentException. (str ~ns ~line-col " " ~message)))))))


;; Metalog.  Because we can't depend on any log library without breaking clients. --

(defn log-string
  "Create a log string from a sequence of log objects.  Parameters are reordered
  so that non-Throwables are first followed by Throwable objects.  Non-Throwables
  are returned as their toString representation separated by commas.  Throwables
  are represented by their stack traces, separated by \"\n==>\n\"."
  [log-objects]
  (let [{exceptions true
         others     false} (group-by #(instance? Throwable %) log-objects)
        messages           (str/join ", "   (map str others))
        stack-traces       (str/join "\n\n" (map stack-trace<- exceptions))]
    (str messages "\n" stack-traces)))


(def log-levels
  "The log levels supported by the metalogger mapped to numbers matching the log4j
  log levels.

  The current log level keywords are: #{:trace :debug :info :warn :error :fatal}"
  {:trace  5000
   :debug 10000
   :info  20000
   :warn  30000
   :error 40000
   :fatal 50000})


(defn metalog
  "The default log implementation if none is specified:

  * println levels < :error to *out*
  * println levels >= :error to *err*"
  [level-keyword & more]
  (binding [*out* (if (< (level-keyword log-levels) (:error log-levels)) *out* *err*)]
    (apply println more)))


(def ^:dynamic *log*
  "The current log implementation.  Defaults to the metalog function."
  metalog)


(defn set-global-metalogger
  "Set the clj-foundation metalogger globally.  f must be a function of type
  (=> [Keyword Any] Any)"
  [f]
  (alter-var-root #'*log* (constantly f)))


(defn log
  "Synopsis: (log :log-level & more)

  The initial :log-level must be one of the keywords in log-levels.  The remainder
  of the arguments are the objects that will be logged using log-string."
  [level & more]
  (let [args (cond (sequential? more)  more
                   :else              [more])]
     (apply *log* level args)))


;; Various retry/timeout strategies ---------------------------------------------------

(defn expect-within
  "Expect the condition specified by predicate to become true within timeout-millis. If this
  does not happen, throws IllegalStateException including the error-message.  On success, returns
  the truthy value that predicate returned."
  [timeout-millis predicate error-message]

  (let [before (.getTime (Date.))]
    (loop [completed (predicate)]
      (let [later (.getTime (Date.))]
        (if (< timeout-millis (- later before))
          (throw (IllegalStateException. error-message))
          (if-not completed
            (do
              (Thread/sleep 100)
              (recur (predicate)))
            completed))))))


(defn retry*
  "Retry calling the specified function f & args while pausing pause-millis
  between attempts.  Throwable objects, and uncaught exceptions are all
  considered errors.  After tries attempts, the last error is returned."
  [tries pause-millis f & args]
  (let [res (try* (apply f args))]
    (if-not (failure? res)
      res
      (if (zero? tries)
        res
        (do
          (if (instance? Throwable res)
            (log :warn res "A failure occurred; retrying...")
            (log :warn (str "A failure occurred; retrying...  [" (pr-str res) "]")))
          (Thread/sleep pause-millis)
          (recur (dec tries) pause-millis f args))))))


(defn retry
  "Retry calling the specified function f & args while pausing pause-millis
  between attempts.  Uncaught exceptions are considered errors.  After tries
  attempts, the last caught exception is re-thrown."
  [tries pause-millis f & args]
  (let [res (try {:value (apply f args)}
                 (catch Exception e
                   (if (zero? tries)
                     (throw e)
                     {:exception e})))]
    (if (:exception res)
      (do
        (log :warn (:exception res) "A failure occurred; retrying...")
        (Thread/sleep pause-millis)
        (recur (dec tries) pause-millis f args))
      (:value res))))


(defn retry-statements
  "Specify a retry policy for a function that will be called later.  tries is the
  number of retries allowed for the future function call, and pause-millis is the
  number of milliseconds to wait between retries.

  Returns a multi-arity function.  The initial parameter is the function to be called.
  The remaining parameters (if any) are that function's arguments."
  [tries pause-millis]
  (fn [f & args] (apply retry tries pause-millis f args)))


;; Retry calling fn with a specified timeout; abort early if abort?-fn returns truthy


(def TIMEOUT-ERROR
  "A timeout error value"
  ::TIMEOUT-ERROR)


(defmethod failure? [clojure.lang.Keyword TIMEOUT-ERROR] [_]
  "Timeout-errors are failures."
  true)


(defn timeout?
  "True iff (= TIMEOUT-ERROR e)"
  [e]
  (= TIMEOUT-ERROR e))


(defmacro try*-timeout-millis
  "Execute body with a specified timeout inside a try* block so that thrown exceptions
  are returned.

  On success, returns the result of executing body.  On failure, returns either the
  failure exception or TIMEOUT-ERROR on timeout."
  [timeout-millis & body]
  `(deref (future (try* (do ~@body)))
          ~timeout-millis
          TIMEOUT-ERROR))


(defn retry?  ;; Returns one of (enum :ABORT-MAX-RETRIES :ABORT-FATAL-ERROR :RETRY-FAILURE :RETRY-TIMEOUT)
  "Something failed.  Examine the retry count and exact failure cause and determine if we can
  retry the operation.  Internal API only; public so we can document using Schema and test."
  [job failure-value]
  (let [job-abort?       (:abort?-fn job)
        result-exception (exception<- failure-value)]
    (cond
      (<= (:max-retries job) (:retries job)) :ABORT-MAX-RETRIES
      (timeout? failure-value)               :RETRY-TIMEOUT
      (job-abort? (seq<- result-exception))  :ABORT-FATAL-ERROR
      :else                                  :RETRY-FAILURE)))


(defn new-default-job
  "Create a Job object.  Only public to make retry? testable."
  [job-name tries pause-millis abort?-fn]

  {:job-name job-name
   :abort?-fn abort?-fn
   :retries 0
   :max-retries tries
   :retry-pause-millis pause-millis})


(defrecord RetrySettings
    [tries          ;; :- Num
     timeout-millis ;; :- Num
     pause-millis   ;; :- Num
     abort?-fn])    ;; (=> [[Throwable]] Bool)


(defn retry-with-timeout
  "Retry (apply f args) up to tries times with pause-millis time in between invocation and a
  timeout value of timeout-millis.  On failure, abort?-fn is called with a vector containing the
  unwrapped exception stack.

  (failure is determined via the (failure? x) multimethod so clients can extend the set of values
  that are considered to be failures.)

  If abort?-fn returns true, the errror is considered fatal and no more retries are attempted,
  even if retries were available.

  If the last result is a failure, and that failure is Throwable, the exception is wrapped in a
  RuntimeException and rethrown.

  If the last result is a TIMEOUT-ERROR, a runtime exception is thrown.  Otherwise, the failure
  value itself is returned as the result."

  [job-name settings f & args]

  (if-not (and job-name settings f)
    (throw (ex-info "null argument detected" {})))

  (let [expected-keys [:tries :timeout-millis :pause-millis :abort?-fn]]
    (doseq [key expected-keys]
      (if-not (get settings key)
        (throw (ex-info (str key " key not found.  Found keys: " (keys settings)) {})))))

  (let [tries          (:tries settings)
        timeout-millis (:timeout-millis settings)
        pause-millis   (:pause-millis settings)
        abort?-fn      (:abort?-fn settings)]
    (loop [j (new-default-job job-name tries pause-millis abort?-fn)]
      (let [result (try*-timeout-millis timeout-millis (apply f args))]
        (if (failure? result)
          (do
            (case (retry? j result)
              :ABORT-MAX-RETRIES (do (log :warn (RuntimeException. (str "MAX-RETRIES(" tries ")[" job-name "]: " (.getMessage result)) result))
                                     (throw result))
              :ABORT-FATAL-ERROR (do (log :warn (RuntimeException. (str "FATAL[" job-name "]: " (.getMessage result)) result))
                                     (throw result))
              :RETRY-FAILURE     (do (log :warn result (str "RETRY[" job-name "]; " (type result) ": " (.getMessage result)))
                                     (Thread/sleep pause-millis))
              :RETRY-TIMEOUT     (do (log :warn (RuntimeException. "Timeout.") (str "RETRY[" job-name "]: Took longer than " timeout-millis " ms."))
                                     (Thread/sleep pause-millis))
              :else              (throw (IllegalStateException. "Program Error!  We should never get here.")))
            (recur (update-in j [:retries] inc)))
          result)))))
