
# Main server

* just a router
* Use Sente for websockets from browser or other server peers
  * Sente provides list of remote connected clients by "user"

## WS message envelope format

[:from #UUID "my-uuid" :domain/topic & more]
[:from #UUID "my-uuid" :to [#UUID "to-uuid" ...] & more]


# Server peers

* Provide ingest, event-driven compute, and storage services
  * e.g.: Push new IMAP email into database (ingest email)
  * Identity services across my identities, others' identities
    * Sync with contacts / directory service
  * Ingest SMS, Slack, Discord, ... messages
  * Tag emails by [:close-contact-list :professional-contact-list :address-book :everything-else]

# UI tier

* Minimal HTML/JS shell with "loading" icon
* Service worker caches app code and other assets
* Maybe push cljs up and compile/eval it in the browser (cache compiled js)
  * Starting with Hoplon/Javelin

-----

# Synonmym Spike

## Todo

Figure out requirements to be a Rally Engine app.  (Talk with Daniel.)

Read the RFC

The app

* Infrastructure
  * Switch to Sente for WS support
  * Add prevayler persistence to atomic lib
    * (Maybe Kafka also?)
  * Transit for comms serialization?
  * Refactor handler.clj into core server bits and the rest

* Synchronized, persistent atom
  * There's the synchronized atom, then there's the thing that manages it
  * Server-side
    * Track client connections
    * Change handling
      * Persist
      * Relay to clients
    * Snapshot
    * Restore state
  * Client side
    * Accept new state
    * Accept changes from server
    * Relay changes to server

* UI
  * Display / edit list
  * Search/filter list
  * Manage environments and data promotion

* Data storage
  * The current list by environment
  * Change history by who
  * Promotion history by who


---------------------------------------------------------------------------------------------------

## Tools

UI styling
https://github.com/dvingo/cljs-emotion
https://mildrenben.github.io/surface/index.html  (lightweight material design css framework)
https://github.com/hoplon/ui  (probably not)


Basic persistence
https://github.com/klauswuestefeld/prevayler-clj [prevayler-clj/prevayler4 "2020.11.14"]


Sente websockets:
https://github.com/ptaoussanis/sente


Syncing atoms:
https://github.com/rymndhng/entangle
https://github.com/wilkerlucio/pathom


Test runner
https://github.com/lambdaisland/kaocha


Diffing
https://github.com/lambdaisland/deep-diff2 - For pretty-printing diffs
https://github.com/juji-io/editscript - Diff/patch Clojure data structures


Databases & persistence
https://github.com/klauswuestefeld/prevayler-clj
https://github.com/juji-io/datalevin
https://opencrux.com/reference/21.01-1.14.0/rocksdb.html

Crypto
https://github.com/open-keychain/openpgp-api
https://github.com/rweather/noise-java
https://github.com/rweather/newhope-java
https://noiseprotocol.org/

Gossip
https://github.com/flopezluis/gossiphttps://github.com/muattiyah/gossip


## edn-seq

(defn edn-seq
  "Returns the objects from stream as a lazy sequence."
  ([]
     (edn-seq *in*))
  ([stream]
     (edn-seq {} stream))
  ([opts stream]
     (lazy-seq (cons (clojure.edn/read opts stream) (edn-seq opts stream)))))

(defn swallow-eof
  "Ignore an EOF exception raised when consuming seq."
  [seq]
  (-> (try
        (cons (first seq) (swallow-eof (rest seq)))
        (catch java.lang.RuntimeException e
          (when-not (= (.getMessage e) "EOF while reading")
            (throw e))))
      lazy-seq))

(with-open [stream (java.io.PushbackReader. (clojure.java.io/reader "foo.txt"))]
  (dorun (map println (swallow-eof (edn-seq stream)))))
