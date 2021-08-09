(ns insideout.boot
  (:import [clojure.java.api Clojure]
           [clojure.lang IFn])
  (:gen-class :main true))


(defn -main [& args]
  (let [require (Clojure/var "clojure.core" "require")]
    (.invoke require (Clojure/read "insideout.core")))

  (let [main (Clojure/var "insideout.core" "-main")]
    (apply main args)))
