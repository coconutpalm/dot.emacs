(ns insideout.boot
  (:import [clojure.java.api Clojure]
           [clojure.lang IFn])
  (:gen-class :main true))


(defn ensure-compiler-loader
  "Ensures the clojure.lang.Compiler/LOADER var is bound to a DynamicClassLoader,
  so that we can add to Clojure's classpath dynamically."
  []
  (when-not (bound? Compiler/LOADER)
    (.bindRoot Compiler/LOADER (clojure.lang.DynamicClassLoader. (clojure.lang.RT/baseLoader)))))


(defn -main [& args]
  (ensure-compiler-loader)

  (let [require (Clojure/var "clojure.core" "require")]
    (require (Clojure/read "insideout.core")))

  (let [main (Clojure/var "insideout.core" "-main")]
    (apply main args)))
