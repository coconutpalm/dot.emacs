(ns util.dynamo
  (:require
   [clojure.core :refer :all]
   [cemerick.pomegranate :refer [add-dependencies]]))


(def ^:dynamic *extra-repositories*
  "Extra repositories to add to Maven Central and Clojars. Default={}"
  {})


(defn require-dependencies
  "Download and require namespace(s) directly from Maven-style dependencies.

  [classloader coordinates require-params] or
  [coordinates require-params] where

  classloader - the parent classloader for the new class files
  coordinates - A vector of '[maven.style/coordinates \"1.0.0\"]
  require-params - A vector of parameters to pass to clojure.core/require
                   Or a vector of vectors to sequentially pass to clojure.core/require"

  ([classloader coordinates require-params]
   (add-dependencies :classloader classloader
                     :coordinates coordinates
                     :repositories (merge cemerick.pomegranate.aether/maven-central
                                          {"clojars" "https://clojars.org/repo"}
                                          *extra-repositories*))
   (when-not (empty? require-params)
     (if (every? sequential? require-params)
       (map require require-params)
       (require require-params))))

  ([coordinates require-params]
   (require-dependencies (-> (Thread/currentThread)
                            (.getContextClassLoader))
                         coordinates
                         require-params)))


;; This has to be a macro because `import` is a macro and has to
;; be executed inside the namespace into which the class will be imported.
(defmacro import-dependencies
  "Download and require classes directly from Maven-style dependencies.

  [coordinates import-params] where
  coordinates - A vector of '[maven.style/coordinates \"1.0.0\"]
  import-params - A vector of parameters to pass to clojure.core/import
                  Or a vector of vectors to sequentially pass to clojure.core/import"

  [coordinates import-params]
  (let [imports (if (empty? import-params)
                  []
                  (if (every? sequential? import-params)
                    (map (fn [i] `(import ~i)) import-params)
                    [~(import import-params)]))]
    `(do
       (add-dependencies :classloader (-> (Thread/currentThread)
                                         (.getContextClassLoader))
                         :coordinates ~coordinates
                         :repositories (merge cemerick.pomegranate.aether/maven-central
                                              {"clojars" "https://clojars.org/repo"}
                                              *extra-repositories*))
       ~@imports)))


(comment

  ,)
