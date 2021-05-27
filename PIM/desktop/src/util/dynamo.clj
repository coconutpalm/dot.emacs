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


(defmacro macro->fn
  "Convert a macro to a function."
  [macro]
  `(fn [& args#] (eval (cons '~macro args#))))

(defn ^:private import-one [clazzes]
  (println clazzes)
  ((macro->fn import) clazzes))


(defn import-dependencies
  "Download and require classes directly from Maven-style dependencies.

  [classloader coordinates import-params] or
  [coordinates import-params] where

  classloader - the parent classloader for the new class files
  coordinates - A vector of '[maven.style/coordinates \"1.0.0\"]
  import-params - A vector of parameters to pass to clojure.core/import
                  Or a vector of vectors to sequentially pass to clojure.core/import"

  ([classloader coordinates import-params]
   (add-dependencies :classloader classloader
                     :coordinates coordinates
                     :repositories (merge cemerick.pomegranate.aether/maven-central
                                          {"clojars" "https://clojars.org/repo"}
                                          *extra-repositories*))
   (when-not (empty? import-params)
     (if (every? sequential? import-params)
       (map import-one import-params)
       (import-one import-params))))

  ([coordinates import-params]
   (import-dependencies (-> (Thread/currentThread)
                           (.getContextClassLoader))
                        coordinates
                        import-params)))
