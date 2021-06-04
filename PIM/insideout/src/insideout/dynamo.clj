(ns insideout.dynamo
  "A component/module system for InsideOut built on classlojure."
  (:require
   [clojure.core :refer :all]
   [classlojure.core :refer [base-classloader]]
   [cemerick.pomegranate :refer [add-dependencies]]))


(def ^:dynamic *extra-repositories*
  "Extra repositories in addition to Maven Central and Clojars. Default={}"
  {})


;; Encapsulate cemerick.pomegranate
(defn dependencies
  "Download and add the specified dependencies to the classpath.  If
  a classloader is specified, use that as the parent classloader else
  use the thread's context classloader.  Default repositories are
  Maven Central and Clojars.  Bind the *extra-repositories* dynamic
  var to add additional repositories beyond these."

  ([classloader coordinates]
   (add-dependencies :classloader classloader
                     :coordinates coordinates
                     :repositories (merge cemerick.pomegranate.aether/maven-central
                                          {"clojars" "https://clojars.org/repo"}
                                          *extra-repositories*)))
  ([coordinates]
   (dependencies base-classloader coordinates)))


(defn require-dependencies
  "Download and require namespace(s) directly from Maven-style dependencies.

  [classloader coordinates require-params] or
  [coordinates require-params] where

  classloader - the parent classloader for the new class files
  coordinates - A vector of '[maven.style/coordinates \"1.0.0\"]
  require-params - A vector of parameters to pass to clojure.core/require
                   Or a vector of vectors to sequentially pass to clojure.core/require"

  ([classloader coordinates require-params]
   (dependencies classloader coordinates)
   (when-not (empty? require-params)
     (if (every? sequential? require-params)
       (map require require-params)
       (require require-params))))

  ([coordinates require-params]
   (require-dependencies base-classloader
                         coordinates
                         require-params)))


;; This has to be a macro because `import` is a macro and has to
;; be executed inside the namespace into which the class will be imported.
(defmacro import-dependencies
  "Download and import classes directly from Maven-style dependencies.

  [classloader coordinates import-params] where
  classloader - The parent classloader
  coordinates - A vector of '[maven.style/coordinates \"1.0.0\"]
  import-params - A vector of parameters to pass to clojure.core/import
                  Or a vector of vectors to sequentially pass to clojure.core/import"

  [classloader coordinates import-params]
  (let [imports (if (empty? import-params)
                  []
                  (if (every? sequential? import-params)
                    (map (fn [i] `(import ~i)) import-params)
                    [~(import import-params)]))]
    `(do
       (dependencies ~classloader ~coordinates)
       ~@imports)))


(def services (atom {}))


(comment
  ;; Need service classloader management life cycle
  ;;      add deps to service classloader
  ;;      event bus for service comms
  ;;      "atomic" pubsub across services
  ,)
