(ns insideout.dynamo
  "A component/module system for InsideOut built on classlojure."
  (:require
   [clojure.core         :refer :all]
   [clj-foundation.types :as tp]
   [cemerick.pomegranate :as pom])
  (:import [java.io File]
           [java.net URL]
           [clojure.lang DynamicClassLoader RT]))


(def ^:dynamic *extra-repositories*
  "Extra repositories in addition to Maven Central and Clojars. Default={}"
  {})

(defn ^DynamicClassLoader
  dyn-classloader []
  (let [cl (-> (Thread/currentThread) .getContextClassLoader)]
    (if (instance? DynamicClassLoader cl)
      cl
      (let [dcl (DynamicClassLoader. cl)]
        (-> (Thread/currentThread) (.setContextClassloader dcl))
        dcl))))

#_(defn ^DynamicClassLoader
    dyn-classloader []
    (let [cl (var-get Compiler/LOADER)]
      (if (instance? DynamicClassLoader cl)
        cl
        (let [dcl (cond
                    (instance? ClassLoader cl)              (DynamicClassLoader. cl)
                    (instance? ClassLoader (RT/baseLoader)) (DynamicClassLoader. (RT/baseLoader))
                    :default                                (DynamicClassLoader. (.getContextClassLoader (Thread/currentThread))))]
          (alter-var-root Compiler/LOADER (constantly dcl))
          dcl))))

(comment
  (dyn-classloader)
  ,)
#_(def ^DynamicClassLoader
    dyn-classloader (delay (->> Compiler/LOADER deref .getParent .getParent)))


;; Encapsulate cemerick.pomegranate
(defn resolve-libs
  "Download and add the specified dependencies to the classpath.  If
  a classloader is specified, use that as the parent classloader else
  use the thread's context classloader.  Default repositories are
  Maven Central and Clojars.  Bind the *extra-repositories* dynamic
  var to add additional repositories beyond these."

  ([classloader coordinates]
   (pom/add-dependencies :classloader classloader
                         :coordinates coordinates
                         :repositories (merge cemerick.pomegranate.aether/maven-central
                                              {"clojars" "https://clojars.org/repo"}
                                              *extra-repositories*)))
  ([coordinates]
   (resolve-libs (dyn-classloader) coordinates)))


(defn require-libs
  "Download and require namespace(s) directly from Maven-style dependencies.

  [classloader coordinates require-params] or
  [coordinates require-params] where

  classloader - the parent classloader for the new class files
  coordinates - A vector of '[maven.style/coordinates \"1.0.0\"]
  require-params - A vector of parameters to pass to clojure.core/require
                   Or a vector of vectors to sequentially pass to clojure.core/require"

  ([classloader coordinates require-params]
   (resolve-libs classloader coordinates)
   (when-not (empty? require-params)
     (if (every? sequential? require-params)
       (apply require require-params)
       (require require-params))))

  ([coordinates require-params]
   (require-libs (dyn-classloader) coordinates require-params)))


;; This has to be a macro because `import` is a macro and has to
;; be executed inside the namespace into which the class will be imported.
(defmacro import-libs
  "Download and import classes directly from Maven-style dependencies.

  [coordinates import-params] where
  coordinates - A vector of '[maven.style/coordinates \"1.0.0\"]
  import-params - A vector of parameters to pass to clojure.core/import
                  Or a vector of vectors to sequentially pass to clojure.core/import"

  [coordinates import-params]
  (let [imports (if (empty? import-params)
                  []
                  (if (every? sequential? import-params)
                    (map (fn [i] `(import ~i)) import-params)
                    [`(import ~import-params)]))]
    `(do
       (resolve-libs (dyn-classloader) ~coordinates)
       ~@imports)))


(defn find-src+test+res []
  (let [conv-over-config [["src/main/clojure" "src/clojure" "src"]
                          ["src/main/resources" "resources"]
                          ["src/test/clojure" "test/clojure" "test"]]]
    (mapcat
     (fn [paths]
       (take 1 (filter #(->> (File. %) (.exists)) paths)))
     conv-over-config)))

(def ^:dynamic *classpath-dirs*
  (map (fn [rel-path] (-> rel-path File. .toURL))
       (find-src+test+res)))


(defn add-source-folders-to-classpath
  "Adds the java.io.File objects in *classpath-dirs* to the classpath."
  []
  (let [cl ^DynamicClassLoader (dyn-classloader)]
    (doseq [u *classpath-dirs*]
      (.addURL cl u))))


(defn classloader-hierarchy
  "Return the current classloader hierarchy."
  []
  (pom/classloader-hierarchy))


(defn get-classpath
  "Return the current classpath."
  []
  (pom/get-classpath))

(comment
  (->> "src" File. .toURL)

  (let [^DynamicClassLoader cl (dyn-classloader)]
    (-> cl (.addURL (->> "src" File. .toURL))))

  ;; Need service classloader management life cycle
  ;;      add deps to service classloader
  ;;      event bus for service comms
  ;;      "atomic" pubsub across services
  ,)
