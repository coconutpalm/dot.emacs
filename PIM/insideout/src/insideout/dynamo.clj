(ns insideout.dynamo
  "A component/module system for InsideOut built on Pomegranite."
  (:require
   [clojure.set                      :as s]
   [from.cemerick.pomegranate        :as pom]
   [from.cemerick.pomegranate.aether :as a]
   [ns-tracker.core                  :as nt])
  (:import
   [clojure.lang DynamicClassLoader]
   [java.io File]))


(def ^:dynamic *extra-repositories*
  "Extra repositories in addition to Maven Central and Clojars. Default={}.
  Use the `eclipse-repo` function to get an Eclipse P2 repository configuration
  if you want to add Eclipse/P2 dependencies."
  [])

(def ^:private dynamo-loader (atom nil))

(defn ^DynamicClassLoader
  dyn-classloader
  "Return the dynamic classloader for the thread's context classloader.  If a dynamic
  classloader hasn't been added to the current thread, one is registered."
  []
  (let [cl (-> (Thread/currentThread) .getContextClassLoader)]
    (if (instance? DynamicClassLoader cl)
      (reset! dynamo-loader cl)
      (let [dcl (or @dynamo-loader (DynamicClassLoader. cl))]
        (-> (Thread/currentThread) (.setContextClassloader dcl))
        (reset! dynamo-loader dcl)))))


(defn ^Thread new-thread
  "Return a new thread preconfigured with the dynamo classloader.  Doesn't call `start`."
  ([]
   (let [t (Thread.)]
     (.setContextClassloader t (dyn-classloader))
     t))
  ([runnable]
   (let [t (Thread. runnable)]
     (.setContextClassloader t (dyn-classloader))
     t)))


(defn resolver
  [dependency]
  (let [[[group-archive version]
         specs] [(take 2 dependency)
                 (apply into {} (drop 2 dependency))]]
    ))


(comment

  (nth [1 2] 2)
  (count [1 2])
  (take 2 (range 1))

  [clojure/spec.alpha "1.1" layout/maven]  ; maven is the default layout
  [clojure/spec.alpha "1.1" repo/github proto/https layout/git]
  [clojure/spec.alpha "1.1" repo/gitlab proto/git]
  [clojure/spec.alpha "1.1" repo/gitlab]  ; implies proto/git
  [clojure/spec.alpha "1.1" proto/git (repo "coconut-palm-software.com/repo/root")]
  [clojure/spec.alpha "1.1" repo/https "coconut-palm-software.com/repo/root"]

  "")


(defn resolve-libs
  "Download and add the specified dependencies to the classpath.  If
  a classloader is specified, use that as the parent classloader else
  use the thread's context classloader.  Default repositories are
  Maven Central and Clojars.  Bind the *extra-repositories* dynamic
  var to add additional repositories beyond these."

  ([classloader coordinates]
   (pom/add-dependencies :classloader classloader
                         :coordinates coordinates
                         :repositories (apply merge from.cemerick.pomegranate.aether/maven-central
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


(defn add-urls-to-classpath
  "Adds the specified URLs to the classpath."
  [urls]
  (let [cl ^DynamicClassLoader (dyn-classloader)]
    (doseq [u urls]
      (.addURL cl u))))


(defn find-src+test+res
  ([]
   (find-src+test+res (File. ".")))
  ([root-path]
   (let [conv-over-config [["src/main/clojure" "src/clojure" "src/main" "src"]
                           ["src/main/resources" "src/resources" "resources"]
                           ["src/test/clojure" "test/clojure" "src/test" "test"]]]
     (mapcat
       (fn [paths]
         (take 1 (filter #(->> (File. root-path %) (.exists)) paths)))
       conv-over-config))))

(def ^:dynamic *classpath-dirs*
  (map (fn [rel-path] (-> rel-path File. .toURL))
       (find-src+test+res)))

(def out-of-date-namespaces (nt/ns-tracker (find-src+test+res)))


;; Filesystem event handling
#_(defn init-project [p]
    (println (str "Initializing project " p)))


(defn add-source-folders-to-classpath
  "Adds the java.io.File objects in *classpath-dirs* to the classpath."
  []
  (add-urls-to-classpath *classpath-dirs*))


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
