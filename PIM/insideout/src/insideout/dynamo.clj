(ns insideout.dynamo
  "A component/module system for InsideOut built on classlojure."
  (:require
   [cemerick.pomegranate        :as pom]
   [cemerick.pomegranate.aether :as pom-mvn]
   [ns-tracker.core             :as nt]
   [clojure.string              :as str]
   [clojure.java.io             :as io]
   [clojure.stacktrace          :as stacktrace])
  (:import
   [clojure.lang DynamicClassLoader]
   [java.io File]
   [org.apache.maven.repository.internal MavenRepositorySystemUtils]
   [cemerick.pomegranate.aether PomegranateWagonProvider TransferListenerProxy]
   [org.eclipse.aether RepositorySystem]
   [org.eclipse.aether.repository LocalRepository]
   [org.eclipse.aether.transfer TransferListener]
   [org.eclipse.aether.connector.basic BasicRepositoryConnectorFactory]
   [org.eclipse.aether.impl DefaultServiceLocator$ErrorHandler]
   [org.eclipse.aether.spi.connector RepositoryConnectorFactory]
   [org.eclipse.aether.spi.connector.layout  RepositoryLayoutFactory]
   [org.eclipse.aether.spi.connector.transport TransporterFactory]
   [org.eclipse.aether.transport.file FileTransporterFactory]
   [org.eclipse.aether.transport.wagon WagonTransporterFactory WagonProvider]
   [org.openntf.maven.p2.connector P2RepositoryConnectorFactory]
   [org.openntf.maven.p2.layout P2RepositoryLayoutFactory]))


(def ^{:private true} default-local-repo
  (io/file (System/getProperty "user.home") ".m2" "repository"))

(defn eclipse-repo
  [& YYYY-MM]
  {:id     "org.eclipse.p2"
   :url    (str "http://download.eclipse.org/releases/" (or (first YYYY-MM) "2021-09"))
   :layout "p2"})


(def ^:dynamic *extra-repositories*
  "Extra repositories in addition to Maven Central and Clojars. Default={}.
  Use the `eclipse-repo` function to get an Eclipse P2 repository configuration
  if you want to add Eclipse/P2 dependencies."
  {})

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


;; Stub for supporting https://github.com/OpenNTF/p2-layout-provider
;;
;; copypasta from https://github.com/clj-commons/pomegranate/blob/master/src/main/clojure/cemerick/pomegranate/aether.clj#L112
(defn- repository-system
  []
  (let [error-handler (clojure.core/proxy [DefaultServiceLocator$ErrorHandler] []
                        (serviceCreationFailed [type-clazz impl-clazz ^Throwable e]
                          (stacktrace/print-cause-trace e)))]
    (.getService
     (doto (MavenRepositorySystemUtils/newServiceLocator)
       (.setService TransporterFactory WagonTransporterFactory)
       (.setService WagonProvider PomegranateWagonProvider)
       (.addService RepositoryConnectorFactory BasicRepositoryConnectorFactory)
       (.addService RepositoryConnectorFactory P2RepositoryConnectorFactory)
       #_(.addService RepositoryLayoutFactory P2RepositoryLayoutFactory)
       (.addService TransporterFactory FileTransporterFactory)
       (.setErrorHandler error-handler))
     RepositorySystem)))

(defn- default-listener-fn
  [{:keys [type method transferred resource error] :as evt}]
  (let [{:keys [name size repository transfer-start-time]} resource]
    (case type
      :started (do
                 (print (case method :get "Retrieving" :put "Sending")
                        name
                        (if (neg? size)
                          ""
                          (format "(%sk)" (Math/round (double (max 1 (/ size 1024)))))))
                 (when (< 70 (+ 10 (count name) (count repository)))
                   (println) (print "    "))
                 (println (case method :get "from" :put "to") repository))
      (:corrupted :failed) (when error (println (.getMessage ^Exception error)))
      nil)))

(defn- transfer-event
  [^org.eclipse.aether.transfer.TransferEvent e]
  ;; INITIATED, STARTED, PROGRESSED, CORRUPTED, SUCCEEDED, FAILED
  {:type (-> e .getType .name str/lower-case keyword)
   ;; :get :put
   :method (-> e .getRequestType str/lower-case keyword)
   :transferred (.getTransferredBytes e)
   :error (.getException e)
   :data-buffer (.getDataBuffer e)
   :data-length (.getDataLength e)
   :resource (let [r (.getResource e)]
               {:repository (.getRepositoryUrl r)
                :name (.getResourceName r)
                :file (.getFile r)
                :size (.getContentLength r)
                :transfer-start-time (.getTransferStartTime r)
                :trace (.getTrace r)})})

(defn- construct-transfer-listener
  [transfer-listener]
  (cond
    (instance? TransferListener transfer-listener) transfer-listener

    (= transfer-listener :stdout)
    (TransferListenerProxy. (comp default-listener-fn transfer-event))

    (fn? transfer-listener)
    (TransferListenerProxy. (comp transfer-listener transfer-event))

    :else (TransferListenerProxy. (fn [_]))))

(defn- repository-session-fn
  [{:keys [local-repo offline? transfer-listener mirror-selector]}]
  (let [session (org.apache.maven.repository.internal.MavenRepositorySystemUtils/newSession)
        session (doto session
                  (.setLocalRepositoryManager (.newLocalRepositoryManager
                                               ^RepositorySystem (repository-system)
                                               session
                                               (LocalRepository.
                                                (io/file (or local-repo default-local-repo)))))
                  (.setMirrorSelector mirror-selector)
                  (.setOffline (boolean offline?))
                  (.setTransferListener (construct-transfer-listener transfer-listener)))]
    (if (contains? (.getConfigProperties session) "aether.checksums.forSignature")
      session
      (doto session
        (.setConfigProperty "aether.checksums.forSignature" true)))))


;; Encapsulate cemerick.pomegranate, but adding P2 repo support
(defn resolve-libs
  "Download and add the specified dependencies to the classpath.  If
  a classloader is specified, use that as the parent classloader else
  use the thread's context classloader.  Default repositories are
  Maven Central and Clojars.  Bind the *extra-repositories* dynamic
  var to add additional repositories beyond these."

  ([classloader coordinates]
   (pom/add-dependencies :repository-session-fn repository-session-fn
                         :classloader classloader
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
  (let [conv-over-config [["src/main/clojure" "src/clojure" "src/main" "src"]
                          ["src/main/resources" "src/resources" "resources"]
                          ["src/test/clojure" "test/clojure" "src/test" "test"]]]
    (mapcat
     (fn [paths]
       (take 1 (filter #(->> (File. %) (.exists)) paths)))
     conv-over-config)))

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
