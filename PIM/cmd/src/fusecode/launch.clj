(ns fusecode.launch
  (:require [clojure.tools.logging  :as log]
            [clojure.java.io :as io]
            [clj-jgit.porcelain :as jgit]

            [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]

            [org.httpkit.client :as http]

            [fusecode.oo :refer [=>]]
            [fusecode.patterns :refer [letfn-map]]
            [fusecode.files :as file]
            [fusecode.config :as config])

  (:import [java.util ArrayList List]
           [java.net URLClassLoader URL]
           [java.nio.file Files Path StandardCopyOption]))


(defn-spec attach-open boolean? [options map? files (s/* string?)]
  (log/info "Attaching to running server (if possible)")
  (let [response @(http/request {:url (str"http://" (:host options) ":" (:port options) "/attach-open")
                                 :method :post
                                 :user-agent "FuseCode Launcher"
                                 :headers {"Content-Type" "text/plain"}
                                 :body (pr-str {:new-window (:new-window options)
                                                :open-files files})
                                 :follow-redirects false})]
    (= (:body response) "Accepted attach request")))


;; Use the fusecode.oo mechanism because the tests want inheritance
(def git-provider
  (letfn-map
   [(plugin-dir [self] (str @config/fuse-plugin-dir "/" (-> @config/settings :fuse-bootplugin-dir)))

    (plugin-dir-exists [self] (file/exists (plugin-dir self)))

    (secure-credentials-present?
     [self]
     (if (-> @config/settings :git-credentials :add-your-credentials)
       (do
         (log/warn "SSH credentials not yet configured.  Edit " (config/fuse-configfilename) " and remove the :add-your-credentials key-value pair after adding your Github credentials.")
         (log/info "If you wish, you can fork repositories mentioned in " (config/fuse-configfilename) " and edit " (config/fuse-configfilename) " to point to your versions.")
         false)

       true))

    (secure-credentials [self] (-> @config/settings :git-credentials))

    (install
     [self origin]
     (log/info "No " (plugin-dir self) " directory found.  Initializing for first run.")

     (if (secure-credentials-present? self)
       (jgit/with-identity (secure-credentials self)
         (jgit/git-clone origin (plugin-dir self)))

       (jgit/git-clone origin (plugin-dir self))))

    (update
     [self]
     (log/info (plugin-dir self) " directory found.  Updating.")

     (if (secure-credentials-present? self)
       (jgit/with-identity (secure-credentials self)
         (jgit/git-pull (jgit/load-repo (plugin-dir self))))

       (jgit/git-pull (jgit/load-repo (plugin-dir self)))))]))


(defn download-or-update-boot-plugin [plugin-manager]
  (if (=> plugin-manager :plugin-dir-exists)
    (=> plugin-manager :update)
    (=> plugin-manager :install (-> @config/settings :boot-plugin-repo))))


(defn invoke [classloader method & args]
  (let [mainClass (.loadClass classloader "boot.Boot")
        boot (.getMethod mainClass method (into-array Class [List]))]
    (.invoke boot nil (object-array (if args args [])))))


(defn launch-boot-plugin [plugin-manager]
  (log/info "Launching bootstrap plugin")
  (let [buildfile (str (=> plugin-manager :plugin-dir) "/build.boot")
        localrepo (str @config/fuse-plugin-dir "/_dependencies")
        bootstrap-jar-file (io/file (str @config/fuse-plugin-dir "/_bootstrap.jar"))
        boot-arguments ["--file" buildfile (-> @config/settings :bootstrap-task)]
        isolated-classloader (URLClassLoader. (into-array URL [(.toURL (.toURI bootstrap-jar-file))])
                                              (.getParent (ClassLoader/getSystemClassLoader)))]

    (System/setProperty "user.dir" (=> plugin-manager :plugin-dir))
    (System/setProperty "boot.app.path" @config/fuse-plugin-dir)
    (System/setProperty "BOOT_LOCAL_REPO" localrepo)
    (System/setProperty "BOOT_VERSION" (:boot-clj-version @config/settings))
    (System/setProperty "BOOT_CLOJURE_VERSION" (:clojure-version @config/settings))

    (with-open [in (io/input-stream (io/resource "bootstrap.jar"))]
      (io/copy in bootstrap-jar-file))

    (log/info (str "boot " boot-arguments))
    (.setContextClassLoader (Thread/currentThread) isolated-classloader)
    (invoke isolated-classloader "bootBoot" (ArrayList. boot-arguments))))


(defn start [options files]
  (download-or-update-boot-plugin git-provider)
  (launch-boot-plugin git-provider))
