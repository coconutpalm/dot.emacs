(ns clj-foundation.config
  "Read configuration from an EDN file.  The EDN file's location is specified using an environment
  variable (for specifying its location during test, staging or production deployments) as well as
  a literal path or URL for specifying its location during development.  If the environment variable
  is present, it overrides any absolute path.

  In addition, the EDN file being used for configuration may contain template variables as defined
  by the [[templates]] namespace.  Template variables may be resolved through the environment or through
  keys and values specified in code."

  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]

            [clj-foundation.templates :as template]
            [clj-foundation.patterns :as p]
            [clj-foundation.io :as io]))


(defn read-config
  "Reads config object via the path represented by keys and returns the resulting object.
  On failure throws IllegalStateException."
  [config & keys]
  (try
    (let [result (reduce get config keys)]

      (if (nil? result)
        (throw (IllegalStateException. (str "Error finding config: " keys)))
        result))

    (catch NullPointerException e (throw (IllegalStateException.
                                          (str "Error finding config: " keys))))))


(defn read-settings-file [config-file-envar default-config-resource & substitutions]
  (edn/read-string (apply io/read-template config-file-envar default-config-resource substitutions)))


(defn config-reader [config-file-location-envar default-config-resource & default-kvs]
  (fn [& key-path]
    (letfn [(read-settings []
              (apply read-settings-file config-file-location-envar
                     default-config-resource
                     default-kvs))]
      (apply read-config (read-settings) key-path))))


(defmacro defconfig
    "Defines a configuration lookup function of type (=> [Keyword & Keyword ...] Any) that returns
  configuration values from the EDN file specified by variable or the default resource.  File
  resolution is done in the following precedence:

  * A Java system variable with its name matching config-file-location-envar.
  * An operating system environment variable with its name matching config-file-location-envar.
  * A Java resource with a relative path specified by default-config-resource.

  The configuration file itself should represent a single Clojure map with keywords as the keys and
  any value (including another map) as the value.  Thus calling a config function generated by
  defconfig might look like:

  (config :topic :subtopic :entry-name)

  An optional third parameter can either be a map of type {s/Keyword s/Any} or a seq of keywords and
  values representing default values that will be substitued into the configuration file using the same
  resolution rules above.  E.g.: The developer can choose to have either separate dev and prod config
  files or to define variables inside the configuration file with default values for dev that will be
  overridden in prod via Java system properties or environment variables."

    [config-fn-name config-file-location-envar default-config-resource & default-kvs]

    `(def ~config-fn-name
       (config-reader ~config-file-location-envar ~default-config-resource ~@default-kvs)))
