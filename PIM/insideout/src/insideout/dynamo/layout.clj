(ns insideout.dynamo.layout
  (:require [from.cemerick.pomegranate :as pom]))

(def maven
  {:layout {:resolver-name "Maven"
            :require #{:group+archive :version}
            :optional #{:extra-deps}
            :resolver pom/add-dependencies}})

(def git+ssh
  {:layout {:resolver-name "Git+SSH"
            :require #{:version :ssh-url :private-key}
            :resolver (constantly "TODO")}})

(def git+https
  {:layout {:resolver-name "Git+HTTPS"
            :require #{:version :https-url}
            :optional #{:user-name :password}
            :resolver (constantly "TODO")}})

(def p2
  {:layout {:resolver-name "Eclipse P2"
            :resolver (constantly "TODO")}})

(def default maven)
