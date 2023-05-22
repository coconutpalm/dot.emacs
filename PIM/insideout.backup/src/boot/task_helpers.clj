(ns boot.task-helpers
  (:require
   [clojure.java.io         :as io]
   [clojure.set             :as set]
   [clojure.string          :as string]
   [clojure.stacktrace      :as trace]
   [boot.from.io.aviso.ansi :as ansi]
   [boot.from.digest        :as digest]))

(defn- first-line [s] (when s (first (string/split s #"\n"))))

(defn- tasks-table [tasks]
  (let [get-task #(-> % :name str)
        get-desc #(-> % :doc first-line)
        built-in {nil (get tasks 'boot.task.built-in)}]
    (->> (dissoc tasks 'boot.task.built-in)
      (concat built-in) (interpose nil)
      (mapcat (fn [[_ xs]] (or xs [{:name "" :doc ""}])))
      (mapv (fn [x] ["" (get-task x) (get-desc x)])))))

(defn- set-title [[[_ & xs] & zs] title] (into [(into [title] xs)] zs))

(defn- available-tasks [sym]
  (let [base  {nil (the-ns sym)}
        task? #(:insideout.boot/task %)
        nssym #(->> % meta :ns ns-name)
        addrf #(if-not (seq %1) %2 (symbol %1 (str %2)))
        proc  (fn [a [k v]] (assoc (meta v) :ns* (nssym v) :name (addrf a k) :var v))
        pubs  (fn [[k v]] (map proc (repeat (str k)) (ns-publics v)))]
    (->>
      (concat
        (->> sym ns-refers (map proc (repeat nil)))
        (->> sym ns-aliases (into base) (mapcat pubs)))
      (filter task?) (sort-by :name) (group-by :ns*) (into (sorted-map)))))

(defn read-pass
  [prompt]
  (String/valueOf (.readPassword (System/console) prompt nil)))
