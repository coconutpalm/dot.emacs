(ns ui.internal.reflectivity
  (:require [clojure.string :as str]
            [clj-foundation.conversions :refer :all]
            [clj-foundation.data :refer [->kebab-case]]
            [clj-foundation.interop :refer [array]])
  (:import [java.lang.reflect Modifier Field]
           [org.reflections Reflections]
           [org.reflections.scanners SubTypesScanner]
           [org.eclipse.swt.custom SashFormLayout ScrolledCompositeLayout CTabFolderLayout]
           [org.eclipse.swt.widgets Shell Composite Widget Layout
            Tray TaskBar TaskItem ScrollBar Item Control]
           [org.eclipse.swt.opengl GLCanvas]))

(def swt-index
  (-> (Reflections. (to-array [(SubTypesScanner.)]))))

(def swt-composites (->> (.getSubTypesOf swt-index Composite)
                       (seq)
                       (remove #{Shell GLCanvas})
                       (#(conj % Composite))))

(def swt-widgets (->> (.getSubTypesOf swt-index Widget)
                              (seq)
                              (remove #(.isAssignableFrom Composite %))
                              (remove #(.isAssignableFrom Item %))
                              (remove #{Control Tray TaskBar TaskItem ScrollBar})))

(def swt-items (->> (.getSubTypesOf swt-index Item)
                  (seq)
                  (remove #{TaskItem})
                  (sort-by #(.getSimpleName %))))

(def swt-layouts (->> (.getSubTypesOf swt-index Layout)
                    (seq)
                    (remove #{SashFormLayout ScrolledCompositeLayout CTabFolderLayout})))

(def swt-listeners (->> (.getSubTypesOf swt-index org.eclipse.swt.internal.SWTEventListener)
                      (filter #(.endsWith (.getSimpleName %) "Listener"))
                      (filter #(> 0 (.indexOf (.getName %) "internal")))
                      (sort-by #(.getSimpleName %))
                      (map (fn [clazz] [clazz (->> (.getMethods clazz)
                                                (remove #(.endsWith (str (.getDeclaringClass %)) "Object"))
                                                (remove #(not= 0 (bit-and Modifier/STATIC (.getModifiers %)))))]))
                      (into {})))

(def widget-to-listener-methods
  (apply merge
         (->> (concat swt-composites swt-widgets swt-items)
            (map (fn [clazz] {clazz (->> (.getMethods clazz)
                                      (remove #(= "addListener" (.getName %)))
                                      (filter (fn [m] (let [name (.getName m)]
                                                       (and (.startsWith name "add")
                                                            (.endsWith name "Listener")))))
                                      (map (fn [m]
                                             (let [listener-type (first (.getParameterTypes m))]
                                               {:add-listener-method (.getName m)
                                                :listener-type listener-type
                                                :listener-methods (get swt-listeners listener-type)}))))})))))


(defn types-in-package [swt-package]
  (->> (Reflections. (array [Object]
                          (str "org.eclipse.swt." swt-package)
                          (SubTypesScanner. false)))
     (.getAllTypes)
     (seq)
     (sort)
     (map #(Class/forName %))))

(def ^:private swt-layoutdata (types-in-package "layout"))


;; =====================================================================================
;; Generate online docs from class metadata

(defn layoutdata-by-layout []
  (letfn [(layout-type [clazz]
            (-> (.getSimpleName clazz)
               ->kebab-case
               (.split "\\-")
               first))]
    (reduce (fn [cur layout-class]
              (let [key (layout-type layout-class)
                    layoutdata (filter #(= (layout-type %) key) swt-layoutdata)]
                (conj cur [layout-class layoutdata])))
            {}
            swt-layouts)))

(defn fn-names<- [classes]
  (letfn [(fn-name<- [clazz]
            (-> (.getSimpleName clazz) ->kebab-case))]
    (sort-by first (map (fn [c] [(fn-name<- c) c]) classes))))


(defn- extract-java-meta [xs]
  (->> xs
     (map (fn [x] [(symbol (str (.getName x)
                               (if (instance? Field x)
                                 ""
                                 (str "("
                                      (str/join ", " (map #(symbol (.getSimpleName %)) (.getParameterTypes x)))
                                      ")"))))
                  {:type (if (instance? Field x) (.getType x) (.getReturnType x))
                   :declaring-class (.getDeclaringClass x)}]))
     (sort-by first)))


(defn fields [^Class clazz]
  (->> (.getFields clazz)
     (filter (fn [field] (not= 0 (bit-and (.getModifiers field) Modifier/PUBLIC))))
     (extract-java-meta)))

(defn setters [^Class clazz]
  (->> (.getMethods clazz)
     (filter (fn [method] (and (not= 0 (bit-and (.getModifiers method) Modifier/PUBLIC))
                              (.startsWith (.getName method) "set"))))
     (extract-java-meta)))

(defn non-prop-methods [^Class clazz]
  (->> (.getMethods clazz)
     (filter (fn [method] (and (not= 0 (bit-and (.getModifiers method) Modifier/PUBLIC))
                              (not= Object (.getDeclaringClass method))
                              (not (.startsWith (.getName method) "get"))
                              (not (.startsWith (.getName method) "set")))))
     (extract-java-meta)))

(defn sorted-publics
  "Like `ns-publics` but returns the results sorted by symbol name."
  [ns]
  (if (string? ns)
    (sorted-publics (symbol ns))
    (->> (ns-publics ns) vec (sort-by first))))
