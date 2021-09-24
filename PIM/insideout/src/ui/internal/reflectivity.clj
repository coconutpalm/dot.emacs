(ns ui.internal.reflectivity
  (:require [ui.inits :refer [widget-classes->inits]]
            [clojure.string :as str]
            [clj-foundation.conversions :refer :all]
            [clj-foundation.data :refer [->kebab-case]]
            [clj-foundation.interop :refer [array]]
            [ui.gridlayout])
  (:import [java.lang.reflect Modifier Field]
           [org.reflections Reflections]
           [org.reflections.scanners SubTypesScanner]
           [org.eclipse.swt SWT]
           [org.eclipse.swt.events TypedEvent]
           [org.eclipse.swt.custom SashFormLayout ScrolledCompositeLayout CTabFolderLayout]
           [org.eclipse.swt.widgets Shell Composite Widget Layout
            Tray TaskBar TaskItem ScrollBar Item Control]
           [org.eclipse.swt.opengl GLCanvas]))

(def ^:private swt-index
  (-> (Reflections. (to-array [(SubTypesScanner.)]))))

(def ^:private swt-composites (->> (.getSubTypesOf swt-index Composite)
                                 (seq)
                                 (remove #{Shell GLCanvas})))

(defmacro  composite-inits []
  (let [inits (widget-classes->inits swt-composites)]
    `[~@inits]))

(def ^:private swt-widgets (->> (.getSubTypesOf swt-index Widget)
                              (seq)
                              (remove #(.isAssignableFrom Composite %))
                              (remove #(.isAssignableFrom Item %))
                              (remove #{Control Tray TaskBar TaskItem ScrollBar})))

(defmacro widget-inits []
  (let [inits (widget-classes->inits swt-widgets)]
    `[~@inits]))


(def ^:private swt-layouts (->> (.getSubTypesOf swt-index Layout)
                              (seq)
                              (remove #{SashFormLayout ScrolledCompositeLayout CTabFolderLayout})))

(defn- types-in-package [swt-package]
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

(defn- layoutdata-by-layout []
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

(defn- fn-names<- [classes]
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

(def documentation
  {:package {:ui.SWT (sorted-publics 'ui.SWT)
             :ui.gridlayout (sorted-publics 'ui.gridlayout)}
   :swt {:SWT {SWT (fields SWT)}
         :composites (fn-names<- (conj swt-composites Shell))
         :widgets (fn-names<- swt-widgets)
         :items (->> (.getSubTypesOf swt-index Item) (seq) (sort-by #(.getSimpleName %)))
         :events (->> (.getSubTypesOf swt-index TypedEvent) (seq) (sort-by #(.getSimpleName %)))
         :listeners (->> (.getSubTypesOf swt-index org.eclipse.swt.internal.SWTEventListener)
                       (filter (fn [clazz] (not (.contains (.getSimpleName clazz) "$"))))
                       (seq)
                       (sort-by #(.getSimpleName %)))
         :graphics (types-in-package "graphics")
         :program (types-in-package "program")
         :layout-managers (layoutdata-by-layout)}})
