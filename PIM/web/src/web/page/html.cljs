(ns web.page.html
  "HTML / DOM manipulation functions"
  (:require [goog.dom :as dom]
            [goog.dom.classes :as classes]
            [goog.events :as events]
            [clojure.string :as str]

            [hoplon.core :as h :refer [html-meta]]
            [dommy.core :as dommy]
            [util.jobs :as job :refer [with-blocking]])
  (:require-macros
   [javelin.core :refer [defc defc=]])
  (:import [goog Timer]))


;; All the metas

(defn metas
  "All the usual HTML meta tags"
  []
  [(html-meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
   (html-meta :http-equiv "X-UA-Compatible" :content "IE=edge")
   (html-meta :name "viewport" :content "width=device-width,initial-scale=1")
   (html-meta :name "msapplication-tap-highlight" :content "no")

   ;; Android 5 Chrome color
   (html-meta :name "theme-color" :content "#EE6E73")

   ;; Favicons - TODO: Need to actually put some icons in the images folder
   (h/link :rel "apple-touch-icon-precomposed" :href "images/favicon/apple-touch-icon-152x152.png")
   (html-meta :name "msapplication-TileColor" :content "#FFFFFF")
   (html-meta :name "msapplication-TileImage" :content "images/favicon/mstile-144x144.png")
   (h/link :rel "icon" :href "/assets/favicon.ico" :sizes "32x32")])

;; Find / process HTML elements

(defn by-id
  "Get an element by its id"
  [id]
  (.getElementById js/document (name id)))


(defn by-tag-name
  "Get elements by tag name"
  [tag-name]
  (.getElementsByTagName js/document (name tag-name)))


(defn length
  "Return the number of elements in an HTMLCollection"
  [nodes]
  (. nodes -length))


(defn item
  "Get the item at offset n in HTMLCollection"
  [nodes n] (.item nodes n))


(defn as-seq
  "Explicitly coerce an HTML collection to a seq"
  [nodes]
  (for [i (range (length nodes))] (item nodes i)))


(defn by-tag
  "Returns the seq of elements with the specified tag name"
  [tag]
  (as-seq
   (.getElementsByTagName js/document (name tag))))



(extend-type js/NodeList
  ISeqable
  (-seq [array] (array-seq array 0)))


(extend-type js/HTMLCollection
  ISeqable
  (-seq [array] (array-seq array 0)))


(extend-type js/FileList
  ISeqable
  (-seq [array] (array-seq array 0)))


(defn refresh-layout
  "Force the browser to relayout the page"
  []
  (set! (-> js/document .-body .-style .-zIndex) 1))


(defn append-child!
  "Add child-node as a child of parent-node.  child-node may be a single dom node or
  a seq of dom nodes to be added."
  ([parent-node child-node]
   (if (seq? child-node)
     (do (append-child! parent-node (first child-node))
         (append-child! parent-node (rest child-node)))
     (.appendChild parent-node child-node)))
  ([child-node]
   (append-child! js/document child-node)))


(defn html
  "Return the html dom node or nil if no HTML node isn't defined."
  []
  (.-html js/document))


(defn head!
  "Add subnode to the head node."
  [subnode]
  (let [h (first (by-tag "head"))]
    (.appendChild h subnode)))


(defn inner-html
  "Return the HTML inside \"dom\".  If \"dom\" is nil or innerHTML is undefined, return nil."
  [dom]
  (when dom
    (. dom -innerHTML)))


(defn inner-html!
  "Set the html inside the specified dom node."
  [dom content]
  (if dom
    (set! (. dom -innerHTML) content)
    (throw "Cannot set inner-html!, dom id nil.")))


(defn- link [& properties]
  (let [l (dommy/create-element :link)]
    (doseq [[k v] (partition 2 (seq properties))]
      (dommy/set-attr! l k v))
    l))

(defn- script [& properties]
  (let [s (dommy/create-element :script)]
    (doseq [[k v] (partition 2 (seq properties))]
      (dommy/set-attr! s k v))
    s))

(defn cssfn
  ([url]
   (fn [] (link :rel "stylesheet" :href url)))
  ([url sri]
   (fn [] (link :rel "stylesheet" :href url :integrity sri :crossorigin "anonymous"))))


(defn jsfn
  ([url]
   (fn [] (script :type "text/javascript" :src url)))
  ([url sri]
   (fn [] (script :type "text/javascript" :src url :integrity sri :crossorigin "anonymous"))))


(defn asset->dom-fn
  "For a given asset: build a fn that will create a DOM node to load that asset and return it as a typed pair."
  [url & [sri]]
  (cond
    (str/ends-with? url ".min.js")  [:minjs  (if sri (jsfn url sri) (jsfn url))]
    (str/ends-with? url ".js")      [:js     (if sri (jsfn url sri) (jsfn url))]
    (str/ends-with? url ".min.css") [:mincss (if sri (cssfn url sri) (cssfn url))]
    (str/ends-with? url ".css")     [:css    (if sri (cssfn url sri) (cssfn url))]
    :default [:error url]))


;; Better:
;;
;; https://stackoverflow.com/questions/13121948/dynamically-add-script-tag-with-src-that-may-include-document-write
;;
;; See the answer with 9 +1s
;;
;; Also useful:
;;
;; https://www.html5rocks.com/en/tutorials/speed/script-loading/


(defn load-script!
  "Dynamically load a JS file.  script-info can be a nullary function returning a script dom node, a vector
  of the form [\"url\" \"hash\"] from which a script node will be built, or a string containing a url
  from which a script node will be built.  When the script is done loading, script-cb will be called.

  If you want blocking behavior, use this together with jobs/with-blocking."
  [script-info script-cb]
  (let [script-dom-node (cond
                          (fn? script-info)         (script-info)
                          (sequential? script-info) (let [[url sri] script-info] (apply (jsfn url sri) []))
                          (string? script-info)     (apply (jsfn script-info) [])
                          :default                  (throw (str "ERROR: Unable to load script: " script-info)))]
    (dommy/listen! script-dom-node :load script-cb)
    (head! script-dom-node)))


(defn load-scripts!
  "Load the specified scripts one after another using job/submit.  job-name is the job name for the UI.
  script-infos is a sequence of script-info in the format accepted by load-script."
  [job-name script-infos]
  (let [loader-fns (map (fn [script-info] (partial load-script! script-info)) script-infos)]
    (job/submit job-name loader-fns)))


(defn load-asset-map!
  "Loads asset map of the form {\".js\" jsfn \".css\" cssfn} (etc.) into the page."
  [asset-map script-cb]

  (let [css (or (get asset-map :mincss) (get asset-map :css))
        css-node (when css (css))
        js (or (get asset-map :minjs) (get asset-map :js))
        js-node (when js (js))]

    (when css-node
      (head! css-node))
    (when js
      (load-script! js script-cb))))


(defn asset-map-loadfn [asset-map] (partial load-asset-map! asset-map))


(defn load-asset-maps!
  "Dynamically load the assets in the specified asset maps in order using job/submit.  job-name is
  the job name for the UI.  asset-maps is a sequence of asset maps in the form used by codemirror-assets."
  [job-name asset-maps]
  (job/submit job-name (map asset-map-loadfn asset-maps)))
