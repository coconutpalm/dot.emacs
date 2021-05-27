(ns web.component.tree
  (:require
   [clojure.string :as str]
   [hoplon.core :refer [div defelem cond-tpl ul li a p input strong text]]
   [javelin.core :as j :refer [cell cell=]]
   [web.model :as m]
   [web.page.websocket :refer [connection-state message-as-string]]))



(defn- is-a? [kind item] (= (:type item) kind))

(defn with-item [item & {:keys [if-file if-dir else]}]
  (cond
    (is-a? "file" item) (if-file)
    (is-a? "dir"  item) (if-dir)
    :else               (else)))


(defn tree-level [level-items]
  (letfn [(tree-item [item] (with-item item
                              :if-file #(li :class "file-item" (a :href (str "#" (:path item)) (:name item)))
                              :if-dir  #(vector (li :class "folder-item" (:name item))
                                                (li :class "folder-children"
                                                    (tree-level (:children item))))
                              :else    #(li :class "file-item" (str "!!" (:type item)))))]

    (ul :class "folder-container" (map tree-item level-items))))


(defn filter-tree [file-tree search-filter]
  (letfn [(filter-item [item] (with-item item
                                :if-file #(if (str/includes? (:name item) search-filter)
                                            [item]
                                            [])
                                :if-dir  #(let [remaining-children (mapcat filter-item (:children item))]
                                            (if (first remaining-children)
                                              [(update-in item [:children] (constantly remaining-children))]
                                              []))
                                :else    #(vector)))]

    (mapcat filter-item file-tree)))


(defn current-selection [file-tree search-filter]
  (when (and search-filter (not= search-filter ""))
    (letfn [(filter-item [item] (with-item item
                                  :if-file #(vector item)
                                  :if-dir  #(mapcat filter-item (:children item))
                                  :else    #(vector)))]

      (mapcat filter-item file-tree))))


(defn file-tree-sidedrawer [on-select-callback]
  (let [search-filter  (cell "")        ; Where the <input> field's value lives
        filtered-items (cell= (filter-tree m/file-tree search-filter))
        selected-item  (cell= (first (current-selection filtered-items search-filter)))]

    (div :id "sidedrawer" :class "mui--no-user-select"
         (div :id "sidedrawer-brand"
              :class "folder-searchbox mui--appbar-line-height mui-textfield mui-panel"

              (input :type "text"
                     :placeholder "filter || >command || ?>query"
                     :value search-filter
                     :change #(do
                                (on-select-callback @selected-item)
                                (reset! search-filter ""))
                     :input  #(reset! search-filter @%)))

         (ul :class "folder-container"
             (cell= (map (fn [item] (with-item item
                                     :if-file #(li :class "file-item" (a :href (str "#" (:path item)) (:name item)))
                                     :if-dir  #(vector (li :class "folder-item" (:name item))
                                                       (li :class "folder-children"
                                                           (tree-level (:children item))))
                                     :else    #(li :class "file-item" (str "!! Unrecognized type" (:type item)))))
                     filtered-items))))))
