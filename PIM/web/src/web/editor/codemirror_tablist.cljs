;; CodeMirror, copyright (c) by Marijn Haverbeke and others
;; Distributed under an MIT license: http://codemirror.net/LICENSE
;;
;; Auto-translated from Javascript using
;; https://roman01la.github.io/javascript-to-clojurescript/

(ns web.editor.codemirror-tablist
  (:require [oops.core :refer [oget oset! ocall oapply ocall! oapply!
                               oget+ oset!+ ocall+ oapply+ ocall!+ oapply!+]]))


(defn markdown-list-indent-mode
  "Adds tab/shift-tab list indent/unindent behavior for markdown lists."
  []

  (set!
   (.. js/CodeMirror -commands -tabAndIndentMarkdownList)
   (fn [cm]
     (let [ranges (.listSelections cm)
           pos (oget ranges "0" "head")
           eolState (.getStateAfter cm (.-line pos))
           inList (not= (.-list eolState) false)]

       (cond
         (= inList true) (.execCommand cm "indentMore")
         (oget cm "options" "indentWithTabs") (.execCommand cm "insertTab")
         :else
         (let [spaces (.join (js/Array (+ (.. cm -options -tabSize) 1)) " ")]
           (.replaceSelection cm spaces))))))

  (set!
   (.. js/CodeMirror -commands -shiftTabAndUnindentMarkdownList)
   (fn [cm]
     (let [ranges (.listSelections cm)
           pos (oget ranges "0" "head")
           eolState (.getStateAfter cm (.-line pos))
           inList (not= (.-list eolState) false)]

       (cond
         inList (.execCommand cm "indentLess")
         (oget cm "options" "indentWithTabs") (.execCommand cm "insertTab")
         :else
         (let [spaces (.join (js/Array (+ (.. cm -options -tabSize) 1)) " ")]
           (.replaceSelection cm spaces)))))))
