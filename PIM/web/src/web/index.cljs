(ns ^{:hoplon/page "index.html"} web.index
  (:require
   [hoplon.jquery]
   [hoplon.core :refer [text table tr td input a span div script textarea ul li strong header link title body head html html-meta]]
   [javelin.core :as j :refer [cell cell=]]

   [web.muicss :refer [muicss-cdn]]
   [web.model :as model]
   [web.page.websocket :as ws]
   [web.page.html :as h]
   [web.component.muicss-sidebar :as materialui-sidebar]
   [web.component.busy :as bu]
   [web.component.tree :as t]
   [web.editor.core :as core]))


(html
 (head
  (h/metas)
  (muicss-cdn)
  (link :href "app.css" :rel "stylesheet" :type "text/css")

  (title (cell= model/document-name)))

 (body
  bu/busy-twirly
  (t/file-tree-sidedrawer model/file-selected)

  (header :id "header" :class "mui-appbar mui--z1 mui--appbar-line-height"
          (div :class "mui-container-fluid"
               (a :class "sidedrawer-toggle mui--visible-xs-inline-block mui--visible-sm-inline-block js-show-sidedrawer" "☰")
               (table :class "mui--hidden-xs mui--hidden-sm appbar-table"  :width "100%"
                      (tr :style "vertical-align:middle"
                          (td :class "appbar-table"
                              (div (a :class "sidedrawer-toggle js-hide-sidedrawer" "☰")
                                   (span :class "mui--text-title" (text "~{model/document-name}"))))
                          (td :class "appbar-table" :align "right"
                              (div :class "mui--text-title mui--text-light-hint" (text "~{ws/state}")))))))

  (div :class "mui--appbar-height")
  (div :id "content-wrapper" :class "mui-container-fluid"
       (textarea :id "Editor" :style "display:none"))))


(materialui-sidebar/install-animation)
(core/page-init)
