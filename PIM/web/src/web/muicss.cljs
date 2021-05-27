(ns web.muicss
  (:require [hoplon.core :refer [link script]]))

(defn muicss-cdn
  "MUICSS - Material UI CSS"
  []

  [(link :href "//cdnjs.cloudflare.com/ajax/libs/animate.css/3.5.2/animate.min.css" :rel "stylesheet" :type "text/css")
   (link :href "//cdn.muicss.com/mui-0.10.0/extra/mui-colors.min.css" :rel "stylesheet" :type "text/css")
   (script :src "//cdn.muicss.com/mui-0.10.0/extra/mui-combined.min.js")
   (link :href "//fonts.googleapis.com/css?family=Roboto:300,400,500,700" :rel "stylesheet" :type "text/css")])
