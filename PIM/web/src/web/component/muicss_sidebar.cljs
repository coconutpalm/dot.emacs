(ns web.component.muicss-sidebar)


(defn install-animation
  "auto-translated from muicss Javascript example using
  https://roman01la.github.io/javascript-to-clojurescript/"
  []
  (js/jQuery
   (fn [$]
     (let [$bodyEl ($ "body")
           $sidedrawerEl ($ "#sidedrawer")]

       (defn showSidedrawer []
         (let [options
               #js {:onclose (fn []
                               (.appendTo
                                (.removeClass $sidedrawerEl "active")
                                (.-body js/document)))}

               $overlayEl ($ (.overlay js/mui "on" options))]

           (.appendTo $sidedrawerEl $overlayEl)
           (js/setTimeout (fn [] (.addClass $sidedrawerEl "active")) 20)))

       (defn hideSidedrawer [] (.toggleClass $bodyEl "hide-sidedrawer"))

       (.on ($ ".js-show-sidedrawer") "click" showSidedrawer)
       (.on ($ ".js-hide-sidedrawer") "click" hideSidedrawer)

       (def $titleEls ($ "strong" $sidedrawerEl))

       (.hide (.next $titleEls))

       (.on $titleEls "click" (fn []
                                (->
                                 ($ (this-as this))
                                 (.next)
                                 (.slideToggle 200))))))))
