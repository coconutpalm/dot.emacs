(ns ui.nrepl)

(def ^:dynamic *coordinates*
  {:nrepl
   '[nrepl/nrepl "0.8.3"]

   :cider
   '[[cider/cider-nrepl "0.26.0-SNAPSHOT"]
     [refactor-nrepl "2.5.1"]]

   :cider-middleware
   '[cider.nrepl/cider-middleware
     refactor-nrepl.middleware/wrap-refactor]

   :reveal
   '[vlaaad/reveal "1.3.196"]

   :reveal-middleware
   '[vlaaad.reveal.nrepl/middleware]})
