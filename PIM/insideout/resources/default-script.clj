;; Default InsideOut startup script
(dynamo/resolve-sources)
(nrepl-server/start! :cider)

(loop []
  (Thread/sleep 1000)
  (recur))
