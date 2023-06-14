(ns insideout.dynamo.repo)

(def github {:ssh-url   "git@github.com:${group}/${archive}.git"
             :https-url "https://github.com/${group}/${archive}.git"})

(def gitlab {:ssh-url   "git@gitlab.com:${group}/${archive}.git"
             :https-url "https://gitlab.com/${group}/${archive}.git"})
