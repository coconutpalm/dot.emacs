(ns web.editor.core
  "Build the Core UI; manage CDN dependencies"
  (:require
   [web.model :as model]
   [util.jobs :as job]
   [web.page.html :as html]
   [web.component.busy :as bu]
   [web.editor.codemirror-assets :as assets]
   [web.editor.codemirror-tablist :as cm-ext]

   [clojure.string :as str]
   [javelin.core :as j :refer [defc cell cell=]]))


(def default-addons
  ["comment/comment" "comment/continuecomment"
   "runmode/colorize"
   "mode/loadmode" "mode/overlay" "mode/multiplex" "mode/simple"
   "dialog/dialog"
   "scroll/annotatescrollbar"
   "search/searchcursor"
   "search/jump-to-line" "search/match-highlighter" "search/search" "search/matchesonscrollbar"
   "display/fullscreen" "display/placeholder"
   "edit/matchbrackets" "edit/closebrackets"
   "edit/matchtags" "edit/closetag"
   "edit/continuelist"
   "fold/brace-fold" "fold/comment-fold" "fold/foldcode" "fold/foldgutter" "fold/indent-fold" "fold/markdown-fold" "fold/xml-fold"
   "hint/anyword-hint" "hint/css-hint" "hint/html-hint" "hint/javascript-hint" "hint/show-hint" "hint/sql-hint" "hint/xml-hint"
   "css-lint" "html-lint" "javascript-lint" "json-lint" "lint/lint" "yaml-lint"
   "merge"
   "hint/show-hint" "hint/javascript-hint" "hint/xml-hint" "hint/html-hint" "hint/css-hint" "hint/sql-hint"
   "lint/lint" "lint/html-lint" "lint/json-lint" "lint/javascript-lint" "lint/coffeescript-lint" "lint/css-lint"
   "tern/tern"])

(def default-modes
  ["clojure" "coffeescript" "css" "diff" "gfm"
   "go" "groovy" "haskell-literate" "haskell" "htmlembedded"
   "htmlmixed" "http" "javascript" "livescript" "markdown" "pegjs" "perl"
   "php" "powershell" "properties" "protobuf" "python" "r" "sass"
   "shell" "vue" "xml" "xquery" "yaml-frontmatter" "yaml" "clike"])


(defn resource-loaders
  "Given a sequence of resources to load, return a sequence of Fn [continuation] to load them."
  [lookup resource-list]
  (letfn [(resource-map-loadfn [resource-name]
            (-> resource-name
               (str/split "/")
               last
               lookup
               html/asset-map-loadfn))]

    (map resource-map-loadfn resource-list)))


(def highlightjs-loader (partial html/load-script! (html/jsfn
                                                    "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.10/highlight.min.js"
                                                    "sha256-1zu+3BnLYV9LdiY85uXMzii3bdrkelyp37e0ZyTAQh0=")))
(def remarkable-loader (partial html/load-script! (html/jsfn
                                                   "https://cdnjs.cloudflare.com/ajax/libs/remarkable/2.0.0/remarkable.min.js"
                                                   "sha256-blWQoMt0+lqHvv7q7wShOwoWOiQITkNA/Bssik2U/08=")))
(def load-cm-metadata (partial html/load-script! (html/jsfn
                                                  "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.48.4/mode/meta.min.js"
                                                  "sha256-9B2kGvilmLfdt3WESTA5SzsGeqD6fuihmhIDssR9tGs=")))
(def load-parinfer (partial html/load-script! (html/jsfn "https://cdn.jsdelivr.net/npm/parinfer-codemirror@1.4.2/parinfer-codemirror.min.js")))
(def cm-loader (html/asset-map-loadfn assets/codemirror))
(def mde-loader (partial html/load-script! "simplemde.js"))


(defn SimpleMDE
  "Custom constructor for SimpleMDE ensuring dependencies are loaded"
  [options and-then]
  (letfn
      [(open-editor []
         (reset! bu/busy? false)
         (and-then (js/SimpleMDE. options)))

       (load-js-then-open []
         (job/submit "Loading CodeMirror" [cm-loader])

         (job/wait-for
          "Waiting for CodeMirror to be available"
          #(exists? js/CodeMirror)
          #(do
             (job/submit "Loading CodeMirror meta" [load-cm-metadata])
             (job/submit "Loading CodeMirror addons" (resource-loaders assets/addon default-addons))
             (job/submit "Loading CodeMirror modes" (resource-loaders assets/mode default-modes))
             (job/submit "Loading Parinfer" [load-parinfer])
             (cm-ext/markdown-list-indent-mode)

             (job/wait-for
              "Waiting for addons/modes to load"
              (fn [] (exists? (.getMode js/CodeMirror "overlay")))
              (fn [] (do
                       (job/submit "Loading SimpleMDE" [mde-loader])
                       (job/wait-for
                        "Waiting for SimpleMDE and its UI mount-point to be available"
                        (fn [] (and (exists? js/SimpleMDE)
                                    (exists? model/editor)))
                        open-editor)

                       (job/submit "Loading HighlightJS" [highlightjs-loader])
                       (job/submit "Loading Remarkable renderer" [remarkable-loader])))))))]

      (if (exists? js/SimpleMDE)
        (open-editor)
        (load-js-then-open))))


(let [file-ext-regex #".+\.([^.]+)$"]

  (defn set-mode!
    "Set the editor's mode based on filename.  Defaults to Github Markdown if no mode can be determined from filename."
    [editor filename]
    (let [ext (last (re-seq file-ext-regex filename))
          info (if ext
                 (or (.findModeByExtension editor ext) (.findModeByMIME editor "text/x-gfm"))
                 (.findModeByMIME editor "text/x-gfm"))
          mode (.-mode info)
          spec (.-mime info)]
      (when mode
        (.autoLoadMode editor mode)
        (.setOption editor "mode" spec)))))


;; Markdown rendering/highlighting

(defn- highlighter
  "Run highlight.js during Markdown translation"
  [code lang]
  (if (and lang (js/hljs.getLanguage lang))
    (try (.-value (js/hljs.highlight lang code))
         (catch js/Error e))
    (try (.-value (js/hljs.highlightAuto (code)))
         (catch js/Error e ""))))

(defn render-preview
  "Build a Markdown HTML preview.  If mdocfile is defined, uses that, else uses the edior text."
  [md]
  (let [real-md (if (= "" @model/mdocfile)
                  md
                  @model/mdocfile)]
    (when (exists? js/remarkable)
      (let [converter (js/remarkable.Remarkable.
                       "full"
                       (clj->js {:html true
                                 :typographer true
                                 :highlight highlighter}))]
        (.render converter real-md)))))


;; Core UI initialization

(defn page-init []
  (SimpleMDE
   (clj->js
    {:autofocus true
     :spellChecker false
     :autosave {:enabled true
                :uniqueId "edit"
                :delay 5000}
     :indentWithTabs false
     :previewRender render-preview
     :promptURLs true})
   #(swap! model/editor (constantly %))))
