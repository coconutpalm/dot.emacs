(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(hsys-org-enable-smart-keys (quote buttons))
 '(package-selected-packages
   (quote
    (helm-slack websocket alert oauth2 emojify graphiql-mode walkman impostman httprepl npm-mode bookmarks+ bookmark+ xwwp-follow-link-helm xwwp github-review magit-popup magit-gh-pulls redo-plus redoplus redo+ olivetti cider-repl writeroom-mode deft tide weatherline-mode weatherline nyan-mode smart-mode-line-powerline-theme centaur-tabs all-the-icons tree-mode smart-mode-line cider-hydra helm-cider kaocha-runner clojure-mode-extra-font-locking lispy clj-refactor clojure-mode smartparens smart-tabs-mode lsp-ui lsp-metals lsp-mode sbt-mode scala-mode play-routes-mode dockerfile-mode docker-compose-mode docker-api docker popup-imenu goto-chg highlight-symbol treemacs-magit treemacs-icons-dired treemacs-projectile treemacs perspective helm-projectile projectile helm-descbinds helm-ag helm git-timemachine magit git-gutter edbi adoc-mode markdown-mode+ markdown-toc markdown-mode impatient-mode simple-httpd web-mode json-mode sass-mode js-comint prettier-js xref-js2 js2-mode typescript-mode restclient jenkinsfile-mode groovy-mode flycheck-cask flycheck-status-emoji flycheck-pos-tip flycheck-color-mode-line flycheck spinner exec-path-from-shell multiple-cursors discover which-key hydra company-box company-quickhelp company epl dimmer nginx-mode ag base16-theme package-utils use-package)))
 '(safe-local-variable-values
   (quote
    ((eval with-eval-after-load
           (quote cider)
           (setq cider-default-cljs-repl
                 (quote figwheel)))
     (dockerfile-image-name . "connect-devenv"))))
 '(sml/mode-width
   (if
       (eq
        (powerline-current-separator)
        (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote sml/global)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes))))

;; Use 2*fibonacci number to determine heights
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#2eae2eae2eae"))) t)
 '(company-scrollbar-fg ((t (:background "#21e121e121e1"))) t)
 '(company-tooltip ((t (:inherit default :background "#1a331a331a33"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#21cc21cc21cc"))))
 '(company-tooltip-scrollbar-track ((t (:background "#2e992e992e99"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(line-number-current-line ((t (:inherit line-number))))
 '(markdown-header-delimiter-face ((t (:inherit markdown-markup-face :weight bold))))
 '(markdown-header-face ((t (:inherit font-lock-type-face))))
 '(markdown-header-face-1 ((t (:inherit font-lock-type-face :height 2.0))))
 '(markdown-header-face-2 ((t (:inherit font-lock-type-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit font-lock-type-face :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit font-lock-type-face :height 1.2))))
 '(markdown-header-face-5 ((t (:inherit font-lock-type-face :underline t))))
 '(markdown-header-face-6 ((t (:inherit font-lock-type-face :underline t))))
 '(markdown-link-face ((t (:inherit font-lock-builtin-face :underline nil))))
 '(markdown-link-title-face ((t (:inherit font-lock-builtin-face :underline nil))))
 '(markdown-url-face ((t (:inherit font-lock-doc-face :underline nil :height 0.7))))
 '(sml/col-number ((t (:inherit sml/global :background "black" :distant-foreground "darkgreen" :foreground "green"))))
 '(sml/folder ((t (:inherit sml/global :background "#151515" :distant-foreground "gray" :foreground "white smoke" :weight normal))))
 '(sml/position-percentage ((t (:inherit sml/prefix :background "#202020" :distant-foreground "dark red" :foreground "firebrick" :weight normal))))
 '(sml/vc ((t (:inherit sml/git :background "#202020" :distant-foreground "SteelBlue4" :foreground "SteelBlue1"))))
 '(sml/vc-edited ((t (:inherit sml/prefix :background "#202020" :foreground "indian red" :slant italic :weight bold)))))
