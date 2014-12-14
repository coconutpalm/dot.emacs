;;; .emacs --- Dave's Emacs configuration -*- emacs-lisp -*-
;;;
;;; Commentary:
;;;  None.

;;; Code:

;;; General settings


;;; I initialize my *scratch* buffer with a random Emacs haiku drawn
;;; from among these:

(defvar ted-emacs-haiku
  '("Oort is so awesome
     deuglifies Outlook crap
     `W k' rocks"
    "Great clouds overhead
     Tiny black birds rise and fall
     Snow covers Emacs
         -- Alex Schroeder"
    "hacking on Smyrno
     `error in process filter'
     something is b0rken"
    "Swiftly typing. Oh!
     Where would we be without you,
     `self-insert-command'?"
    "treeless quiet field
     sudden bud: EmacsWiki
     now he{ar,re} the birds sing
         -- ttn"
    "an emacs user's
     fingers dance on the keyboard;
     a nerd pianist
         -- Erik Bourget"
    "The file was open.
     flying in a sparrow stole
     a parenthesis
         -- Oliver Scholz"
    "The day went away.
     The file still puts its weight on
     the tired mode-line.
         -- Oliver Scholz"
    "On a cloudy day
     you hear the cons cells whisper:
     'We are lost and gone.'
         -- Oliver Scholz"
    "A message, a string
     remind me of my sweet love.
     Good bye, my buffers.
         -- Oliver Scholz"
    "Hot night in summer:
     Hush, you quibbling characters!
     Do not wake her up!
         -- Oliver Scholz"
    "A bright, busy day.
     The windows watch a thousand
     wild cursors dancing.
         -- Oliver Scholz"
    "Oh, why don't you are
     a lake, a stream, a meadow
     this morning, Emacs?
         -- Oliver Scholz" ;%
    "The friends chat gaily,
     I stand up to join their talk.
     My `save-excursion'.
         -- Oliver Scholz")
  "Haiku taken from the Emacs Wiki's EmacsHaiku page.")

(defun ted-random-emacs-haiku (&optional prefix)
  "Select and format a random haiku from `ted-emacs-haiku'."
  (random t)
  (let* ((prefix (or prefix ";; "))
xc         (n (random (length ted-emacs-haiku)))
         (haiku (nth n ted-emacs-haiku)))
    (with-temp-buffer
      (insert haiku)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (goto-char (point-at-bol))
        (delete-horizontal-space)
        (insert prefix)
        (when (looking-at "--")
          (insert "    "))
        (forward-line 1))
      (concat (buffer-substring-no-properties (point-min) (point-max))
              "\n\n"))))

(setq initial-scratch-message (ted-random-emacs-haiku))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)
(when window-system (global-unset-key "\C-z"))
(when window-system (set-frame-size (selected-frame) 120 80))
(setq x-select-enable-clipboard t) ; enable use of system clipboard across emacs and applications
(setq-default fill-column 120)
(setq-default standard-indent 3) ; set standard indent to 3 rather that 4
(setq-default tab-width 3)
(setq-default indent-tabs-mode nil) ; spaces instead of tabs by default

(when (eq system-type 'darwin)

  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "Courier")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  (set-face-attribute 'default nil :height 140)

  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  ;; you may want to add different for other charset in this way.
  )

(setq initial-major-mode 'text-mode)
(setq-default major-mode 'text-mode)

(global-linum-mode t)
(global-hl-line-mode 1) ; highlight current line, turn it on for all modes by default
(setq column-number-mode t)
;;(set-face-attribute 'default nil :height 200) ; 9 point fonts by default

(setq display-time-day-and-date t
      display-time-24hr-format nil)
(display-time)
(setq frame-title-format (concat  "%b - emacs@" (system-name))) ;; default to better frame titles

;; These are two nice themes, leuven and professional
(set-face-background 'fringe "white") ; Hide the fringe mark on the left
(setq-default indicate-empty-lines t)
(setq-default highlight-changes-mode 1)
(setq-default indicate-buffer-boundaries 'right)


(delete-selection-mode 1) ; typing with the mark active will overwrite the marked region
(transient-mark-mode 1) ; enable visual feedback on selections, default since v23


;; backup
(setq make-backup-files nil) ; stop making backup ~ files
(setq backup-inhibited t) ; disable backup for a per-file basis, not to be used by major modes

;; auto revert
(global-auto-revert-mode 1) ; auto-refresh all buffers, does not work for remote files
(setq-default auto-revert-interval 10) ; default is 5 s
;(auto-revert-tail-mode t) ; auto-revert if file grows at the end, also works for remote files
(setq-default auto-revert-verbose nil) 


(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key (kbd "M-[ h") 'beginning-of-line) ;; Fix for Terminal.app
(global-set-key (kbd "M-[ f") 'end-of-line)       ;; Fix for Terminal.app
(global-set-key (kbd "\C-c g") 'goto-line)
(global-set-key (kbd "\C-c c") 'compile)

;; Web-mode
(add-to-list 'load-path "~/.emacs.d/elisp/web-mode")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Scala-mode from the Git repo
;(when (>= emacs-major-version 24) 
;  (add-to-list 'load-path "~/.emacs.d/elisp/scala-mode2")
;  (require 'scala-mode2) )

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/elisp-root")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start t)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; Temporararily remove yasnippet from ac-sources until auto-complete gets fixed
(delq 'ac-source-yasnippet ac-sources)

(defun my-semicolon ()
  (interactive)
  (insert ";")
  (newline-and-indent))

(defun my-colon ()
  (interactive)
  (insert ":"))

(defun my-brace ()
  (interactive)
  (insert "{")
  (newline-and-indent))

(defun my-closebrace ()
  (interactive)
  (insert "}")
  (indent-according-to-mode)
  (newline-and-indent))

(add-hook 'css-mode-hook
  (lambda ()
     (make-local-variable 'ac-ignores)
     (add-to-list 'ac-ignores ";")
     (add-to-list 'ac-ignores ":")
     (add-to-list 'ac-ignores "{")
     (define-key (current-local-map) (kbd ";") 'my-semicolon)
     (define-key (current-local-map) (kbd ":") 'my-colon)
     (define-key (current-local-map) (kbd "}") 'my-closebrace)
     (define-key (current-local-map) (kbd "{") 'my-brace) ))

(add-hook 'ruby-mode-hook
          (lambda ()
            (make-local-variable 'ac-ignores)
            (add-to-list 'ac-ignores "end")))

;; yasnippet
(add-to-list 'load-path
	     "~/.emacs.d/elisp")
(add-to-list 'load-path
	     "~/.emacs.d/elisp/yasnippet")
(require 'yasnippet)
;;(yas/load-directory "~/snippets")
(yas-global-mode 1)
(add-to-list 'ac-sources 'ac-source-yasnippet)

;; flymake-jslint
(add-to-list 'load-path "~/.emacs.d/elisp/lintnode")
(require 'flymake-jslint)
;; Make sure we can find the lintnode executable
(setq lintnode-location "~/.emacs.d/elisp/lintnode")
;; JSLint can be... opinionated
(setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
;; Start the server when we first open a js file and start checking
(add-hook 'js-mode-hook
          (lambda ()
            (lintnode-hook)))
;; Put messages in the mini-buffer
(custom-set-variables
     '(help-at-pt-timer-delay 0.9)
     '(help-at-pt-display-when-idle '(flymake-overlay)))

;; General Javascript
(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)))

;; Javascript REPL
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'js-comint)
;; Use node as our repl
(setq inferior-js-program-command "/usr/bin/nodejs")
 
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                     (replace-regexp-in-string ".*1G.*3G" "> " output)))))

;; Load the ensime lisp code...
(setq exec-path (append exec-path (list "~/liftweb" )))
(add-to-list 'load-path "~/.emacs.d/elisp/ensime/elisp/")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Scala-mode settings
(add-hook 'scala-mode-hook '(lambda ()

  ;; Bind the 'newline-and-indent' command to RET (aka 'enter'). This
  ;; is normally also available as C-j. The 'newline-and-indent'
  ;; command has the following functionality: 1) it removes trailing
  ;; whitespace from the current line, 2) it create a new line, and 3)
  ;; indents it.  An alternative is the
  ;; 'reindent-then-newline-and-indent' command.
  (local-set-key (kbd "RET") 'newline-and-indent)

  ;; Alternatively, bind the 'newline-and-indent' command and
  ;; 'scala-indent:insert-asterisk-on-multiline-comment' to RET in
  ;; order to get indentation and asterisk-insertion within multi-line
  ;; comments.
  ;; (local-set-key (kbd "RET") '(lambda ()
  ;;   (interactive)
  ;;   (newline-and-indent)
  ;;   (scala-indent:insert-asterisk-on-multiline-comment)))

  ;; Bind the 'join-line' command to C-M-j. This command is normally
  ;; bound to M-^ which is hard to access, especially on some European
  ;; keyboards. The 'join-line' command has the effect or joining the
  ;; current line with the previous while fixing whitespace at the
  ;; joint.
  (local-set-key (kbd "C-M-j") 'join-line)

  ;; Bind the backtab (shift tab) to
  ;; 'scala-indent:indent-with-reluctant-strategy command. This is usefull
  ;; when using the 'eager' mode by default and you want to "outdent" a
  ;; code line as a new statement.
  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)

  ;; and other bindings here
))

(setq ensime-sem-high-faces
  '(
   (var . (:foreground "#ff2222"))
   (val . (:foreground "#dddddd"))
   (varField . (:foreground "#ff3333"))
   (valField . (:foreground "#dddddd"))
   (functionCall . (:foreground "#84BEE3"))
   (param . (:foreground "#ffffff"))
   (class . font-lock-type-face)
   (trait . (:foreground "#084EA8"))
   (object . (:foreground "#026DF7"))
   (package . font-lock-preprocessor-face)
   ))

; Java/Groovy configuration

(add-to-list 'load-path "~/.emacs.d/groovy")
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager-managed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialize the package manager with the MELPA archive
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-refresh-contents) 

(package-initialize)


;; smart tabs (indent with tabs, align with spaces)
(unless (package-installed-p 'smart-tabs-mode)
  (package-install 'smart-tabs-mode))
(require 'smart-tabs-mode)

;;(global-smart-tab-mode 1)
(autoload 'smart-tabs-mode "smart-tabs-mode"
  "Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")
(autoload 'smart-tabs-insinuate "smart-tabs-mode")
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)


; Clojure
(unless (package-installed-p 'cider)
  (package-install 'cider))
(require 'cider)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(unless (package-installed-p 'ac-cider)
  (package-install 'ac-cider))
(require 'ac-cider)

(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))
(require 'clojure-mode)

(unless (package-installed-p 'clojure-mode-extra-font-locking)
  (package-install 'clojure-mode-extra-font-locking))
(require 'clojure-mode-extra-font-locking)

(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(require 'auto-complete)

(unless (package-installed-p 'paredit)
  (package-install 'paredit))
(require 'paredit)

(unless (package-installed-p 'popup)
  (package-install 'popup))
(require 'popup)

(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))
(require 'rainbow-delimiters)

(unless (package-installed-p 'rainbow-mode)
  (package-install 'rainbow-mode))
(require 'rainbow-mode)


; Dependencies / misc

(unless (package-installed-p 'dash)
  (package-install 'dash))
(require 'dash)

(unless (package-installed-p 'f)
  (package-install 'f))
(require 'f)

(unless (package-installed-p 's)
  (package-install 's))
(require 's)

(unless (package-installed-p 'tabbar)
  (package-install 'tabbar))

(unless (package-installed-p 'tree-mode)
  (package-install 'tree-mode))
(require 'tree-mode)

(unless (package-installed-p 'windata)
  (package-install 'windata))
(require 'windata)

(unless (package-installed-p 'dirtree)
  (package-install 'dirtree))
(require 'dirtree)

(autoload 'dirtree "dirtree" "Add directory to tree view" t)
(global-set-key "\C-\\" 'dirtree-show)

;; enable tabbar minor mode
;(setq tabbar-use-images nil) ; speed up by not using images
(tabbar-mode 1)
(global-set-key [M-left] 'tabbar-backward-tab)
(global-set-key [M-right] 'tabbar-forward-tab)

;; tabbar coloring code...
(set-face-attribute
   'tabbar-default nil
   :background "gray60")
  (set-face-attribute
   'tabbar-unselected nil
   :background "gray85"
   :foreground "gray30"
   :box nil)
  (set-face-attribute
   'tabbar-selected nil
   :background "#f2f2f6"
   :foreground "blue"
   :box nil)
  (set-face-attribute
   'tabbar-button nil
   :box '(:line-width 1 :color "gray72" :style released-button))
  (set-face-attribute
   'tabbar-separator nil
   :height 1.0)

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))
;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
      (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))

(defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
  `(defun ,name (arg)
     (interactive "P")
     ,do-always
     (if (equal nil arg)
         ,on-no-prefix
       ,on-prefix)))

(defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
(defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
(global-set-key [(control tab)] 'shk-tabbar-next)
(global-set-key [(control shift tab)] 'shk-tabbar-prev)

;; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " + " (concat ad-return-value " "))
          (concat " " (concat ad-return-value " ")))))

;; Called each time the modification state of the buffer changed.
(defun ztl-modification-state-change ()
   (tabbar-set-template tabbar-current-tabset nil)
   (tabbar-display-update))

;; First-change-hook is called BEFORE the change is made.
(defun ztl-on-buffer-modification ()
   (set-buffer-modified-p t)
   (ztl-modification-state-change))

(add-hook 'after-save-hook 'ztl-modification-state-change)
;; This doesn't work for revert, I don't know.
;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

(setq tabbar-cycle-scope 'tabs)
(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
   "Return the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with '*', plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
               ((eq major-mode 'dired-mode) "emacs")
               (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;;w3m
(unless (package-installed-p 'w3m)
  (package-install 'w3m))
(require 'w3m)

(when (locate-library "w3m")
  (autoload 'w3m "w3m" nil t)
  (autoload 'w3m-goto-url "w3m" nil t)
  (autoload 'w3m-region "w3m")

  (setq w3m-home-page
        (if (file-readable-p "~/html/home.html")
            (concat "file://" (expand-file-name "~/html/home.html"))
          "http://www.google.com"))

  (setq w3m-use-toolbar t
        w3m-use-tab     nil
        w3m-key-binding 'info)

  (setq w3m-search-default-engine "google")

  (setq w3m-command-arguments       '("-F" "-cookie")
        w3m-mailto-url-function     'compose-mail
        browse-url-browser-function 'w3m
        mm-text-html-renderer       'w3m)

  (add-hook 'w3m-mode-hook 'ted-hide-trailing-whitespace)

  (eval-after-load "w3m"
    '(define-key w3m-mode-map (kbd "z") 'bury-buffer))

  (defalias 'eshell/w3m 'w3m)

  (setq w3m-use-cookies t)
  (setq w3m-cookie-accept-bad-cookies t)

  (defun ted-w3m-edit-emacswiki-page (url)
    (let ((node (substring (substring w3m-current-url
                                      (string-match "wiki[/?][^/&=]+\\'"
                                                    w3m-current-url))
                           5)))
      (w3m-goto-url (concat "http://www.emacswiki.org/cgi-bin/wiki"
                            "?action=edit;id=" node))))

  (defun ted-delicious-url ()
    "Bookmark this page with del.icio.us."
    (interactive)
    (w3m-goto-url
     (concat "http://del.icio.us/hober?"
             "url="    (w3m-url-encode-string w3m-current-url)
             "&title=" (w3m-url-encode-string w3m-current-title))))

  (eval-after-load "w3m"
    '(progn
       (add-to-list 'w3m-uri-replace-alist
                    '("\\`lj:\\(.+\\)" w3m-pattern-uri-replace
                      "http://www.livejournal.com/users/\\1/"))
       (add-to-list 'w3m-edit-function-alist
                    '(".*emacswiki.org/cgi-bin/wiki.*"
                      . ted-w3m-edit-emacswiki-page))
       (define-key w3m-info-like-map "a" 'ted-delicious-url))))

(defadvice org-open-at-point (around org-open-at-point-choose-browser activate)
  (let ((browse-url-browser-function
         (cond ((equal (ad-get-arg 0) '(4))
                'browse-url-generic)
               ((equal (ad-get-arg 0) '(16))
                'choose-browser)
               (t
                (lambda (url &optional new)
                  (w3m-browse-url url t)))
               )))
    ad-do-it))


(unless (package-installed-p 'org)
  (package-install 'org))
(require 'org)


;; org mode hooks
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'visual-line-mode)
;; turn on soft wrapping mode for org mode
(add-hook 'org-mode-hook 
          (lambda () (setq truncate-lines nil)))
(setq org-completion-use-ido t)
(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)) t)

;; Scala-mode from the package manager
(unless (package-installed-p 'scala-mode2)
  (package-install 'scala-mode2))

; Flycheck
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))

(add-hook 'after-init-hook #'global-flycheck-mode)

; Flycheck-tip
(unless (package-installed-p 'flycheck-tip)
  (package-install 'flycheck-tip))

(require 'flycheck-tip)
(global-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)

; Malabar Mode (for Java)
(unless (package-installed-p 'malabar-mode)
  (package-install 'malabar-mode))

(unless (package-installed-p 'cedet)
  (package-install 'cedet))

(unless (package-installed-p 'semantic)
  (package-install 'semantic))

; Java / Malabar mode
(require 'cedet)
(require 'semantic)
;(require 'semantic/semanticdb-javap)
(require 'semantic/ia)
(load "semantic/loaddefs.el")
(semantic-mode 1);;
;(require 'malabar-mode)
;(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

                                        ; tab bar
(defun my-java-malabar-mode-hook ()
  ;; IDEA default for jump to source
  (define-key c-mode-base-map "\C-\M-g" 'malabar-jump-to-thing)
  (global-set-key "\M-n" 'semantic-ia-complete-symbol)
  )
(add-hook 'c-mode-common-hook 'my-java-malabar-mode-hook)


;; Compiling the file on save makes malabar display the errors in the
;; Java source code.
(add-hook 'malabar-mode-hook
          (lambda () 
            (add-hook 'after-save-hook 'malabar-compile-file-silently
                      nil t)))

(require 'compile)
(setq compilation-error-regexp-alist
  (append (list
           ;; works for jikes
           '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
           ;; works for javac
           '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2)
           ;; works for maven 2.x
           '("^\\(.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3)
           ;; works for maven 3.x
           '("^\\(\\[ERROR\\] \\)?\\(/[^:]+\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 2 3 4)
           '("^\\(\\[WARNING\\] \\)?\\(/[^:]+\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 2 3 4)
           )
          compilation-error-regexp-alist))


(if (boundp 'show-trailing-whitespace)
    (progn
      (setq-default show-trailing-whitespace t)

      (defun ted-hide-trailing-whitespace ()
        "Do not highlight trailing whitespace in this buffer."
        (interactive)
        (setq show-trailing-whitespace nil))

      (defun ted-show-trailing-whitespace ()
        "Highlight trailing whitespace in this buffer."
        (interactive)
        (setq show-trailing-whitespace t))

      (defun ted-toggle-show-trailing-whitespace ()
        "Highlight trailing whitespace in this buffer."
        (interactive)
        (setq show-trailing-whitespace (not show-trailing-whitespace)))

      (mapc (lambda (mode-hook)
              (add-hook mode-hook
                        'ted-hide-trailing-whitespace))
            '(Buffer-menu-mode-hook custom-mode-hook text-mode-hook
              term-mode-hook Info-mode-hook comint-mode-hook
              buffer-menu-mode-hook apropos-mode-hook
              tooltip-show-hook gnus-article-mode-hook mail-mode-hook
              gnus-summary-mode-hook message-mode-hook
              gnus-group-mode-hook eshell-mode-hook w3-mode-hook
              initial-calendar-window-hook))

      (mapc (lambda (mode-hook)
              (add-hook mode-hook
                        (lambda ()
                          (setq show-trailing-whitespace t))))
            '(latex-mode-hook LaTeX-mode-hook html-mode-hook)))
  (defalias 'ted-hide-trailing-whitespace 'ignore))

;;; Misc key bindings

(global-set-key [f1] 'shell)
(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'split-window-horizontally)
(global-set-key [f4] 'delete-other-windows)
;(global-set-key [f6] 'list-buffers)
(global-set-key [f6] 'ibuffer)
(global-set-key [\C-f6] 'other-window) ; switch to the other buffer
(global-set-key "\C-c z" 'repeat)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-z") 'undo)

;;; (provide 'emacs-init)
;;; emacs-init.el ends here

