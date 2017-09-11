;;; .emacs --- Dave's Emacs configuration -*- emacs-lisp -*-
;;;
;;; Commentary:
;;;  None.

;;; Code:

;;; General settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; Configuration settings
;;;

(setq debug-on-error t)
(add-to-list 'load-path "~/.emacs.d/elisp")

(setq nodejs-path "/usr/bin/nodejs")
(setq lein-path "~/bin/lein")

;; In Clojure, Cmd-enter inserts the contents of this file into the current repl
(setq clojure-repl-init-file "~/.repl.clj")

;; On MacOS, make sure we have these environment variables:
(setq macos-copy-from-env-list '("AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY" "PATH" "JAVA_HOME"))

;; My minor mode
(require 'modi-mode)


(defun starts-with (begins s)
      "Return non-nil if string S starts with BEGINS."
      (cond ((>= (length s) (length begins))
             (string-equal (substring s 0 (length begins)) begins))
            (t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialize the package manager with the MELPA archive


(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			;("elpa" . "http://tromy.com/elpa/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;;
;; Upgrade packages automatically on startup
;;  If you don't want this, comment out package-utils-upgrade-all
;;

(unless (package-installed-p 'epl)
  (package-install 'epl))
(require 'epl)

(unless (package-installed-p 'package-utils)
  (package-install 'package-utils))
(require 'package-utils)
;; (package-utils-upgrade-all)



;;;
;;; Generic utilities
;;;

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)


(defun ends-with? (s ending)
      "Return non-nil if string S ends with ENDING."
      (cond ((>= (length s) (length ending))
             (let ((elength (length ending)))
               (string= (substring s (- 0 elength)) ending)))
            (t nil)))


(defun starts-with? (s begins)
 "Return non-nil if string S starts with BEGINS."
     (cond ((>= (length s) (length begins))
            (string-equal (substring s 0 (length begins)) begins))
           (t nil)))



(make-variable-buffer-local 'tags-file-name)
(make-variable-buffer-local 'show-paren-mode)

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(defun add-to-load-path (path)
  "Add PATH to LOAD-PATH if PATH exists."
  (when (file-exists-p path)
    (add-to-list 'load-path path)))
(add-to-load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
;; WORKAROUND http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16449
(add-hook 'nxml-mode-hook (lambda () (flyspell-mode -1)))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b". ibuffer))

(use-package ivy
  :ensure nil)

(use-package subword
  :ensure nil
  :diminish subword-mode
  :config (global-subword-mode t))

(use-package dired
  :ensure nil
  :config
  ;; a workflow optimisation too far?
  (bind-key "C-c c" 'sbt-hydra dired-mode-map)
  (bind-key "C-c e" 'next-error dired-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for generic interactive convenience methods.
;; Arguably could be uploaded to MELPA as package 'fommil-utils.
;; References included where shamelessly stolen.
(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun unfill-paragraph (&optional region)
  ;; http://www.emacswiki.org/emacs/UnfillParagraph
  "Transforms a paragraph in REGION into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun unfill-buffer ()
  "Unfill the buffer for function `visual-line-mode'."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region 0 (point-max))))

(defun revert-buffer-no-confirm ()
  ;; http://www.emacswiki.org/emacs-en/download/misc-cmds.el
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(defun exit ()
  "Short hand for DEATH TO ALL PUNY BUFFERS!"
  (interactive)
  (if (daemonp)
      (message "You silly")
    (save-buffers-kill-emacs)))

(defun safe-kill-emacs ()
  "Only exit Emacs if this is a small session, otherwise prompt."
  (interactive)
  (if (daemonp)
      ;; intentionally not save-buffers-kill-terminal as it has an
      ;; impact on other client sessions.
      (delete-frame)
    (let ((count-buffers (length (buffer-list))))
      (if (< count-buffers 10)
          (save-buffers-kill-emacs)
        (message-box "use 'M-x exit'")))))

(defun declare-buffer-bankruptcy ()
  "Declare buffer bankruptcy and clean up everything using `midnight'."
  (interactive)
  (let ((clean-buffer-list-delay-general 0)
        (clean-buffer-list-delay-special 0))
    (clean-buffer-list)))


(defvar ido-buffer-whitelist
  '("^[*]\\(notmuch\\-hello\\|unsent\\|ag search\\|grep\\|eshell\\).*")
  "Whitelist regexp of `clean-buffer-list' buffers to show when switching buffer.")

(defun midnight-clean-or-ido-whitelisted (name)
  "T if midnight is likely to kill the buffer named NAME, unless whitelisted.
Approximates the rules of `clean-buffer-list'."
  (require 'midnight)
  (require 'dash)
  (cl-flet* ((buffer-finder (regexp) (string-match regexp name))
             (buffer-find (regexps) (-partial #'-find #'buffer-finder)))
    (and (buffer-find clean-buffer-list-kill-regexps)
         (not (or (buffer-find clean-buffer-list-kill-never-regexps)
                  (buffer-find ido-buffer-whitelist))))))

(defun company-or-dabbrev-complete ()
  "Force a `company-complete', falling back to `dabbrev-expand'."
  (interactive)
  (if company-mode
      (company-complete)
    (call-interactively 'dabbrev-expand)))


(defun company-backends-for-buffer ()
  "Calculate appropriate `company-backends' for the buffer.
For small projects, use TAGS for completions, otherwise use a
very minimal set."
  (projectile-visit-project-tags-table)
  (cl-flet ((size () (buffer-size (get-file-buffer tags-file-name))))
    (let ((base '(company-keywords company-dabbrev-code company-yasnippet)))
      (if (and tags-file-name (<= 20000000 (size)))
          (list (push 'company-etags base))
        (list base)))))


(defun plist-merge (&rest plists)
  "Merge property lists"
  (if plists
      (let ((result (copy-sequence (car plists))))
        (while (setq plists (cdr plists))
          (let ((plist (car plists)))
            (while plist
              (setq result (plist-put result (car plist) (car (cdr plist)))
                    plist (cdr (cdr plist))))))
        result)
    nil))

;;
;; Unbind key bindings
;;
;; Use it interactively
;; Or like it's used below
;;

(defun get-key-combo (key)
  "Just return the key combo entered by the user"
  (interactive "Key combo: ")
  key)


(defun keymap-unset-key (key keymap)
    "Remove binding of KEY in a keymap
    KEY is a string or vector representing a sequence of keystrokes."
    (interactive
     (list (call-interactively #'get-key-combo)
           (completing-read "Which map: " minor-mode-map-alist nil t)))
    (let ((map (rest (assoc (intern keymap) minor-mode-map-alist))))
      (when map
        (define-key map key nil)
        (message  "%s unbound for %s" key keymap))))



;;; Misc display settings

(setq inhibit-splash-screen t)
(setq x-select-enable-clipboard t)      ; enable use of system clipboard across emacs and applications
(setq-default fill-column 120)
(setq-default standard-indent 3) ; set standard indent to 3 rather that 4
(setq-default tab-width 3)
(setq scroll-step 1)             ; control screen "leaping"
(setq-default indent-tabs-mode nil) ; spaces instead of tabs by default
(global-linum-mode t)
(global-hl-line-mode 1) ; highlight current line, turn it on for all modes by default
(set-face-background 'hl-line "lightgray")
(setq column-number-mode t)
(global-prettify-symbols-mode)

(setq display-time-day-and-date t
      display-time-24hr-format nil)
(display-time)
(setq frame-title-format (concat  "%b - emacs@" (system-name))) ;; default to better frame titles

(set-face-background 'fringe "white") ; Hide the fringe mark on the left
(setq-default indicate-empty-lines t)
(setq-default highlight-changes-mode 1)
(setq-default indicate-buffer-boundaries 'right)

(delete-selection-mode 1) ; typing with the mark active will overwrite the marked region
(transient-mark-mode 1) ; enable visual feedback on selections, default since v23

(when window-system
  (global-unset-key "\C-z")
  (set-frame-size (selected-frame) 120 37)

  ;; default Latin font (e.g. Consolas)
  (set-default-font "Mononoki 12")

  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  ;; you may want to add different for other charset in this way.
  )


;; No trailing whitespace, please...
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; backup
(setq make-backup-files nil) ; stop making backup ~ files
(setq backup-inhibited t) ; disable backup for a per-file basis, not to be used by major modes

;; auto revert
(global-auto-revert-mode 1) ; auto-refresh all buffers, does not work for remote files
(setq-default auto-revert-interval 10) ; default is 5 s
;(auto-revert-tail-mode t) ; auto-revert if file grows at the end, also works for remote files
(setq-default auto-revert-verbose nil)


;; protects against accidental mouse movements
;; http://stackoverflow.com/a/3024055/1041691
(add-hook 'mouse-leave-buffer-hook
          (lambda () (when (and (>= (recursion-depth) 1)
                           (active-minibuffer-window))
                  (abort-recursive-edit))))


;; *scratch* is immortal
(add-hook 'kill-buffer-query-functions
          (lambda () (not (member (buffer-name) '("*scratch*" "scratch.el")))))


;; ansi-term / multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")

(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-r" "C-s" "C-y" "<ESC>" "<TAB>" "C-[")
  "The key list that will need to be unbind."
  :type 'list
  :group 'multi-term)

(defcustom term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-raw)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("M-r" . term-send-reverse-search-history)
    ("M-," . term-send-input)
    ("M-." . comint-dynamic-complete))
  "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format."
  :type 'alist
  :group 'multi-term)

(defun remote-term (new-buffer-name cmd &rest switches)
  "Ansi-terms on remote hosts."
  (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)
  (term-set-escape-char ?\C-x)
  (switch-to-buffer term-ansi-buffer-name))


(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)
            (setq show-trailing-whitespace nil)
            (yas-minor-mode -1)
            (define-key term-raw-map (kbd "C-y") 'term-paste)))


;; shell-mode
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )

; interpret and use ansi color codes in shell output windows
(require 'ansi-color)

(ansi-color-for-comint-mode-on)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


; make completion buffers disappear after 5 seconds.
(add-hook 'completion-setup-hook
  (lambda () (run-at-time 5 nil
    (lambda () (delete-windows-on "*Completions*")))))



(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))


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
(add-to-list 'auto-mode-alist '("\\.rtml?\\'" . web-mode))


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
(setq inferior-js-program-command nodejs-path)

(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                     (replace-regexp-in-string ".*1G.*3G" "> " output)))))


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


;;
;; Fix macos environment variable handling
;;
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-envs macos-copy-from-env-list)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))


(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(require 'auto-complete)
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-show-menu-immediately-on-auto-complete t)
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start t)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)



(unless (package-installed-p 'spinner)
  (package-install 'spinner))
 (require 'spinner)



;; Markdown / AsciiDoc

;; Markdown
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
(require 'markdown-mode)

;; Temporarily unavailable
;(unless (package-installed-p 'markdown-mode+)
;  (package-install 'markdown-mode+))
;(require 'markdown-mode+)

(unless (package-installed-p 'markdown-preview-mode)
  (package-install 'markdown-preview-mode))
(require 'markdown-preview-mode)

;; Generate a TOC from a markdown file: M-x markdown-toc-generate-toc
;; This will compute the TOC at insert it at current position.
;; Update existing TOC: C-u M-x markdown-toc-generate-toc
(unless (package-installed-p 'markdown-toc)
  (package-install 'markdown-toc))
(require 'markdown-toc)

(autoload 'markdown-preview-mode "markdown-preview-mode"
  "Major mode for editing Markdown files with preview" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-preview-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-preview-mode))

;; AsciiDoc

(unless (package-installed-p 'adoc-mode)
  (package-install 'adoc-mode))
(require 'adoc-mode)

(autoload 'adoc-mode "adoc-mode"
  "Major mode for editing AsciiDoc files" t)
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . adoc-mode))

(require 'asciidoc)

;;
;; Ruby
;;
;(unless (package-installed-p 'enh-ruby-mode)
;  (package-install 'enh-ruby-mode))

;(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
;(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
;(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
;(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
;(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
;(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
;(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

;(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
;; (setq enh-ruby-program "(path-to-ruby1.9)/bin/ruby") ; so that still works if ruby points to ruby1.8
;
;(setq enh-ruby-bounce-deep-indent t)
;(setq enh-ruby-hanging-brace-indent-level 2)
;
;

(require 'cl) ; If you don't have it already

(defun* get-closest-gemfile-root (&optional (file "Gemfile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (loop
     for d = default-directory then (expand-file-name ".." d)
     if (file-exists-p (expand-file-name file d))
     return d
     if (equal d root)
     return nil)))

(require 'compile)

(defun rspec-compile-file ()
  (interactive)
  (compile (format "cd %s;bundle exec rspec %s"
                   (get-closest-gemfile-root)
                   (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                   ) t))

(defun rspec-compile-on-line ()
  (interactive)
  (compile (format "cd %s;bundle exec rspec %s -l %s"
                   (get-closest-gemfile-root)
                   (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                   (line-number-at-pos)
                   ) t))

;; Robe mode makes Emacs into a Ruby IDE
(unless (package-installed-p 'robe)
  (package-install 'robe))

(add-hook 'robe-mode-hook 'ac-robe-setup)

(add-hook 'ruby-mode-hook 'robe-mode)

;; Textmate emulation
(unless (package-installed-p 'textmate)
  (package-install 'textmate))


;; Magit - Git support

(use-package git-gutter
  :diminish git-gutter-mode
  :commands git-gutter-mode)

(use-package magit
  :commands magit-status magit-blame magit-refresh-all
  :init (setq
         magit-revert-buffers nil)
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(setq magit-revert-buffers 0.5)
(setq magit-push-always-verify nil)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)
(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-x G") 'magit-diff-unstaged)
(global-set-key (kbd "C-x C-G") 'magit-diff-unstaged)
(add-hook 'after-save-hook 'magit-refresh-all)

(use-package git-timemachine
  :commands git-timemachine
  :init (setq
         git-timemachine-abbreviation-length 4))

(use-package etags-select
  :commands etags-select-find-tag)



;;
;; Projectile / Helm
;;
;; @see: http://tuhdo.github.io/helm-intro.html

(unless (package-installed-p 'helm)
  (package-install 'helm))


(require 'helm-config)
(require 'helm-buffers)
(require 'helm-locate)
(require 'helm-bookmark)
(require 'helm-files)

(helm-mode 1)

(unless (package-installed-p 'helm-descbinds)
  (package-install 'helm-descbinds))
(require 'helm-descbinds)
(helm-descbinds-mode)
(global-set-key (kbd "C-h h") 'describe-bindings)

(unless (package-installed-p 'project-explorer)
  (package-install 'project-explorer))
(global-set-key "\C-\\" 'project-explorer-toggle)
(global-set-key "\C-\M-\\" 'project-explorer-helm)
(setq pe/omit-gitignore t)
(setq pe/width 65)
(setq
    helm-boring-buffer-regexp-list '("^diary$")
    helm-boring-file-regexp-list
    '("\\.git$" "\\.hg$" "\\.svn$"  "^\\."  "\\.$"
       "\\.\\.$" "\\.Plo$" "\\.lo$"  "_source.*"
       "_8h.*"  "\\.CVS$" "\\._darcs$"  "\\.la$"
       "\\.o$" "~$"  "^#.*")
    helm-ff-skip-boring-files t
    helm-buffer-max-length 80
    helm-idle-delay 2.0
    helm-find-files-show-icons t
    helm-quick-update t
    helm-candidate-number-limit 20
    helm-use-standard-keys nil
    helm-locate-case-fold-search t
    helm-locate-command "locate -e -b %s -r %s")

(global-set-key (kbd "C-x b")
                (lambda () (interactive)
                  (ignore-errors
                    (helm :prompt "Location:"
                          :sources '( helm-source-buffers-list
                                      helm-source-locate
                                      helm-source-bookmarks
                                      helm-source-recentf
                                      helm-source-files-in-current-dir)))))

(require 'cl-lib)

;; Sort Helm's switch buffer list
(add-hook 'ido-make-buffer-list-hook
          (lambda ()
            (setq
             ido-temp-list
             (cl-sort ido-temp-list #'string<
                      :key (lambda (b) (with-current-buffer b
                                         (prin1-to-string major-mode)))))))

(global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-c C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-c h") 'helm-command-prefix) ;; Better Helm activation sequence
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to autocomplete
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-c a")  'helm-select-action)
(define-key helm-find-files-map [(control backspace)] #'helm-find-files-up-one-level)
(define-key helm-read-file-map [(control backspace)] #'helm-find-files-up-one-level)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(unless (package-installed-p 'helm-projectile)
  (package-install 'helm-projectile))

(unless (package-installed-p 'perspective)
  (package-install 'perspective))
(persp-mode)

(require 'helm-projectile)

(projectile-global-mode)

(setq projectile-switch-project-action 'project-explorer-open)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'native)
(setq projectile-use-git-grep t)

(global-set-key (kbd "C-x p p") 'projectile-switch-project)
(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-F") 'projectile-grep)
(global-set-key (kbd "C-c C-f") 'projectile-find-file)
(global-set-key (kbd "C-x M-f") 'projectile-find-file)
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'projectile-switch-to-buffer)


(use-package etags-select
  :commands etags-select-find-tag)


(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol))


(use-package goto-chg
  :commands goto-last-change
  :bind (("C-," . goto-last-change)
         ("C-." . goto-last-change-reverse)))


;; Like Eclipse's quick outline
(use-package popup-imenu
  :commands popup-imenu
  :bind ("C-o" . popup-imenu))


;; Visual undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :init
  (add-hook 'undo-tree-mode-hook (lambda () (unbind-key "C-/" undo-tree-map)))
  (setq undo-tree-visualizer-timestamps t)
  :bind ("C-_" . undo-tree-visualize))


;; Spell checking: from https://raw.githubusercontent.com/kaushalmodi/.emacs.d/master/setup-files/setup-spell.el
(require 'setup-spell)

;; Helm support: from https://raw.githubusercontent.com/pronobis/helm-flyspell/master/helm-flyspell.el
(require 'helm-flyspell)
(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)


(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; dabbrev is too slow, use C-TAB explicitly
  (delete 'company-dabbrev company-backends)
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil))



(use-package hydra
  :commands defhydra
  :bind ("C-M-s" . hydra-splitter/body))

(defun hydra-splitter/body ()
  "Defines a Hydra to resize the windows."
  ;; overwrites the original function and calls it
  ;; https://github.com/abo-abo/hydra/issues/149
  (interactive)
  (require 'hydra-examples)
  (funcall
   (defhydra hydra-splitter nil "splitter"
     ("<left>" hydra-move-splitter-left)
     ("<down>" hydra-move-splitter-down)
     ("<up>" hydra-move-splitter-up)
     ("<right>" hydra-move-splitter-right))))

(defun hydra-smerge/body ()
  "Defines a Hydra to give ediff commands in `smerge-mode'."
  (interactive)
  (funcall
   (defhydra hydra-smerge nil "smerge"
     ("p" smerge-prev)
     ("n" smerge-next)
     ("e" smerge-ediff)
     ("a" smerge-keep-mine)
     ("b" smerge-keep-other))))
(add-hook 'smerge-mode-hook (lambda () (hydra-smerge/body)))


;;
;; Scala/ensime
;;


;; Java / Scala support for templates
(defun mvn-package-for-buffer ()
  "Calculate the expected package name for the buffer;
assuming it is in a maven-style project."
  (let* ((kind (file-name-extension buffer-file-name))
         (root (locate-dominating-file default-directory kind)))
    (when root
      (require 'subr-x) ;; maybe we should just use 's
      (replace-regexp-in-string
       (regexp-quote "/") "."
       (string-remove-suffix "/"
                             (string-remove-prefix
                              (expand-file-name (concat root "/" kind "/"))
                              default-directory))
       nil 'literal))))


(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(defun c-mode-newline-comments ()
  "Newline with indent and preserve multiline comments."
  ;; TODO: annoyingly preserve single line comments, I don't want that
  (interactive)
  (c-indent-new-comment-line)
  (indent-according-to-mode))


(use-package nov
  :init
  (setq show-trailing-whitespace nil)
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))


(use-package play-routes-mode
  :pin melpa
  :init
  (require 'play-routes-mode))


(use-package scala-mode
  :pin melpa
  :interpreter
  ("scala" . scala-mode)
  :init
  (setq
   scala-indent:use-javadoc-style t
   scala-indent:align-parameters t)
  :config

  (bind-key "RET" 'scala-mode-newline-comments scala-mode-map) ; Or: reindent-then-newline-and-indent
  ;; BUG https://github.com/Fuco1/smartparens/issues/468
  ;; backwards/next not working particularly well

  (bind-key [f1] 'ensime-sbt scala-mode-map)
  (bind-key [f3] 'ensime-edit-definition-of-thing-at-point scala-mode-map)
  (bind-key "C-G" 'ensime-show-uses-of-symbol-at-point scala-mode-map)
  (bind-key "M-R" 'ensime-refactor-diff-rename scala-mode-map)
  (bind-key "M-M" 'ensime-refactor-diff-extract-method scala-mode-map)
  (bind-key "M-L" 'ensime-refactor-diff-extract-local scala-mode-map)
  (bind-key "M-I" 'ensime-refactor-diff-inline-local scala-mode-map)
  (bind-key "M-T" 'ensime-refactor-add-type-annotation scala-mode-map)
  (bind-key "C-O" 'ensime-refactor-diff-organize-imports scala-mode-map)
  (bind-key "M-<return>" 'ensime-import-type-at-point scala-mode-map)
  (bind-key "C-M-j" 'join-line scala-mode-map)
  (bind-key "<backtab>" 'scala-indent:indent-with-reluctant-strategy scala-mode-map)
  (bind-key "s-n" 'ensime-search scala-mode-map)
  (bind-key "s-t" 'ensime-print-type-at-point scala-mode-map)
  (bind-key "M-." 'ensime-edit-definition-with-fallback scala-mode-map)

  ;; i.e. bypass company-mode
  (bind-key "C-<tab>" 'dabbrev-expand scala-mode-map)

  (bind-key "C-c c" 'sbt-hydra scala-mode-map)
  (bind-key "C-c e" 'next-error scala-mode-map))

;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


(defun ensime-edit-definition-with-fallback ()
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive)
  (unless (and (ensime-connection-or-nil)
               (ensime-edit-definition))
    (projectile-find-tag)))


(use-package ensime
  :ensure t
  :pin melpa
  :commands ensime ensime-mode
  :init
  (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  (setq
   ensime-default-buffer-prefix "ENSIME-"
   ensime-prefer-noninteractive t
   ensime-refactor-preview t
   ensime-refactor-preview-override-hunk 10
   ensime-startup-snapshot-notification nil
   ensime-startup-notification nil
   ensime-implicit-gutter-icons t
   scala-indent:step 2)
  :config
  (auto-complete-mode) ;; Turn off auto-complete since Ensime does that already
  (subword-mode)

  (require 'ensime-helm)
  (add-hook 'git-timemachine-mode-hook (lambda () (ensime-mode 0)))

  (setq ensime-sbt-command "/usr/local/bin/sbt"
        ensime-search-interface 'helm
        ensime-goto-test-config-defaults
        (plist-merge ensime-goto-test-config-defaults
                     '(:test-class-suffixes ("Spec" "Test" "Check"))
                     '(:test-template-fn ensime-goto-test--test-template-scalatest-flatspec))))


;; This should be done by Ensime, but:
;;    https://github.com/ensime/ensime-server/issues/61
(defcustom ensime-mode-key-prefix [?\C-c ?\C-e]
  "The prefix key for ensime-mode commands."
  :group 'ensime-mode
  :type 'sexp)

(require 'ensime)

(define-key ensime-mode-map (kbd "C-<return>") 'ensime-print-errors-at-point)

;; (require 'ensime-vars)
;; (require 'ensime-company)
;; (require 'ensime-notes)

(setq exec-path (append exec-path (list "~/liftweb" "/usr/bin")))

(setq ensime-sem-high-faces
  '(
   (var . (:foreground "#ff2222"))
   (val . (:foreground "#1111ff"))
   (varField . (:foreground "#ff3333"))
   (valField . (:foreground "#dd11ff"))
   (functionCall . (:foreground "#84BEE3"))
   (param . (:foreground "#111111"))
   (class . font-lock-type-face)
   (trait . (:foreground "#084EA8"))
   (object . (:foreground "#026DF7"))
   (package . font-lock-preprocessor-face)
   ))


(use-package sbt-mode
  :pin melpa
  :interpreter
  ("sbt" . sbt-mode)
  :commands sbt-start sbt-command
  :init (setq sbt:prefer-nested-projects t)
  :config
  ;; WORKAROUND: https://github.com/hvesalai/sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)

  (bind-key "C-c c" 'sbt-command sbt:mode-map)
  (bind-key "C-c e" 'next-error sbt:mode-map))

(defcustom
  scala-mode-prettify-symbols
  '(("->" . ?→)
    ("<-" . ?←)
    ("=>" . ?⇒)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("==" . ?≡)
    ("!=" . ?≠)
    ;; implicit https://github.com/chrissimpkins/Hack/issues/214
    ("+-" . ?±))
  "Prettify symbols for scala-mode.")

(add-hook 'scala-mode-hook
          (lambda ()
            (show-paren-mode t)
            (yas-minor-mode t)
            (git-gutter-mode t)
            (company-mode t)
            (ensime-mode t)
            (setq prettify-symbols-alist scala-mode-prettify-symbols)
            (prettify-symbols-mode t)
            (scala-mode:goto-start-of-code)))

(add-hook 'ensime-mode-hook
          (lambda ()
            (company-mode t)
            (setq ensime-sbt-command "/usr/local/bin/sbt")
            (let ((backends (company-backends-for-buffer)))
              (setq company-backends (push 'ensime-company backends)))))


;;..............................................................................
;; YAML

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;;..............................................................................
;; Java
(use-package cc-mode
  :ensure nil
  :config
  (bind-key "C-c c" 'sbt-command java-mode-map)
  (bind-key "C-c e" 'next-error java-mode-map)
  (bind-key "RET" 'c-mode-newline-comments java-mode-map))

(add-hook 'java-mode-hook
          (lambda ()
            (whitespace-mode-with-local-variables)
            (show-paren-mode t)
            (yas-minor-mode t)
            (git-gutter-mode t)
            (company-mode t)
            (ensime-mode t)))


;;..............................................................................
;; C
(add-hook 'c-mode-hook (lambda ()
                         (yas-minor-mode t)
                         (company-mode t)
                         (smartparens-mode t)))


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


;; Clojure

;;
;; Smart parenthesis matching everywhere, please
;;
;; (unless (package-installed-p 'smartparens)
  ;; (package-install 'smartparens))
;; (require 'smartparens-config)
;; (smartparens-global-mode)
;; (show-smartparens-global-mode t)
;; (sp-with-modes '(rhtml-mode)
               ;; (sp-local-pair "<" ">")
               ;; (sp-local-pair "<%" "%>"))


(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (smartparens-global-mode)
  (setq sp-interactive-dwim t)
  (show-smartparens-global-mode t)
  :config
  (require 'smartparens-config)
  ;; (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "s-[")
  (sp-pair "{" "}" :wrap "C-{"))

(sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

(defun sp-restrict-c (sym)
  "Smartparens restriction on `SYM' for C-derived parenthesis."
  (sp-restrict-to-pairs-interactive "{([" sym))

(bind-key "s-<delete>" (sp-restrict-c 'sp-kill-sexp) scala-mode-map)
(bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) scala-mode-map)
(bind-key "s-<home>" (sp-restrict-c 'sp-beginning-of-sexp) scala-mode-map)
(bind-key "s-<end>" (sp-restrict-c 'sp-end-of-sexp) scala-mode-map)
(bind-key "s-{" 'sp-rewrap-sexp smartparens-mode-map)

(unless (package-installed-p 'paredit)
  (package-install 'paredit))
(require 'paredit)

;; Paredit normally takes over key bindings like ctrl-left/right
;; which in sane editors means to move a word left/right.  Fix that.
(global-set-key (kbd "C-}") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "C-{") 'paredit-forward-barf-sexp)
(global-set-key (kbd "M-q") 'paredit-reindent-defun)


;; clojure-semantic (https://github.com/kototama/clojure-semantic)
;; (Prerequisite for Lispy Clojure support)
(add-to-list 'load-path "~/.emacs.d/clojure-semantic")
(load "clojure.el")

;; Lispy - VI-like keybindings to paredit (https://github.com/abo-abo/lispy)
(unless (package-installed-p 'lispy)
  (package-install 'lispy))


(defun lispy-mode-key-unbindings ()
  "Disable some Lispy-mode keybindings that conflict with Clojure or other packages."
  (if lispy-mode
      (progn
        (if (boundp 'lispy-mode-map)
            (progn
              (define-key lispy-mode-map (kbd "[") nil)
              (define-key lispy-mode-map (kbd "]") nil)
              (define-key lispy-mode-map (kbd "M-<left>") nil)
              (define-key lispy-mode-map (kbd "M-<right>") nil)
              (define-key lispy-mode-map (kbd "s-<return>") nil)
              (define-key lispy-mode-map (kbd "C-<return>") nil)
              (define-key lispy-mode-map (kbd "S-C-<return>") nil)))

        (if (boundp 'lispy-mode-map-x)
            (progn
              (define-key lispy-mode-map-x (kbd "[") nil)
              (define-key lispy-mode-map-x (kbd "]") nil)
              (define-key lispy-mode-map-x (kbd "M-<left>") nil)
              (define-key lispy-mode-map-x (kbd "M-<right>") nil)
              (define-key lispy-mode-map-x (kbd "s-<return>") nil)
              (define-key lispy-mode-map-x (kbd "C-<return>") nil)
              (define-key lispy-mode-map-x (kbd "S-C-<return>") nil)))

        (if (boundp 'lispy-mode-map-c-digits)
            (progn
              (define-key lispy-mode-map-c-digits (kbd "[") nil)
              (define-key lispy-mode-map-c-digits (kbd "]") nil)
              (define-key lispy-mode-map-c-digits (kbd "M-<left>") nil)
              (define-key lispy-mode-map-c-digits (kbd "M-<right>") nil)
              (define-key lispy-mode-map-c-digits (kbd "s-<return>") nil)
              (define-key lispy-mode-map-c-digits (kbd "C-<return>") nil)
              (define-key lispy-mode-map-c-digits (kbd "S-C-<return>") nil)))

        (if (boundp 'lispy-mode-map-lispy)
            (progn
              (define-key lispy-mode-map-lispy (kbd "[") nil)
              (define-key lispy-mode-map-lispy (kbd "]") nil)
              (define-key lispy-mode-map-lispy (kbd "M-<left>") nil)
              (define-key lispy-mode-map-lispy (kbd "M-<right>") nil)
              (define-key lispy-mode-map-lispy (kbd "s-<return>") nil)
              (define-key lispy-mode-map-lispy (kbd "C-<return>") nil)
              (define-key lispy-mode-map-lispy (kbd "S-C-<return>") nil)))

        (if (boundp 'lispy-mode-map-base)
            (progn
              (define-key lispy-mode-map-base (kbd "[") nil)
              (define-key lispy-mode-map-base (kbd "]") nil)
              (define-key lispy-mode-map-base (kbd "M-<left>") nil)
              (define-key lispy-mode-map-base (kbd "M-<right>") nil)
              (define-key lispy-mode-map-base (kbd "s-<return>") nil)
              (define-key lispy-mode-map-base (kbd "C-<return>") nil)
              (define-key lispy-mode-map-base (kbd "S-C-<return>") nil)))

        (if (boundp 'lispy-mode-map-oleh)
            (progn
              (define-key lispy-mode-map-oleh (kbd "[") nil)
              (define-key lispy-mode-map-oleh (kbd "]") nil)
              (define-key lispy-mode-map-oleh (kbd "M-<left>") nil)
              (define-key lispy-mode-map-oleh (kbd "M-<right>") nil)
              (define-key lispy-mode-map-oleh (kbd "s-<return>") nil)
              (define-key lispy-mode-map-oleh (kbd "C-<return>") nil)
              (define-key lispy-mode-map-oleh (kbd "S-C-<return>") nil)))

        (if (boundp 'lispy-mode-map-special)
            (progn
              (define-key lispy-mode-map-special (kbd "[") nil)
              (define-key lispy-mode-map-special (kbd "]") nil)
              (define-key lispy-mode-map-special (kbd "M-<left>") nil)
              (define-key lispy-mode-map-special (kbd "M-<right>") nil)
              (define-key lispy-mode-map-special (kbd "s-<return>") nil)
              (define-key lispy-mode-map-special (kbd "C-<return>") nil)
              (define-key lispy-mode-map-special (kbd "S-C-<return>") nil)))

        (if (boundp 'lispy-mode-map-paredit)
            (progn
              (define-key lispy-mode-map-paredit (kbd "[") nil)
              (define-key lispy-mode-map-paredit (kbd "]") nil)
              (define-key lispy-mode-map-paredit (kbd "M-<left>") nil)
              (define-key lispy-mode-map-paredit (kbd "M-<right>") nil)
              (define-key lispy-mode-map-special (kbd "s-<return>") nil)
              (define-key lispy-mode-map-special (kbd "C-<return>") nil)
              (define-key lispy-mode-map-special (kbd "S-C-<return>") nil))))))


  ;; (defun lispy-mode-key-unbindings ()
  ;;   "Unbind keys from lispy modes."
  ;;   (let ((modes (list 'lispy-mode-map-paredit 'lispy-mode-map-special 'lispy-mode-map-oleh 'lispy-mode-map-base
  ;;                      'lispy-mode-map-lispy 'lispy-mode-map-c-digits 'lispy-mode-map-x 'lispy-mode-map)))
  ;;         (map list
  ;;              (lambda (mode)
  ;;                (define-key mode "[" nil)
  ;;                (define-key mode "]" nil)
  ;;                (define-key mode (kbd "M-<left>") nil)
  ;;                (define-key mode (kbd "M-<right>") nil))
  ;;              modes)))

(defun lispy-mode-on ()
  "Turn lispy mode on."
  (lispy-mode 1))


;; Turn it on by default; toggle via M-x lispy-mode
(add-hook 'clojure-mode-hook 'lispy-mode-on)
(add-hook 'emacs-lisp-mode-hook 'lispy-mode-on)
(add-hook 'lispy-mode-hook 'lispy-mode-key-unbindings)

(unless (package-installed-p 'cider)
  (package-install 'cider))
(require 'cider)
(require 'cider-repl)

(setq cider-lein-command lein-path)
(add-hook 'cider-mode-hook #'eldoc-mode)

(unless (package-installed-p 'ac-cider)
  (package-install 'ac-cider))
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))


(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)


(setq cider-repl-use-clojure-font-lock t)
;; (setq cider-repl-pop-to-buffer-on-connect nil)

;; Abbreviate the REPL prompt if it gets long
(setq cider-repl-prompt-function
      '(lambda (namespace)
        (if (> (length namespace) 20)
            (cider-repl-prompt-abbreviated namespace)
            (cider-repl-prompt-default namespace))))


;; From: https://raw.githubusercontent.com/vspinu/cider/79f828b60963747d87f898487912aa0b5fb802d2/nrepl-client.el
;;  and: https://github.com/clojure-emacs/cider/pull/818
;;  merged with master: https://github.com/clojure-emacs/cider/blob/master/nrepl-client.el
;;
;; Enable the nrepl-server buffer to scroll automatically for log following
(defun nrepl-server-filter (process string)
  "Process server PROCESS output contained in STRING."
  (with-current-buffer (process-buffer process)
    (let ((moving (= (point) (process-mark process))))
      (save-excursion
        (goto-char (process-mark process))
        (insert string)
        (set-marker (process-mark process) (point)))
      (when moving
        (goto-char (process-mark process))
        (-when-let (win (get-buffer-window))
          (set-window-point win (point))))))
  (when (string-match "nREPL server started on port \\([0-9]+\\)" string)
    (let ((port (string-to-number (match-string 1 string))))
      (message (format "nREPL server started on %s" port))
      (with-current-buffer (process-buffer process)
        (let* ((client-proc (nrepl-start-client-process nil port process))
               (client-buffer (process-buffer client-proc)))
          (setq nrepl-client-buffers
                (cons client-buffer
                      (delete client-buffer nrepl-client-buffers)))

          (when (functionp nrepl-post-client-callback)
            (funcall nrepl-post-client-callback client-buffer)))))))


(defun init-ns ()
  "Insert the contents of clojure-repl-init-file into the current repl.
If in a Clojure buffer, change the repl namespace to match the
buffer's."
  (interactive)
  (if (eq major-mode 'clojure-mode)
      (cider-repl-set-ns (cider-current-ns)))
  (cider-switch-to-repl-buffer)
  (goto-char (point-max))
  (insert-file-contents clojure-repl-init-file)
  (goto-char (point-max)))


(defun cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (cider-sexp-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (cider-switch-to-repl-buffer t)
    (goto-char (point-max))
    (insert form)))

(defun cider-eval-defun-at-point-in-repl ()
  (interactive)
  (let ((form (cider-defun-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (setq form (concat "\n" form))
    (cider-switch-to-repl-buffer t)
    (goto-char (point-max))
    (insert form)))

(define-key clojure-mode-map (kbd "s-<return>") 'init-ns)
(define-key clojure-mode-map (kbd "C-s-<return>") 'cider-eval-expression-at-point-in-repl)
(define-key clojure-mode-map (kbd "M-s-<return>") 'cider-eval-defun-at-point-in-repl)


(defun pretty-print-if-possible ()
 (interactive)
 (if (starts-with? (chomp (cider-repl--current-input)) "(def")
     (setq cider-repl-use-pretty-printing nil)
   (setq cider-repl-use-pretty-printing t))
 (cider-repl-return))

(add-hook 'cider-repl-mode-hook '(lambda ()
 (local-set-key (kbd "RET") 'pretty-print-if-possible)))


(defun cider-namespace-refresh ()
  (let* ((filename (file-name-nondirectory (buffer-file-name))))
    (if (not (or (string= filename "profiles.clj")
                 (string= filename "project.clj")
                 (string= filename "build.boot")
                 (ends-with? filename "scratchpad.clj")
                 (ends-with? filename ".edn")))
        (cider-load-buffer))))

(add-hook 'cider-mode-hook
   '(lambda () (add-hook 'after-save-hook
    '(lambda ()
       (if (and (boundp 'cider-mode) cider-mode)
           (cider-namespace-refresh))))))

(setq cider-prompt-for-symbol nil)

(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))
(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.hl\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode)) ; Shebang script support

(defun clojure-pretty-symbols ()
  "Pretty symbols for Clojure code."
  (setq prettify-symbols-alist
        '(("fn" . 955)                  ; lambda
          ("comp" . ?∘)               ; dot
          ("->" . ?→)
          ("<-" . ?←)
          ("=>" . ?⇒)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("=" . ?≡)
          ("unless" . ?≠))))

(add-hook 'clojure-mode-hook
          (lambda ()
            (clojure-pretty-symbols)
            ;; see http://ergoemacs.org/emacs/keyboard_shortcuts_examples.html
            (git-gutter-mode t)
            (local-set-key [f1] 'cider-jack-in)))

(unless (package-installed-p 'clojure-mode-extra-font-locking)
  (package-install 'clojure-mode-extra-font-locking))
(require 'clojure-mode-extra-font-locking)


(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(require 'yasnippet)
(yas/load-directory "~/.snippets")
(yas-global-mode 1)
(add-to-list 'ac-sources 'ac-source-yasnippet)
;; Fix yasnippet / auto-complete incompatibility
(defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
(defalias 'yas/table-hash 'yas--table-hash)



(unless (package-installed-p 'rainbow-mode)
  (package-install 'rainbow-mode))
(require 'rainbow-mode)

;; Dependencies / misc

(unless (package-installed-p 'dash)
  (package-install 'dash))
(require 'dash)

(unless (package-installed-p 'f)
  (package-install 'f))
(require 'f)

(unless (package-installed-p 's)
  (package-install 's))
(require 's)

(unless (package-installed-p 'smart-mode-line)
  (package-install 'smart-mode-line))
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'light)
(sml/setup)

;; Only works well with dark themes
;; Like VIM's modeline hack
;(unless (package-installed-p 'smart-mode-line-powerline-theme)
;  (package-install 'smart-mode-line-powerline-theme))
;(require 'smart-mode-line-powerline-theme)

(unless (package-installed-p 'tree-mode)
  (package-install 'tree-mode))
(require 'tree-mode)

;; We don't want tabbar in aquamacs
(unless (boundp 'aquamacs-version)
  (use-package tabbar
    :commands ensime ensime-mode)

  ;; enable tabbar minor mode
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
    "Return a label for TAB.  That is, a string used to represent it on the tab bar."
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


  (setq tabbar-buffer-groups-function
        (lambda ()
          "Return the name of the tab group names the current buffer belongs to.
There are two groups: Emacs buffers (those whose name starts with '*', plus
dired buffers), and the rest.  This works at least with Emacs v24.2 using
tabbar.el v1.7."
          (list (cond ((starts-with "*sbt*" (buffer-name)) "system")
                      ((starts-with "*terminal" (buffer-name)) "system")
                      ((eq major-mode 'org-mode) "system")
                      ((eq major-mode 'clojure-mode) "clojure")
                      ((eq major-mode 'clojurescript-mode) "clojure")
                      ((starts-with "TAGS" (buffer-name)) "emacs")
                      ((starts-with "*cider-error" (buffer-name)) "emacs")
                      ((starts-with "*cider" (buffer-name)) "user")
                      ((starts-with "*nrepl-server" (buffer-name)) "user")
                      ((string-equal "*eshell*" (buffer-name)) "user")
                      ((starts-with "*term" (buffer-name)) "user")
                      ((string-equal "*scratch*" (buffer-name)) "lisp")
                      ((eq major-mode 'emacs-lisp-mode) "lisp")
                      ((starts-with "*magit" (buffer-name)) "magit")
                      ((starts-with "*helm" (buffer-name)) "helm")
                      ((starts-with "*Helm" (buffer-name)) "helm")
                      ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
                      ((eq major-mode 'dired-mode) "emacs")
                      (t "user"))))))

;;SQL

;; From https://raw.githubusercontent.com/bsvingen/sql-indent/master/sql-indent.el
(eval-after-load "sql"
  '(load-library "sql-indent"))


;; Org mode

(unless (package-installed-p 'org)
  (package-install 'org))
(require 'org)

(setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

(require 'ob-clojure)                   ; Use Clojure/CIDER for executable code in org files
(setq org-babel-default-header-args     ; Use a single session for each org file
           (cons '(:session . "default-clojure")
                 (assq-delete-all :session org-babel-default-header-args)))

;; Redefine execute to always use cider; doesn't seem to work?
(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel."
  (let ((expanded (org-babel-expand-body:clojure body params))
        result)
    (require 'cider)
    (let ((result-params (cdr (assoc :result-params params))))
      (setq result
            (nrepl-dict-get
             (nrepl-sync-request:eval
              expanded (cider-current-connection) (cider-current-session))
             (if (or (member "output" result-params)
                     (member "pp" result-params))
                 "out"
               "value"))))))


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

; Flycheck
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))

(add-hook 'after-init-hook #'global-flycheck-mode)

; Flycheck-tip
;; (unless (package-installed-p 'flycheck-tip)
;;   (package-install 'flycheck-tip))

;; (require 'flycheck-tip)
;; (global-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)

;; (use-package flycheck-cask
;;   :commands flycheck-cask-setup
;;   :config (add-hook 'emacs-lisp-mode-hook (flycheck-cask-setup)))

; Malabar Mode (for Java)
;; (unless (package-installed-p 'malabar-mode)
;;   (package-install 'malabar-mode))

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
;; (defun my-java-malabar-mode-hook ()
;;   ;; IDEA default for jump to source
;;   (define-key c-mode-base-map "\C-\M-g" 'malabar-jump-to-thing)
;;   (global-set-key "\M-n" 'semantic-ia-complete-symbol)
;;   )
;; (add-hook 'c-mode-common-hook 'my-java-malabar-mode-hook)


;; Compiling the file on save makes malabar display the errors in the
;; Java source code.
;; (add-hook 'malabar-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook 'malabar-compile-file-silently
;;                       nil t)))

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
              gnus-summary-mode-hook message-mode-hook scala-mode-hook
              gnus-group-mode-hook eshell-mode-hook w3-mode-hook
              initial-calendar-window-hook cider-repl-mode-hook))

      (mapc (lambda (mode-hook)
              (add-hook mode-hook
                        (lambda ()
                          (setq show-trailing-whitespace t))))
            '(latex-mode-hook LaTeX-mode-hook html-mode-hook)))
  (defalias 'ted-hide-trailing-whitespace 'ignore))


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
         (n (random (length ted-emacs-haiku)))
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
              "\n\n;; Execute the following to upgrade packages:\n(package-utils-upgrade-all)\n\n;; and the following to change fonts/sizes:\n(set-default-font \"Mononoki 12\")
\n\n;;Quick key binding examples:\n(bind-key \"C-c c\" 'sbt-hydra scala-mode-map)\n(define-key ensime-mode-map (kbd \"C-<return>\") 'ensime-print-errors-at-point)\n\n(global-set-key (kbd \"C-/\") 'comment-or-uncomment-region-or-line)\n(global-set-key [A-left] 'windmove-left)\n\n(define-key clojure-mode-map (kbd \"s-<return>\") 'init-ns)\n(define-key clojure-mode-map (kbd \"C-s-<return>\") 'cider-eval-expression-at-point-in-repl)\n(define-key clojure-mode-map (kbd \"M-s-<return>\") 'cider-eval-defun-at-point-in-repl)\n"))))

(setq initial-scratch-message (ted-random-emacs-haiku))
(setq-default word-wrap t)



;;; Misc key (un)bindings


;; The default for ctrl-backspace is to delete words backward into the
;; clipboard, thus destroying whatever was in the clipboard.  This
;; (plus the keybinding below) fixes that.
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     (subword-mode
      (subword-backward-kill 1))
     (t
      (backward-delete-word 1)))))


(defun exit-message ()
  "Define a friendly message to display for the re-bound C-x C-c."
  (interactive)
  (message "Type C-x C-q to exit Emacs.  It's waaaay too easy to accidentally hit C-x C-c")
  (ding))


;; Horizontal scrolling, please
(setq-default truncate-lines t)

;; and no tool bar; I never use it
(tool-bar-mode 0)


(global-set-key [f1] 'multi-term)
(global-set-key [\C-f6] 'other-window) ; Eclipse-like switch to the other buffer
(global-set-key [f6] 'helm-buffers-list)
(global-set-key "\C-c z" 'repeat)
(global-set-key (kbd "M-/") 'hippie-expand)
;;(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

(when (boundp 'aquamacs-version)
  (define-key osx-key-mode-map [home] 'beginning-of-line)
  (define-key osx-key-mode-map [end] 'end-of-line)
  (define-key osx-key-mode-map (kbd "C-<left>") 'backward-word)
  (define-key osx-key-mode-map (kbd "C-<right>") 'forward-word))

(global-set-key "\M-[1;5C"    'forward-word)      ; Ctrl+right   => forward word
(global-set-key "\M-[1;5D"    'backward-word)     ; Ctrl+left    => backward word

(global-set-key (kbd "C-<left>") 'backward-word)
(global-set-key (kbd "C-<right>") 'forward-word)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word) ; NOTE alternative below********
;; (global-set-key (kbd "C-<backspace>") 'contextual-backspace)

(global-set-key (kbd "M-[ h") 'beginning-of-line) ;; Fix for Terminal.app
(global-set-key (kbd "M-[ f") 'end-of-line)       ;; Fix for Terminal.app
(global-set-key (kbd "\C-c g") 'goto-line)
(global-set-key (kbd "\C-F") 'find-file-at-point)
(global-set-key (kbd "\C-c c") 'compile)
(global-set-key (kbd "C-x C-c") 'exit-message) ;; It's waaaay too easy to accidentally Ctrl-x Ctrl-c
(global-set-key (kbd "C-x C-q") 'save-buffers-kill-terminal)

(global-set-key [s-left] 'windmove-left)   ; move to left window
(global-set-key [s-right] 'windmove-right)        ; move to right window
(global-set-key [s-up] 'windmove-up)              ; move to upper window
(global-set-key [s-down] 'windmove-down)          ; move to lower window

(global-set-key [A-left] 'windmove-left)   ; move to left window
(global-set-key [A-right] 'windmove-right)        ; move to right window
(global-set-key [A-up] 'windmove-up)              ; move to upper window
(global-set-key [A-down] 'windmove-down)          ; move to lower window

(find-file (concat (file-name-as-directory "~/") "TIME.org" ))


;;; (provide 'emacs-init)
;;; emacs-init.el ends here
