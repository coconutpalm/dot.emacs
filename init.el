;;; .emacs --- Dave's Emacs configuration -*- emacs-lisp -*-
;;;
;;; Commentary:
;;;
;;;  Execellent examples:
;;;    https://so.nwalsh.com/2020/02/29/dot-emacs
;;;    https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
;;;
;;; Code:

(setq auth-sources '("~/.ssh/.authinfo"))

;;; Tell custom to put its stuff somewhere else, but load it early
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)


;; Make it easy to visit the init.el file
(defun init-visit ()
  "Load the user init.el file."
  (interactive)
  (find-file (concat (expand-file-name "~/.emacs.d") "/init.el" )))


;; Immediately tidy the frame
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))     ;Macs keep the menu bar visible so might as well have it populated
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)            ; Blink forever

(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)
            (flyspell-mode-off)))

;; Configure straight.el and use-package
;;
;; NOTE-When receiving an out-of-date certificate error, uncomment
;;      (setq package-check-signature nil)               and
;;      (package-install 'gnu-elpa-keyring-update)       and reload
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up github:/raxod502/straight.el
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Upgrade packages automatically on startup
;;  If you don't want this, comment out package-utils-upgrade-all
;;   (package-utils-upgrade-all)
;;   (package-install 'gnu-elpa-keyring-update)

(use-package spinner)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color theme
(use-package base16-theme
  :ensure t)

(load-theme 'base16-chalk t)


;; Ag searching
(use-package ag)


;; Better Lisp lists and strings

(use-package dash
  :straight (:type git :host github :repo "magnars/dash.el"))

(use-package s)
(use-package f)
;;
;; Deft notetaking/notes
;;   - by default stored in ~/.deft
;;   https://jblevins.org/projects/deft/
;;
(use-package deft
  :ensure t
  :bind
  ("S-C-d" . deft)
  :config
  (setq deft-file-naming-rules
        '((noslash . "_")
          (nospace . "-")
          (case-fn . downcase))
        deft-recursive t
        deft-auto-save-interval 60
        deft-default-extension "md"
        deft-markdown-mode-title-level 1
        deft-use-filter-string-for-filename t))


;; Distraction-free writing, please
(defun zoom ()
  "Zoom to distraction-free writing mode."
  (interactive)
  (olivetti-mode "toggle"))

(use-package olivetti)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some basic behavior/ keybindings so if we blow up the editor has some baseline things

(use-package subword
  :ensure nil
  :diminish subword-mode
  :config (global-subword-mode t))

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


;; Font sizing / zooming
;; See: https://emacs.stackexchange.com/questions/28390/quickly-adjusting-text-to-dpi-changes
;;
(defun my-dpi (&optional frame)
  "Get the DPI of FRAME (or current if nil)."
  (cl-flet ((pyth (lambda (w h)
                    (sqrt (+ (* w w)
                             (* h h)))))
            (mm2in (lambda (mm)
                     (/ mm 25.4))))
    (let* ((atts (frame-monitor-attributes frame))
           (pix-w (cl-fourth (assoc 'geometry atts)))
           (pix-h (cl-fifth (assoc 'geometry atts)))
           (pix-d (pyth pix-w pix-h))
           (mm-w (cl-second (assoc 'mm-size atts)))
           (mm-h (cl-third (assoc 'mm-size atts)))
           (mm-d (pyth mm-w mm-h)))
      (/ pix-d (mm2in mm-d)))))

(defun normalize-pts (base-pts)
  "Normalize BASE-PTS based on pixels/inch of current display."
  (let ((pt-zoom-factor (/ (my-dpi) 72)))
    (message (format "Font scale factor: %f" pt-zoom-factor))
    (round (* base-pts pt-zoom-factor))))


(defun zoom-by (delta-points)
  "Increase font size by DELTA-POINTS."
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         delta-points))
  (set-face-attribute 'variable-pitch nil
                      :height
                      (+ (face-attribute 'default :height)
                         delta-points))
  (set-face-attribute 'mode-line nil
                      :height
                      (+ (face-attribute 'mode-line :height)
                         delta-points)))

(defun get-buffers-matching-mode (mode)
  "Return a list of buffers where their `major-mode' is equal to MODE."
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun zoom-in ()
  "Increase font size by 10 points."
  (interactive)
  (zoom-by 10)
  (when (get-buffers-matching-mode 'xwidget-webkit-mode)
    (xwidget-webkit-zoom-in)))

(defun zoom-out ()
  "Decrease font size by 10 points."
  (interactive)
  (zoom-by -10)
  (when (get-buffers-matching-mode 'xwidget-webkit-mode)
    (xwidget-webkit-zoom-out)))

;; change font size interactively
(global-set-key (kbd "C-=") #'zoom-in)
(global-set-key (kbd "C--") #'zoom-out)



(use-package nginx-mode)


;; More global keybinding adjustments
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(define-key visual-line-mode-map [end] 'end-of-visual-line)
(define-key visual-line-mode-map [home] 'beginning-of-visual-line)

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

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

(global-set-key (kbd "C-z") 'undo)
;(global-set-key (kbd "C-Z") 'redo)

;;; Load the init file into a buffer so it's easy to get to
;; (find-file (concat (file-name-as-directory "~/.emacs.d") "init.el" ))

;;;
;;; Configuration settings
;;;
(require 'cl-lib)   ;; Common Lisp compatibility layer
(use-package general :ensure t)

(setq debug-on-error t)

(setq exec-path-from-shell-check-startup-files nil)

(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/elisp/confluence-el")
(add-to-list 'load-path "~/.emacs.d/elisp/graphiql.el")
(setq exec-path (append exec-path (list "/usr/bin")))

(setq nodejs-path (executable-find "node"))
(setq lein-path (executable-find "lein"))
(setq boot-path (executable-find "boot"))
(setq clojure-path (executable-find "clj"))
(setq env-path (executable-find "env"))

;; On Macs the TERM envar is stupidly set to "dumb".
(setq TERM (if (eq system-type 'darwin)
               "zsh"
             "bash"))

;; In Clojure, Cmd-enter inserts the contents of this file into the current repl
(setq clojure-repl-init-file "~/.repl.clj")


;;; Misc display settings

;; Set the selection color; but doesn't work well with syntax highlight
;; (set-face-attribute 'region nil :background "#5555ff" :foreground "#ffffff")

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq inhibit-splash-screen t)
(setq select-enable-clipboard t)      ; enable use of system clipboard across emacs and applications
(setq-default fill-column 120)
(setq-default standard-indent 3) ; set standard indent to 3 rather that 4
(setq-default tab-width 3)
;;(pixel-scroll-mode t)                    ;Emacs 26 or later, but it can be slow so commented

(setq scroll-step 1)             ; control screen "leaping"
(setq-default indent-tabs-mode nil) ; spaces instead of tabs by default

(global-hl-line-mode 1) ; highlight current line, turn it on for all modes by default
(set-face-background 'hl-line "black")

(setq mouse-autoselect-window t)        ; Focus follows mouse inside Emacs please

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


(defun starts-with (begins s)
  "Return non-nil if string S begins with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))


(use-package epl)


;; Productivity tools: =============================================================

;;
;; Pomodoro timers
;;
(use-package pomidor
  :ensure t
  :bind (("<f12>" . pomidor))
  :config
  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil
        pomidor-play-sound-file (lambda (file)
                                  (if (eq system-type 'darwin)
                                      (start-process "emacs-pomidor-sound"
                                                     nil
                                                     "afplay"
                                                     file)
                                    (start-process "emacs-pomidor-sound"
                                                     nil
                                                     "mplayer"
                                                     file))))
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1) ; Emacs 26.1+
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          ;; force fringe update
                          (set-window-buffer nil (current-buffer)))))
(require 'pomidor)

;; Comp(lete)any mode

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (global-company-mode)
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  (setq
   company-dabbrev-ignore-case t
   company-dabbrev-code-ignore-case t
   company-dabbrev-downcase nil
   company-idle-delay 0.0
   company-minimum-prefix-length 2)
  (setq-local
   completion-ignore-case t)
  :config
  ;; dabbrev is too slow
  (add-to-list 'debug-ignored-errors "^Cannot complete at point$")
  (delete 'company-dabbrev company-backends))

(use-package company-quickhelp
  :init
  (company-quickhelp-mode))

;; Icons in content help menus
(use-package company-box
  :hook (company-mode . company-box-mode))

;; company-flyspell lives in site elisp directory
(require 'company-flyspell)

;; Company-flyspell should have lowest priority
(setq company-backends (append company-backends (list 'company-flyspell)))


;; Resolve conflicts with indenting, completion, and yasnippets

(defun check-expansion ()
  "Return 't if the prior characters should be expanded and nil otherwise."
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  "Expand snippet or return null."
  (let ((yas/fallback-behavior 'return-nil))
    (when (fboundp 'yas-expand)           ; Make sure yas-minor-mode is enabled
      (yas-expand))))

(defun company-tab-indent-or-complete ()
  "If focus is in the minibuffer then autocomplete else try to expand/tab using (or yasnippet company indent)."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (bound-and-true-p yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))


;; Company colors
(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))



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


(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

  :config
  (when window-system
    (progn
      (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
            treemacs-deferred-git-apply-delay      0.5
            treemacs-display-in-side-window        t
            treemacs-eldoc-display                 t
            treemacs-file-event-delay              5000
            treemacs-file-follow-delay             0.2
            treemacs-follow-after-init             t
            treemacs-git-command-pipe              ""
            treemacs-goto-tag-strategy             'refetch-index
            treemacs-indentation                   2
            treemacs-indentation-string            " "
            treemacs-is-never-other-window         nil
            treemacs-max-git-entries               5000
            treemacs-missing-project-action        'ask
            treemacs-no-png-images                 nil
            treemacs-no-delete-other-windows       t
            treemacs-project-follow-cleanup        nil
            treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
            treemacs-position                      'left
            treemacs-recenter-distance             0.1
            treemacs-recenter-after-file-follow    nil
            treemacs-recenter-after-tag-follow     nil
            treemacs-recenter-after-project-jump   'always
            treemacs-recenter-after-project-expand 'on-distance
            treemacs-show-cursor                   nil
            treemacs-show-hidden-files             t
            treemacs-silent-filewatch              nil
            treemacs-silent-refresh                nil
            treemacs-sorting                       'alphabetic-desc
            treemacs-space-between-root-nodes      t
            treemacs-tag-follow-cleanup            t
            treemacs-tag-follow-delay              1.5
            treemacs-width                         40)))

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)


  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  :hook
  (treemacs-mode . (lambda ()
                     (variable-pitch-mode 1)))

  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-\\"      . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


;; (use-package treemacs-evil
;;   :after treemacs evil
;;   :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))


(use-package treemacs-magit
  :after treemacs magit
  :ensure t)


(use-package which-key
  :config
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-mode t))


;; Multiple cursors
(use-package multiple-cursors)

(global-set-key (kbd "C-S-l C-S-l") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-s->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-s-<") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Keep a list of recent files
(use-package recentf
  :init
  (setq recentf-max-menu-items 100)
  :config
  (recentf-mode 1))

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


(require 'dired)
(setq dired-dwim-target t)           ; If two dired windows are open, "copy" and "move" target the other window's path
;; a workflow optimisation too far?
(bind-key "C-c c" 'sbt-hydra dired-mode-map)
(bind-key "C-c e" 'next-error dired-mode-map)


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

;; Automatically clean buffer list every hour
;; (require 'midnight)
;; (run-at-time "12am" (* 60 60) #'clean-buffer-list) ; the REPEAT parameter is in seconds

;; (setq clean-buffer-list-delay-general 0)
;; (setq clean-buffer-list-delay-special (* 8 60 60)) ; last used more than 8h ago
;; (setq clean-buffer-list-never-regexps
      ;; '("company"
        ;; "magit"))
;; (setq clean-buffer-list-kill-regexps
      ;; '("[*]magit.*"))

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
  (cl-flet* ((buffer-finder (regexp) (string-match regexp name))
             (buffer-find (regexps) (-partial #'-find #'buffer-finder)))
    (and (buffer-find clean-buffer-list-kill-regexps)
         (not (or (buffer-find clean-buffer-list-kill-never-regexps)
                  (buffer-find ido-buffer-whitelist))))))


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



;;
;; Fix macos environment variable handling
;;
(setq macos-copy-from-env-list
      '("AWS_ACCESS_KEY_ID"
        "AWS_SECRET_ACCESS_KEY"
        "SERVICE_LOCATOR_CONVENTION_SCHEME"
        "SERVICE_LOCATOR_CONVENTION_DNS_PATTERN"
        "TERM"
        "CLICOLOR"
        "PATH"
        "JAVA_HOME"
        "NVM_BIN"
        "NVM_CD_FLAGS"
        "NVM_DIR"
        "PERL5LIB"                      ; Needed for edbi
        "PERL_LOCAL_LIB_ROOT"
        "PERL_MB_OPT"
        "PERL_MM_OPT"))

(use-package exec-path-from-shell :ensure t)
(when (or (daemonp) (memq window-system '(mac ns)))
    (exec-path-from-shell-copy-envs macos-copy-from-env-list)
    (exec-path-from-shell-initialize))

;; Horizontal scrolling, please
(setq-default truncate-lines t)


;; No trailing whitespace, please...
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Word-wrap text buffers
(add-hook 'text-mode-hook
          (lambda () (visual-line-mode 1)))

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


;; Content-assist package
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-color-mode-line)
(use-package flycheck-pos-tip)
(use-package flycheck-status-emoji)

(use-package flycheck-cask              ;ELisp support
  :commands flycheck-cask-setup
  :config (add-hook 'emacs-lisp-mode-hook (flycheck-cask-setup)))


;; ansi-term
(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-r" "C-s" "C-y" "<ESC>" "<TAB>" "\t" "C-[")
  "The key list that will need to be unbind."
  :type 'list
  :group 'term)

(defcustom term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-f" . isearch-forward)
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
  :group 'term)

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


; interpret and use ansi color codes in shell output windows
(require 'ansi-color)

(ansi-color-for-comint-mode-on)
(defun colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(eval-after-load "ansi-term"
  '(define-key ansi-term-raw-map (kbd "C-c C-y") 'term-paste))


(defvar ansi-term-history nil)

(defun terminal-run (command &optional name)
  "Run COMMAND in a `term' buffer, optionally named NAME."
  (interactive
   (list (read-from-minibuffer "$ " nil nil nil 'ansi-term-history)))
  (let* ((name (or name command))
         (switches (split-string-and-unquote command))
         (command (pop switches))
         (termbuf (apply 'make-term name command nil switches)))
    (set-buffer termbuf)
    (term-mode)
    (term-char-mode)
    (switch-to-buffer termbuf)))

(defun ansi-term-with-config ()
  (terminal-run (concat env-path " " TERM " -l") "ansi-term"))


;; Native terminals WIP
;;
;;  Installation prereqs: cmake, libtool
;;
;;  Still need to bind arrow keys, backspace, delete
;;
(use-package eterm-256color :ensure t)
(use-package vterm
  :ensure t

  :config
  (setq vterm-term-environment-variable "xterm-256color")
  (setq vterm-buffer-name-string "* vterm %s *")
  (setq vterm-max-scrollback 10000)

  :hook
  (vterm-mode
   .
   (lambda ()
     ;; Close buffer and window when shell exits
     (let* ((buff (current-buffer))
            (proc (get-buffer-process buff)))
       (set-process-sentinel
        proc
        `(lambda (process event)
           (if (or (string= event "finished\n")
                   (starts-with? event "exited"))
               (progn
                 (kill-buffer ,buff)
                 (delete-window)
                 (previous-window))
             (message event))))))))


;; handy code recipe
;; (term-send-string (get-buffer-process "*ansi-term*") "source /etc/profile\n")

(defun terminal ()
  "Switch to terminal.  Launch if nonexistent."
  (interactive)
  (split-window)
  (other-window 1 nil)

  (if (get-buffer "*vterm*")
      (switch-to-buffer "*vterm*")
    (vterm))

  (get-buffer-process "*vterm*"))

(global-set-key "\C-t" 'terminal)


;; Kill term buffers when their process dies
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            ((lambda ()                      ; Oddly: do isn't working here
               (kill-buffer ,buff)
               (delete-window)
               (previous-window))))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)


;; Integrate dired with ansi-term
(require 'dired-x)
(global-set-key (kbd"\C-c \C-t") 'dired-jump)
(define-key dired-mode-map (kbd "`") 'dired-open-term)
(define-key dired-mode-map (kbd "\C-t") 'dired-open-term)

(defun dired-open-term ()
  "Open an `ansi-term' that corresponds to current directory."
  (interactive)
  (let ((current-dir (dired-current-directory)))
    (term-send-string
     (terminal)
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))


; make completion buffers disappear after 5 seconds.
(add-hook 'completion-setup-hook
  (lambda () (run-at-time 5 nil
    (lambda () (delete-windows-on "*Completions*")))))


;; Slack
;;
;; From github:/yuya373/emacs-slack
(use-package alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))
(use-package emojify)
(use-package oauth2)
(use-package request)
(use-package websocket)

(use-package slack
  :commands (slack-start)
  :config
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)

  (slack-register-team
   :name "Rally Health"
   :default t
   :token (auth-source-pick-first-password
           :host "rallyhealth.slack.com"
           :user "david.orme@rallyhealth.com")
   :full-and-display-names t
   :subscribed-channels
   '((care-echo
      care-ui
      connect-fpc
      domain-unification
      linux-laptops
      scala-discussions
      openapi-codegen
      backend-bootcamp
      backend-geeks
      clojure)))

  (slack-register-team
   :name "clojurians"
   :token (auth-source-pick-first-password
           :host "clojurians.slack.com"
           :user "david@coconut-palm-software.com")
   :full-and-display-names t
   :subscribed-channels
   '((announcements
      beginners
      clojure
      clojurescript
      events
      liquid
      news-and-articles
      remote-jobs)))

  ;; (slack-register-team
  ;; :name "emacs-slack"
  ;; :default t
  ;; :token "xoxs-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
  ;; :subscribed-channels '(test-rename rrrrr)
  ;; :full-and-display-names t)

  ;; (slack-register-team
  ;; :name "test"
  ;; :token "xoxs-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
  ;; :subscribed-channels '(hoge fuga))
  :hook
  (slack-mode
   .
   (lambda ()
     (setq show-trailing-whitespace nil)
     (olivetti-mode 1)
     (variable-pitch-mode 1))))

(use-package helm-slack
  :straight (:type git :host github :repo "yuya373/helm-slack")
  :after (helm slack))


;; Source code comment line(s)
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))


(use-package groovy-mode
  :mode
  (("\\.groovy$" . groovy-mode)
   ("\\.gradle$" . groovy-mode))

  :config
  (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

  :hook
  (groovy-mode
   . (lambda ()
       (require 'groovy-electric)
       (groovy-electric-mode))))

(use-package jenkinsfile-mode
  :mode
  ("Jenkinsfile.*" . jenkinsfile-mode))

;;
;; Hyperbole - make everything hypertext  C-h h
;;
(use-package hyperbole)

;;
;; JSON and API explorer support
;;
(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\manifest.webapp\\'" . json-mode )
         ("\\.tern-project\\'" . json-mode)))

;; Import Postman files into restclient or verb
(require 'impostman)

;; For quick & dirty HTTP requests
(use-package httprepl)

;; For HTTP protocol syntax
(use-package restclient
  :ensure t
  :defer t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
              ("C-c C-i" . impostman-import-string)
	           ("C-c C-f" . json-mode-beautify)
              ("C-c C-r" . httprepl)))

;; Extend Org mode into a literate form expressing
;; HTTP requests.  Supports cURL import/export.
;; C-c C-' displays Walkman's menu inside an Org buffer
;;
;; https://github.com/abrochard/walkman/blob/master/sample.org
(use-package walkman
  :ensure t
  :defer t
  :config
  (setq walkman-keep-headers t)
  :bind (:map org-mode-map
              ("C-c C-c" . walkman-at-point)))
(walkman-setup)

;; Extend Org mode to DRY up REST requests
;; An alternative to "walkman"
;;
;; Keymap is added below in org-mode package
(use-package verb :ensure t)


;; Org mode
(use-package org
  :ensure t

  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (setq org-src-fontify-natively t)
  (setq org-duration-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

  :hook
  (org-mode
   . (lambda ()
       (org-indent-mode t)
       (turn-on-font-lock)
       (visual-line-mode)
       (setq truncate-lines nil))))


;; GraphQL
(use-package request)
(require 'graphiql)             ; Not sure which I prefer
(setq graphiql-use-lsp t)
;; (use-package graphql-mode)


;;
;; (Type|Java)Script
;;
;; More ideas:
;;   https://github.com/felipeochoa/rjsx-mode
;;   https://dev.to/viglioni/how-i-set-up-my-emacs-for-typescript-3eeh
;;   https://news.ycombinator.com/item?id=24119611  (rjsx configs; react stuff generally)

(use-package typescript-mode
  :hook
  (typescript-mode
   . (lambda ()
       (add-to-list
        (make-local-variable
         'grep-find-ignored-directories) "build")
       (electric-indent-mode -1)))

  (typescript-mode . setup-tide-mode)

  :config
  (setq typescript-indent-level 4)

  :mode (rx ".ts" (? "x") string-end))


(use-package js2-mode
  :config
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'setup-tide-mode)

  :custom
  (js2-include-node-externs t)
  (js2-global-externs '("customElements"))
  (js2-highlight-level 3)
  (js2r-prefer-let-over-var t)
  (js2r-prefered-quote-type 2)
  (js-indent-align-list-continuation t))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


(use-package xref-js2
  :bind
  ("C-S-g" . xref-find-references)

  :hook
  (js2-mode
   . (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))


;; Use my fork of npm-mode that fixes jump to error
(require 'npm+-mode)


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (tide-hl-identifier-mode t)
  (company-mode +1))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :config
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

  :hook ((typescript-mode . setup-tide-mode)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))


;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)


;; Web-mode
(use-package web-mode
  :ensure t
  :after (tide)
  :config
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.rtml?\\'" . web-mode))

  :hook ((web-mode-hook
          .
          (lambda ()
            (when (or (string-equal "ejs" (file-name-extension buffer-file-name))
                      (string-equal "tsx" (file-name-extension buffer-file-name))
                      (string-equal "jsx" (file-name-extension buffer-file-name)))
              (setq web-mode-code-indent-offset 4)
              (setup-tide-mode))))))

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)


(use-package prettier-js
  :ensure t
  :hook
  (web-mode-hook . prettier-js-mode)
  (js2-mode-hook . prettier-js-mode))


;; Javascript REPL
(use-package js-comint
  :config
  (setq inferior-js-program-command nodejs-path)

  :hook
  (inferior-js-mode-hook . (lambda ()
                             (ansi-color-for-comint-mode-on)
                             ;; Deal with some prompt nonsense
                             (add-to-list 'comint-preoutput-filter-functions
                                          (lambda (output)
                                            (replace-regexp-in-string ".*1G.*3G" "> " output))))))

;; Javascript testing/debugging
(use-package jest-test-mode :ensure t :defer t :commands jest-test-mode
  :init
  (add-hook 'typescript-mode-hook 'jest-test-mode)
  (add-hook 'js2-mode-hook 'jest-test-mode)
  (add-hook 'typescript-tsx-mode-hook 'jest-test-mode))

(use-package upbo
  :ensure t
  :init
  (add-hook 'typescript-mode-hook 'upbo-mode)
  (add-hook 'js2-mode-hook 'upbo-mode)
  (add-hook 'typescript-tsx-mode-hook 'upbo-mode))



;; Configure embedded Webkit browser
(require 'xwidget)

(use-package xwwp
  :custom
  (xwwp-follow-link-completion-system 'helm)
  :bind (:map xwidget-webkit-mode-map
              ("l" . xwwp-follow-link))
  :after (helm))

(use-package xwwp-follow-link-helm
  :after (helm))

(setq browse-url-browser-function 'xwidget-webkit-browse-url)

(defun web-browse-or-search (browse-target)
  "Browse to a web link or search terms in BROWSE-TARGET."
  (interactive "sURL or search terms: ")
  (xwwp browse-target browse-target))

(defun web-bookmark-page ()
  "Bookmark current page's URL."
  (interactive)
  (xwidget-webkit-bookmark-make-record))

(defun web-storyboard ()
  "Open my current project's scrum board."
  (interactive)
  (browse-url "https://jira.rallyhealth.com/secure/RapidBoard.jspa?rapidView=844"))

(defun web-chat ()
  "Open my current project's chat app."
  (interactive)
  (browse-url "https://rallyhealth.slack.com"))

(defun web-continuous-integration ()
  "Open my current project's CI server."
  (interactive)
  (browse-url "https://ci.rally-dev.com/teams-connect/job/connect/"))

(global-set-key (kbd "C-c b g") 'helm-google-suggest)
(global-set-key (kbd "C-c b s") 'web-browse-or-search)
(global-set-key (kbd "C-c b b") 'web-bookmark-page)

;; Accelerators to quickly open important web applications
(global-set-key (kbd "C-c b S") 'web-storyboard)
(global-set-key (kbd "C-c b C") 'web-chat)
(global-set-key (kbd "C-c b I") 'web-continuous-integration)

(define-key xwidget-webkit-mode-map (kbd "C-w") 'xwidget-webkit-copy-selection-as-kill)
(define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
(define-key xwidget-webkit-mode-map [remap beginning-of-buffer] 'xwidget-webkit-scroll-top)
(define-key xwidget-webkit-mode-map [remap end-of-buffer] 'xwidget-webkit-scroll-bottom)
(define-key xwidget-webkit-mode-map (kbd "C-=") nil) ; Use global bindings instead
(define-key xwidget-webkit-mode-map (kbd "C-+") nil)
(define-key xwidget-webkit-mode-map (kbd "C--") nil)
;; (define-key xwidget-webkit-mode-map [home] 'xwidget-webkit-scroll-top)
;; (define-key xwidget-webkit-mode-map [end] 'xwidget-webkit-scroll-bottom)


(defun make-unique-title (title)
  "Return a unique TITLE name."
  (if (get-buffer title)
      (make-unique-title (concat title "-"))
    title))

(defun xwidget-webkit-new-doctitle (title)
  "Called when a web browser has a new document TITLE."
  (unless (string-equal title "")
    (rename-buffer (make-unique-title title))
    (centaur-tabs-get-groups)))

;; HACK! Forked from xwidget.el: better buffer names!
(defun xwidget-webkit-callback (xwidget xwidget-event-type)
  "Callback for xwidgets.
XWIDGET instance, XWIDGET-EVENT-TYPE depends on the originating xwidget."
  (if (not (buffer-live-p (xwidget-buffer xwidget)))
      (xwidget-log
       "error: callback called for xwidget with dead buffer")
    (with-current-buffer (xwidget-buffer xwidget)
      (cond ((eq xwidget-event-type 'load-changed)
             (xwidget-webkit-execute-script
              xwidget "document.title"
              #'xwidget-webkit-new-doctitle)
             (pop-to-buffer (current-buffer)))
            ((eq xwidget-event-type 'decide-policy)
             (let ((strarg  (nth 3 last-input-event)))
               (if (string-match ".*#\\(.*\\)" strarg)
                   (xwidget-webkit-show-id-or-named-element
                    xwidget
                    (match-string 1 strarg)))))
            ((eq xwidget-event-type 'javascript-callback)
             (let ((proc (nth 3 last-input-event))
                   (arg  (nth 4 last-input-event)))
               (funcall proc arg)))
            (t (xwidget-log "unhandled event:%s" xwidget-event-type))))))

(defun xwidget-kill-buffer-query-function ()
  "Don't ask before killing a buffer that has xwidgets."
  1)



;; SASS css support
(use-package sass-mode)


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
     (define-key (current-local-map) (kbd ";") 'my-semicolon)
     (define-key (current-local-map) (kbd ":") 'my-colon)
     (define-key (current-local-map) (kbd "}") 'my-closebrace)
     (define-key (current-local-map) (kbd "{") 'my-brace) ))

;; Markdown / AsciiDoc

;; Impatient-mode
(use-package simple-httpd)
(use-package impatient-mode)


(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><script src=\"https://cdnjs.cloudflare.com/ajax/libs/he/1.1.1/he.js\"></script><link rel=\"stylesheet\" href=\"https://assets-cdn.github.com/assets/github-e6bb18b320358b77abe040d2eb46b547.css\"><link rel=\"stylesheet\" href=\"https://assets-cdn.github.com/assets/frameworks-95aff0b550d3fe338b645a4deebdcb1b.css\"><title>Impatient Markdown</title><div id=\"markdown-content\" style=\"display:none\">%s</div><div class=\"markdown-body\" style=\"max-width:968px;margin:200 auto;\"></div><script>fetch('https://api.github.com/markdown', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ \"text\": document.getElementById('markdown-content').innerHTML, \"mode\": \"gfm\", \"context\": \"knit-pk/homepage-nuxtjs\"}) }).then(response => response.text()).then(response => {document.querySelector('.markdown-body').innerHTML = he.decode(response)}).then(() => { fetch(\"https://gist.githubusercontent.com/FieryCod/b6938b29531b6ec72de25c76fa978b2c/raw/\").then(response => response.text()).then(eval)});</script></html>"
                   (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))


(defun markdown-preview-browse! ()
  "Open an impatient-mode web browser on the `markdown-preview!' port."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (browse-url "http://localhost:8901/imp/")
  (other-window -1))

(defun markdown-preview! ()
  "Open a live markdown preview for the current buffer."
  (interactive)

  (httpd-stop)
  (setq httpd-port 8901)
  (httpd-start)

  (impatient-mode 1)
  (setq imp-user-filter #'markdown-html)
  (cl-incf imp-last-state)
  (imp--notify-clients)
  (markdown-preview-browse!))


(use-package markdown-mode
  :ensure t

  :commands (markdown-mode gfm-mode)

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("\\.txt$" . gfm-mode))

  :bind (("<tab>" . markdown-cycle))

  :custom
  (markdown-asymmetric-header t)
  (markdown-split-window-direction 'right)

  :config
  (setq markdown-command "pandoc -c file:///home/djo/.emacs.d/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-wiki-links t)
  (setq markdown-wiki-link-search-subdirectories t)
  ;;  (setq markdown-header-scaling t)      ;TODO: Investigate if I should use this over custom.el
  )

(defun markdown-flyspell-check-word-p ()
  "Return t if `flyspell' should check word just before point."
      (save-excursion
        (goto-char (1- (point)))
        (not (or (markdown-code-block-at-point-p)
                 (markdown-inline-code-at-point-p)
                 (markdown-in-comment-p)
                 (let ((faces (get-text-property (point) 'face)))
                   (if (listp faces)
                       (or (memq 'markdown-reference-face faces)
                           (memq 'markdown-markup-face faces)
                           (memq 'markdown-url-face faces))
                     (memq faces '(markdown-reference-face
                                   markdown-markup-face
                                   markdown-url-face))))))))

(defun markdown-pretty-symbols ()
  "Pretty symbols for Markdown."
  (setq prettify-symbols-alist
        '(("* " . ?•)
          (" * " . ?•)
          ("->" . ?→)
          ("<-" . ?←)
          ("=>" . ?⇒)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("==" . ?≡)
          ("!=" . ?≠))))

(add-hook 'markdown-mode-hook
          (lambda ()
            (variable-pitch-mode 1)
            (olivetti-mode 1)
            (flyspell-mode 1)
            (markdown-pretty-symbols)
            (setq flyspell-generic-check-word-predicate 'markdown-flyspell-check-word-p)))


(require 'flyspell)


;; Hanging indents for bullets in visual-line-mode paragraphs.  Not perfect with variable pitch.
;;
;;  See: https://github.com/dirkholz/adaptive-wrap/blob/elpa/adaptive-wrap.el
;;       https://github.com/brentonk/adaptive-wrap-vp/
;;
;;  The second package tries to handle variable pitch but shows ugly visible ^J characters
;;  when wrapping more than 2 lines of text.
;;
;;  It also would be nice to adaptively wrap Markdown header lines, but I don't know how
;;  to do that yet.
(use-package adaptive-wrap
  :hook
  (visual-line-mode . adaptive-wrap-prefix-mode))

;; Generate a TOC from a markdown file: M-x markdown-toc-generate-toc
;; This will compute the TOC at insert it at current position.
;; Update existing TOC: C-u M-x markdown-toc-generate-toc
(use-package markdown-toc)

;; See functions above for MD preview; may bring this back someday?

;; (autoload 'markdown-preview-mode "markdown-preview-mode"
  ;; "Major mode for editing Markdown files with preview" t)
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-preview-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-preview-mode))

(use-package ascii-art-to-unicode :ensure t)

;; AsciiDoc

(use-package adoc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
  (add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode))
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . adoc-mode)))


;; Database support
(use-package edbi)
(global-set-key (kbd "C-c C-d") 'edbi:open-db-viewer)


;; Magit - Git support

(use-package git-gutter
  :diminish git-gutter-mode
  :commands git-gutter-mode)

(use-package magit
  :commands magit-status magit-blame magit-refresh-all
  :config
  (defun find-jira-ticket-id-in-branchname ()
    "Return any `JIRA-##' string from the branch name or the empty string."
    (let ((ISSUEKEY "[[:upper:]]+-[[:digit:]]+")
          (current-branch (magit-get-current-branch)))
      (if (and current-branch
               (string-match-p ISSUEKEY current-branch))
          (replace-regexp-in-string
           (concat ".*?\\(" ISSUEKEY "\\).*")
           "\\1: "
           (magit-get-current-branch))
        "")))

  (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)
  (define-key magit-mode-map (kbd "TAB") 'magit-section-toggle)
  (define-key magit-mode-map (kbd "\t") 'magit-section-toggle)
  (add-hook 'git-commit-setup-hook
            (lambda () (insert (find-jira-ticket-id-in-branchname))))

  :bind (("M-l" . magit-log-current) ;; See git-timemachine bindings below
         ("\t" . magit-section-toggle)
         ("<tab>" . magit-section-toggle)
         ("TAB" . magit-section-toggle)
         ("s-g" . magit-status)
         ("s-b" . magit-blame)))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)
(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-x G") 'magit-diff-unstaged)
(global-set-key (kbd "C-x C-S-g") 'magit-diff-unstaged)
(add-hook 'after-save-hook 'magit-refresh-all)


;; Use Git to provide local history
;(require 'local-history)
;(global-set-key [C-H] 'local-history-check-changes)


(use-package git-timemachine
  :commands git-timemachine
  :bind ("C-M-l" . git-timemachine) ;; See magit bindings above
  :init (setq
         git-timemachine-abbreviation-length 4))


(use-package forge
  :after magit)
(require 'forge)


(use-package github-review
  :ensure t
  :after forge
  :config
  (define-key magit-status-mode-map (kbd "g") 'github-review-forge-pr-at-point)
  (define-key magit-mode-map (kbd "g") 'github-review-forge-pr-at-point))
(require 'github-review)


(use-package ediff
  :init
  (setq
   ;; Always split nicely for wide screens
   ediff-split-window-function 'split-window-horizontally)
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents
       ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents
       ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map ()
    (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))


(defhydra hydra-smerge
  (:color red :hint nil
          :pre (smerge-mode 1))
  "
^Move^ ^Keep^ ^Diff^ ^Pair^
------------------------------------------------------
_n_ext _b_ase _R_efine _<_: base-mine
_p_rev _m_ine _E_diff _=_: mine-other
^ ^ _o_ther _C_ombine _>_: base-other
^ ^ _a_ll _r_esolve
_q_uit _RET_: current
"
  ("RET" smerge-keep-current)
  ("C" smerge-combine-with-next)
  ("E" smerge-ediff)
  ("R" smerge-refine)
  ("a" smerge-keep-all)
  ("b" smerge-keep-base)
  ("m" smerge-keep-mine)
  ("n" smerge-next)
  ("o" smerge-keep-other)
  ("p" smerge-prev)
  ("r" smerge-resolve)
  ("<" smerge-diff-base-mine)
  ("=" smerge-diff-mine-other)
  (">" smerge-diff-base-other)
  ("q" nil :color blue))
;;

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
;; (use-package undo-tree
;;   ;; :diminish undo-tree-mode
;;   :config
;;   (global-undo-tree-mode 1)
;;   (defalias 'redo 'undo-tree-redo)
;;   (setq undo-tree-visualizer-timestamps t)
;;   :bind
;;   ("C-_" . undo)
;;   ("C-z" . undo)
;;   ("M-_" . redo)
;;   ("C-S-z" . redo))


;; Spell checking: from https://raw.githubusercontent.com/kaushalmodi/.emacs.d/master/setup-files/setup-spell.el
;; (require 'setup-spell)

;;
;; Docker
;;

(use-package docker)
(use-package docker-api)
(use-package docker-compose-mode)
(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))


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


(defun c-mode-newline-comments ()
  "Newline with indent and preserve multiline comments."
  ;; TODO: annoyingly preserve single line comments, I don't want that
  (interactive)
  (c-indent-new-comment-line)
  (indent-according-to-mode))


(use-package play-routes-mode
  :init
  (require 'play-routes-mode))


(use-package scala-mode
  :interpreter
  ("drydoc" . scala-mode) ;; Since the 'scala' command is deprecated

  :mode "\\.s\\(cala\\|bt\\)$"

  :init
  (setq
   ;; scala-indent:use-javadoc-style t
   scala-indent:align-parameters t)
  (subword-mode t)

  :bind
  (("RET"     . reindent-then-newline-and-indent)
   ("C-<tab>" . dabbrev-expand)              ; ???? Do I still want this?
   ("C-c c"   . 'sbt-hydra)
   ("C-c e"   . 'next-error)

   ("s-<delete>"    . 'sp-kill-sexp)
   ("s-<backspace>" . 'sp-backward-kill-sexp)
   ("s-<home>"      . 'sp-beginning-of-sexp)
   ("s-<end>"       . 'sp-end-of-sexp))

  ;; Old stuff....

  ;; BUG https://github.com/Fuco1/smartparens/issues/468
  ;; backwards/next not working particularly well

  ;; (bind-key "C-c C-e e" 'ensime scala-mode-map)
  ;; (bind-key "C-c C-e d" 'ensime-db-attach scala-mode-map)
  ;; (bind-key "C-c C-e b" 'ensime-db-set-break scala-mode-map)
  ;; (bind-key "C-c C-e B" 'ensime-db-clear-break scala-mode-map)
  ;; (bind-key "C-c C-e i" 'ensime-db-inspect-value-at-point scala-mode-map)
  ;; (bind-key "C-c C-e t" 'ensime-db-backtrace scala-mode-map)
  ;; (bind-key [f1] 'ensime-sbt scala-mode-map)
  ;; (bind-key [f3] 'ensime-edit-definition-of-thing-at-point scala-mode-map)
  ;; (bind-key [f7] 'ensime-db-step scala-mode-map)
  ;; (bind-key [f8] 'ensime-db-step-out scala-mode-map)
  ;; (bind-key [f9] 'ensime-db-continue scala-mode-map)
  ;; (bind-key "C-c C-s" 'ensime-helm-search scala-mode-map)
  ;; (bind-key "C-S-g" 'ensime-show-uses-of-symbol-at-point scala-mode-map)
  ;; (bind-key "C-M-T" 'ensime-show-hierarchy-of-type-at-point scala-mode-map)
  ;; (bind-key "M-R" 'ensime-refactor-diff-rename scala-mode-map)
  ;; (bind-key "M-M" 'ensime-refactor-diff-extract-method scala-mode-map)
  ;; (bind-key "M-L" 'ensime-refactor-diff-extract-local scala-mode-map)
  ;; (bind-key "M-I" 'ensime-refactor-diff-inline-local scala-mode-map)
  ;; (bind-key "M-T" 'ensime-refactor-add-type-annotation scala-mode-map)
  ;; (bind-key "C-O" 'ensime-refactor-diff-organize-imports scala-mode-map)
  ;; (bind-key "M-<return>" 'ensime-import-type-at-point scala-mode-map)
  ;; (bind-key "C-M-j" 'join-line scala-mode-map)
  ;; (bind-key "<backtab>" 'scala-indent:indent-with-reluctant-strategy scala-mode-map)
  ;; (bind-key "s-n" 'ensime-search scala-mode-map)
  ;; (bind-key "s-t" 'ensime-print-type-at-point scala-mode-map)
  ;; (bind-key "M-." 'ensime-edit-definition-with-fallback scala-mode-map)

  )

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
            (setq prettify-symbols-alist scala-mode-prettify-symbols)
            (prettify-symbols-mode t)
            (scala-mode:goto-start-of-code)))


(use-package sbt-mode
  :interpreter
  ("sbt" . sbt-mode)

  :commands sbt-start sbt-command

  :init
  (setq sbt:prefer-nested-projects t)

  :config
  ;; WORKAROUND: https://github.com/hvesalai/sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)

  (setq sbt:program-options '("-Dsbt.supershell=false"))

  (bind-key "C-c c" 'sbt-hydra sbt:mode-map)
  (bind-key "C-c s" 'sbt-command sbt:mode-map)
  (bind-key "C-c e" 'next-error sbt:mode-map))


;; Rust

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun djo/rustic-mode-hook ()
  "So that run C-c C-c C-r works without having to confirm."
  (setq-local buffer-save-without-query t))



;; Use Scala's Metals / lanuage server protocol backend

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "S-C-l")

(use-package lsp-mode
  :hook
  ;; (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-lens-mode)
  (java-mode . lsp)
  (scala-mode . lsp)
  (sbt-mode . lsp)
  ;; (clojure-mode . lsp)   ; Need to figure out how to use this with Boot
  ;; (clojurescript-mode . lsp)
  (javascript-mode . lsp)
  (js2-mode . lsp)
  (typescript-mode . lsp)

  :init
  (setq lsp-keymap-prefix "C-c l")

  :config
  (setq lsp-lens-enable t)
  (lsp-enable-which-key-integration t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)

  :commands lsp lsp-deferred

  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)

  :bind
  ([f3] . 'lsp-goto-implementation))

;; Scala
(use-package lsp-metals)

(use-package lsp-java
  :config
  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m")))

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode

  :bind
  ("C-S-g" . lsp-ui-peek-find-references)

  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-update-mode 'point)
  ;; (lsp-ui-doc-enable nil)
  (lsp-clients-deno-enable-code-lens-references-all-functions 1))

;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs
  :after '(lsp))

(use-package nvm
  :straight (:type git :host github :repo "rejeep/nvm.el")
  :ensure t)

(use-package posframe)                  ; Popups for debug mode

;; optionally if you want to use debugger
(use-package dap-mode
  :after (nvm)

  :hook
  (lsp . dap-mode)
  (lsp . dap-ui-mode)

  ;; Uncomment custom/config below if you want all UI panes to be hidden by default!
  :custom
  (lsp-enable-dap-auto-configure nil)

  :config
  (nvm-use "12")
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)

  ;; Configure some specific languages up-front
  (require 'dap-node)
  (dap-node-setup)

  (require 'dap-chrome)
  (dap-chrome-setup)

  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil))

  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger")))

;; Old, but maybe still useful
;;
;; (use-package lsp-mode
;;   :hook (scala-mode . lsp)
;;   (scala-mode . lsp)
;;   :commands lsp
;;   :config (setq lsp-prefer-flymake nil))


;; (use-package lsp-ui
;;   :bind
;;   (:map lsp-ui-mode-map
;;         ("C-S-g" . lsp-ui-peek-find-references))

;;   :config
;;   (setq lsp-ui-sideline-enable t
;;         lsp-ui-doc-enable t
;;         lsp-ui-flycheck-enable t
;;         lsp-ui-imenu-enable t
;;         lsp-ui-sideline-ignore-duplicate t))

;; (use-package lsp-treemacs)

;; (use-package dap-mode                   ;Debug server protocol support
;;   :hook
;;   (lsp-mode . dap-mode)
;;   (lsp-mode . dap-ui-mode)
;;   )


;;..............................................................................
;; Lua

(use-package lua-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))


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
            (show-paren-mode t)
            (yas-minor-mode t)
            (git-gutter-mode t)
            (company-mode t)))


;;..............................................................................
;; C
(add-hook 'c-mode-hook (lambda ()
                         (yas-minor-mode t)
                         (company-mode t)
                         (smartparens-mode t)))


;; Clojure

;;
;; Smart parenthesis matching everywhere, please
;;

(use-package smartparens
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair

  :preface
  (defun sp-restrict-c (sym)
    "Smartparens restriction on `SYM' for C-derived parenthesis."
    (sp-restrict-to-pairs-interactive "{([" sym))

  :config
  (require 'smartparens-config)
  ;; (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "s-[")
  (sp-pair "{" "}" :wrap "C-{")

  (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

  :init
  (smartparens-global-mode)
  (setq sp-interactive-dwim t)
  (show-smartparens-global-mode t)

  :bind
  (:map smartparens-mode-map
        ("s-{" . 'sp-rewrap-sexp)))


;;(use-package paredit)

(global-set-key (kbd "C-}") 'lispy-forward-slurp-sexp)
(global-set-key (kbd "C-{") 'lispy-forward-barf-sexp)
(global-set-key (kbd "M-q") 'cider-format-defun)


(use-package clojure-mode
  :bind
  (:map clojure-mode-map
        ("C-c k t" . 'kaocha-runner-run-test-at-point)
        ("C-c k r" . 'kaocha-runner-run-tests)
        ("C-c k a" . 'kaocha-runner-run-all-tests)
        ("C-c k w" . 'kaocha-runner-show-warnings)
        ("C-c k h" . 'kaocha-runner-hide-windows)

        ("s-<return>" . 'init-ns)
        ("C-s-<return>" . 'cider-eval-expression-at-point-in-repl)
        ("c-<return>" . 'cider-eval-expression-at-point-in-repl)
        ("M-s-<return>" . 'cider-eval-defun-at-point-in-repl)))

(use-package clj-refactor)


;; Lispy - VI-like keybindings to paredit (https://github.com/abo-abo/lispy)
(use-package lispy)

;; clojure-semantic (https://github.com/kototama/clojure-semantic)
;; (Prerequisite for Lispy Clojure support)
;;(use-package clojure-semantic)

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
(add-hook 'lispy-mode-hook 'lispy-mode-key-unbindings)

;; Use Lispy mode with Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (outline-minor-mode 1)
            (lispy-mode 1)))


(use-package cider
  :config
  (setq cider-lein-command lein-path)
  (setq cider-boot-command boot-path)
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-connection-message-fn #'cider-random-tip)

  ;; Abbreviate the REPL prompt if it gets long
  (setq cider-repl-prompt-function
        '(lambda (namespace)
           (if (> (length namespace) 20)
               (cider-repl-prompt-abbreviated namespace)
             (cider-repl-prompt-default namespace))))
  :hook
  (cider-mode-hook . eldoc-mode)
  (cider-mode-hook . cider-company-enable-fuzzy-completion)
  (cider-repl-mode-hook . cider-company-enable-fuzzy-completion)
  (cider-mode-hook . company-mode)
  (cider-repl-mode . company-mode))


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
    (save-mark-and-excursion
      (cider-switch-to-repl-buffer t)
      (goto-char (point-max))
      (insert form))))

(defun cider-eval-defun-at-point-in-repl ()
  (interactive)
  (let ((form (cider-defun-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (setq form (concat "\n" form))
    (save-mark-and-excursion
      (cider-switch-to-repl-buffer t)
      (goto-char (point-max))
      (insert form))))


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


(use-package clojure-mode-extra-font-locking)


;; Integrate with the kaocha test runner
(use-package kaocha-runner)


;; Search everything please
(use-package helm-cider
  :config
  (helm-cider-mode 1))

;; Keybinding menus!
(use-package cider-hydra
  :after '(clojure-mode)
  :config
  (add-hook 'clojure-mode-hook #'cider-hydra-mode))


;; FIXME: Need to enable Company mode support here?
(use-package yasnippet
  :config
  (yas/load-directory "~/.snippets")
  (yas-global-mode 1))



;; Modeline things
;; (use-package nyan-mode :ensure t)   ; Cat in modeline!
(use-package smart-mode-line-powerline-theme :ensure t)
(use-package smart-mode-line :ensure t)
;; (setq sml/theme 'powerline)
(setq sml/theme 'dark)
(sml/setup)


(use-package tree-mode)


;;SQL

;; From https://raw.githubusercontent.com/bsvingen/sql-indent/master/sql-indent.el
(eval-after-load "sql"
  '(load-library "sql-indent"))


(use-package semantic)
(load "semantic/loaddefs.el")
(semantic-mode 1);;


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


;; (if (boundp 'show-trailing-whitespace)
;;     (progn
;;       (setq-default show-trailing-whitespace t)

;;       (defun ted-hide-trailing-whitespace ()
;;         "Do not highlight trailing whitespace in this buffer."
;;         (interactive)
;;         (setq show-trailing-whitespace nil))

;;       (defun ted-show-trailing-whitespace ()
;;         "Highlight trailing whitespace in this buffer."
;;         (interactive)
;;         (setq show-trailing-whitespace t))

;;       (defun ted-toggle-show-trailing-whitespace ()
;;         "Highlight trailing whitespace in this buffer."
;;         (interactive)
;;         (setq show-trailing-whitespace (not show-trailing-whitespace)))

;;       (mapc (lambda (mode-hook)
;;               (add-hook mode-hook
;;                         'ted-hide-trailing-whitespace))
;;             '(Buffer-menu-mode-hook custom-mode-hook text-mode-hook
;;               term-mode-hook Info-mode-hook comint-mode-hook
;;               buffer-menu-mode-hook apropos-mode-hook
;;               tooltip-show-hook gnus-article-mode-hook mail-mode-hook
;;               gnus-summary-mode-hook message-mode-hook scala-mode-hook
;;               gnus-group-mode-hook eshell-mode-hook w3-mode-hook
;;               initial-calendar-window-hook cider-repl-mode-hook))

;;       (mapc (lambda (mode-hook)
;;               (add-hook mode-hook
;;                         (lambda ()
;;                           (setq show-trailing-whitespace t))))
;;             '(latex-mode-hook LaTeX-mode-hook html-mode-hook)))
;;   (defalias 'ted-hide-trailing-whitespace 'ignore))


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
              "\n\n;; Execute the following to upgrade packages:\n(package-utils-upgrade-all)\n\n;; and the following to change fonts/sizes:\n(set-face-font 'default \"Noto Mono:size=14\")\n(set-face-font 'mode-line \"Noto Sans:weight=ultra-light:size=18\")\n\n;; Enable word wrap:\n(visual-line-mode)

\n\n;;Quick key binding examples:\n(bind-key \"C-c c\" 'sbt-hydra scala-mode-map)\n(define-key ensime-mode-map (kbd \"C-<return>\") 'ensime-print-errors-at-point)\n\n(global-set-key (kbd \"C-/\") 'comment-or-uncomment-region-or-line)\n(global-set-key [A-left] 'windmove-left)\n\n(define-key clojure-mode-map (kbd \"s-<return>\") 'init-ns)\n(define-key clojure-mode-map (kbd \"C-s-<return>\") 'cider-eval-expression-at-point-in-repl)\n(define-key clojure-mode-map (kbd \"M-s-<return>\") 'cider-eval-defun-at-point-in-repl)\n"))))

(setq initial-scratch-message (ted-random-emacs-haiku))
(setq-default word-wrap t)



;;; Misc key (un)bindings


(use-package all-the-icons)

;; We don't want extra tabs in aquamacs
(unless (boundp 'aquamacs-version)

  (use-package centaur-tabs
    :demand
    :config
    (centaur-tabs-mode t)
    (setq centaur-tabs-height 60
          centaur-tabs-set-icons t
          centaur-tabs-plain-icons t
          centaur-tabs-set-modified-marker t
          centaur-tabs-modified-marker "❤"
          centaur-tabs-forward-tab-text "→"
          centaur-tabs-backward-tab-text "←"
          centaur-tabs-style "rounded"
          centaur-tabs-set-bar 'under
          x-underline-at-descent-line t
          centaur-tabs-cycle-scope 'tabs
          centaur-tabs-show-navigation-buttons t
          centaur-tabs-gray-out-icons 'buffer
          uniquify-separator "/"
          uniquify-buffer-name-style 'forward)
    (centaur-tabs-headline-match)
    (centaur-tabs-change-fonts "Noto Sans" 140)

    :hook
    (dashboard-mode . centaur-tabs-local-mode)
    (calender-mode . centaur-tabs-local-mode)
    (helpful-mode . centaur-tabs-local-mode)

    :bind
    ("C-c t s" . centaur-tabs-counsel-switch-group)
    ("C-c t p" . centaur-tabs-group-by-projectile-project)
    ("C-c t g" . centaur-tabs-group-buffer-groups)
    ("M-<left>" . centaur-tabs-backward)
    ("M-<right>" . centaur-tabs-forward))

  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
	  (cond
      ((string-match "slack" (downcase mode-name))
       "Slack")
      ((or (derived-mode-p 'term-mode)
           (derived-mode-p 'vterm-mode)
           (derived-mode-p 'cider-repl-mode))
       "Terminals")
      ((derived-mode-p 'compilation-mode)
       "Compiler output")
      ((derived-mode-p 'custom-mode)
       "Customizer")
      ((derived-mode-p 'pomidor-mode)
       "Pomodoro")
	   ((or (string-equal "*" (substring (buffer-name) 0 1))
	        (memq major-mode '(magit-process-mode
				                  magit-status-mode
				                  magit-diff-mode
				                  magit-log-mode
				                  magit-file-mode
				                  magit-blob-mode
				                  magit-blame-mode)))
	    "Emacs")
	   ((derived-mode-p 'prog-mode)
	    "Programming")
	   ((derived-mode-p 'dired-mode)
	    "Dired")
      ((derived-mode-p 'xwidget-webkit-mode)
       "Web")
	   ((memq major-mode '(helpful-mode
			                 help-mode))
	    "Help")
	   ((or (string-match "org" (downcase mode-name))
           (derived-mode-p 'diary-mode))
	    "OrgMode")
	   (t (or (vc-root-dir)
	          (centaur-tabs-get-group-name (current-buffer))))))))


;; Projectile / Helm
;;
;; @see: http://tuhdo.github.io/helm-intro.html

(use-package helm-icons
  :straight (:type git :host github :repo "yyoncho/helm-icons")
  :config
  (setq helm-icons-provider #'all-the-icons)
  (helm-icons-enable))


(use-package helm
  :config
  (require 'helm-config)

  (setq
   helm-boring-buffer-regexp-list
   '("^diary$"
     "magit-"
     "magit:"
     "helm"
     "*ag"
     "*t"
     "*forge"
     "*Mini"
     "*Messages"
     "*which"
     "*code"
     "*straight"
     "*Slack Log"
     "*Echo")
   helm-boring-file-regexp-list
   '("\\.git$" "\\.hg$" "\\.svn$"  "^\\."  "\\.$"
     "\\.\\.$" "\\.Plo$" "\\.lo$"  "_source.*"
     "_8h.*"  "\\.CVS$" "\\._darcs$"  "\\.la$"
     "\\.o$" "~$"  "^#.*")
   ;; helm-ff-skip-boring-files t
   helm-buffer-max-length nil
   ;; helm-idle-delay 2.0
   ;; helm-find-files-show-icons t
   ;; helm-quick-update t
   helm-candidate-number-limit 40
   ;; helm-use-standard-keys nil
   ;; helm-locate-case-fold-search t
   ))

(use-package helm-ag)

(helm-mode 1)

(setq helm-autoresize-max-height 80)
(helm-autoresize-mode 1)

(use-package helm-descbinds
  :config
  (helm-descbinds-mode)
  (global-set-key (kbd "C-h h") 'describe-bindings))

(global-set-key (kbd "C-x b")
                (lambda () (interactive)
                  (ignore-errors
                    (helm :prompt "Location:"
                          :sources '( helm-source-buffers-list
                                      helm-source-locate
                                      helm-source-bookmarks
                                      helm-source-recentf
                                      helm-source-in-buffer
                                      helm-source-files-in-current-dir)))))


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

(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action) ; rebind tab to autocomplete
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to autocomplete
(define-key helm-map (kbd "\t") 'helm-execute-persistent-action) ; rebind tab to autocomplete
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-<return>") 'helm-select-action)
(define-key helm-find-files-map [(control backspace)] #'helm-find-files-up-one-level)
(define-key helm-read-file-map [(control backspace)] #'helm-find-files-up-one-level)

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(use-package projectile :ensure t)
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-switch-project-action 'treemacs)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'native)
(setq projectile-globally-ignored-directories
      '(".git"
        ".github"
        ".history"
        ".log"
        ".metals"
        ".storybook"
        ".vscode"))
(setq projectile-use-git-grep t)

(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-F") 'projectile-grep)
(global-set-key (kbd "C-c C-f") 'projectile-find-file)
(global-set-key (kbd "C-x M-f") 'projectile-find-file)
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'projectile-switch-to-buffer)

(setq project-switch-commands
        '((?f "File" helm-find-files)
          (?s "Subdir" prot-project-find-subdir)
          (?g "Grep" helm-ag)
          (?d "Dired" project-dired)
          (?b "Buffer" projectile-switch-to-buffer)
          (?q "Query replace" project-query-replace-regexp)
          (?m "Magit" magit-status)
          (?l "Log VC" magit-log)
          (?t "Terminal" terminal)))


(use-package perspective
  :config
  (persp-mode))


;; Save window layout perspectives per projectile project please
(use-package persp-projectile
  :straight (:type git :host github :repo "bbatsov/persp-projectile")
  :config
  (global-set-key (kbd "C-x p p") 'projectile-persp-switch-project)
  (global-set-key (kbd "C-c p p") 'projectile-persp-switch-project))



;; Misc global keybindings/overrides
(global-set-key [tab] 'company-tab-indent-or-complete)

;; esc always quits
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key transient-map [escape] 'transient-quit-one)
(define-key magit-mode-map [escape] 'magit-mode-bury-buffer)
(define-key xwidget-webkit-mode-map [escape] 'quit-window)
(define-key pomidor-mode-map [escape] 'quit-window)
;; (define-key deft-mode-map [escape] 'bury-buffer)
(define-key help-mode-map [escape] 'quit-window)
(define-key debugger-mode-map [escape] 'quit-window)
(define-key special-mode-map [escape] 'quit-window)
(define-key lisp-interaction-mode-map [escape] 'quit-window)
(global-set-key (kbd "<escape>") 'keyboard-quit)

(global-set-key (kbd "C-s") 'save-buffer) ; Was isearch-forward
(global-set-key (kbd "C-f") 'isearch-forward) ; Was find-file
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-set-key (kbd "S-C-f") 'query-replace)
(global-set-key (kbd "M-o") 'helm-find-files)
(global-set-key (kbd "s-o") 'helm-find-files)

(global-set-key [f1] 'terminal)
(global-set-key [\C-f6] 'other-window) ; Eclipse-like switch to the other buffer
(global-set-key [f6] 'helm-buffers-list)
(global-set-key "\C-c z" 'repeat)
(global-set-key (kbd "M-/") 'hippie-expand)
;;(global-set-key (kbd "C-z") 'undo)

(global-set-key [s-left] 'windmove-left)   ; move to left window
(global-set-key [s-right] 'windmove-right)        ; move to right window
(global-set-key [s-up] 'windmove-up)              ; move to upper window
(global-set-key [s-down] 'windmove-down)          ; move to lower window

(global-set-key [C-M-left] 'windmove-left)   ; move to left window
(global-set-key [C-M-right] 'windmove-right)        ; move to right window
(global-set-key [C-M-up] 'windmove-up)              ; move to upper window
(global-set-key [C-M-down] 'windmove-down)          ; move to lower window

(global-set-key (kbd "C-S-x") 'kill-region)
(global-set-key (kbd "C-S-c") 'kill-ring-save)
(global-set-key (kbd "C-S-v") 'yank)

(require 'redo+)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)


(defun set-local-fonts ()
  "Override fonts."
  ;; default Latin font (e.g. Consolas)
  (set-face-font 'default (format "Noto Mono:size=%d" (normalize-pts 10)))
  (set-face-font 'variable-pitch (format "Noto Sans:size=%d" (normalize-pts 10)))
  (set-face-font 'mode-line (format "Noto Sans:weight=ultra-light:size=%d" (normalize-pts 12)))

  ;; Modern color UTF-8 glyphs/emoji
  (set-fontset-font t 'symbol "Noto Color Emoji")
  (set-fontset-font t 'symbol "Symbola" nil 'append))

(when window-system
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'set-local-fonts)
    (set-local-fonts))

  (set-frame-size (selected-frame) 120 42)
  (set-face-attribute 'region nil :background "#777" :foreground "#ffffff"))


(defun unclutter-window ()
  (interactive)
  (set-face-foreground 'vertical-border (face-background 'default))
  (set-face-background 'fringe (face-background 'default))
  (set-face-foreground 'fringe (face-background 'default)))

(unclutter-window)

;; Dim inactive windows
(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-company-box)
  (dimmer-configure-helm)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-which-key))
(dimmer-mode 1)



(pomidor)  ; Start the pomidor timer; [F12] to interact
(find-file (concat (expand-file-name "~/_NOTES") "/NOTES.md" ))


(provide 'init)
;;; init.el ends here
