;;;; General settings
(setq inhibit-splash-screen t)
(when window-system (global-unset-key "\C-z"))
(set-face-attribute 'default nil :height 90) ; 9 point fonts by default

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
(add-to-list 'load-path "~/.emacs.d/")
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

; Java configuration


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager-managed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialize the package manager with the MELPA archive
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-refresh-contents) 

;; Scala-mode from the package manager
(unless (package-installed-p 'scala-mode2)
  (package-install 'scala-mode2))

; Malabar Mode (for Java)
(unless (package-installed-p 'malabar-mode)
  (package-install 'malabar-mode))

; Java / Malabar mode
(require 'cedet)
(require 'semantic)
(load "semantic/loaddefs.el")
(semantic-mode 1);;
(require 'malabar-mode)
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))


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

