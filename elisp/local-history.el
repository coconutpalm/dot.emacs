;; From https://raw.githubusercontent.com/cneira/emacs-localhistory/master/localhistory.el
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Carlos Neira  <cneirabustos@gmail.com>
;; Version: 1.0
;; Keywords: magit,localhistory,backups


;;; Commentary:
;;; This package tries to emulate the intellij localhistory option using magit api.
;;; All saved buffers will be put into a git repository located in ~/.localhistory-emacs/ where you could
;;; use magit to recover your changes,etc.
;;; TODO:
;;; what happens if two files have the same name ? add diffing between commits.
;;; Code:


(require 'magit)
(require 'git-commit)
(require 'magit-core)
(require 'magit-diff)
(require 'magit-apply)
(require 'magit-log)

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")


;; Initialize local git repo just once 
(defun local-history-init ()
  (let* ((local-history-dir "~/.localhistory-emacs" ) (local-history-git (concat local-history-dir "/.git") ) )
    (if (file-exists-p local-history-git) (message "Not creating already exists")
      (magit-call-git "init" (magit-convert-git-filename
                              (expand-file-name local-history-dir))))))


(defun dired-copy-file-here (file)
  (copy-file file "~/.localhistory-emacs/"  t ))



(defun local-history-stage-current-buffer ()
  (let*
      ( (file-to-stage  (buffer-file-name (current-buffer)))
        (new-file (first (last (split-string  file-to-stage  "/")))))
    (progn
      (setq saved-default-directory default-directory)
      (dired-copy-file-here file-to-stage )
      (setq default-directory "~/.localhistory-emacs/")
      (magit-stage-file new-file)
      (magit-call-git  "commit" (concat "-m " "\""  (format-time-string current-date-time-format (current-time)) "\"" ) new-file)
      )))


(defun local-history-check-changes ()
  (interactive )
  (setq default-directory "~/.localhistory-emacs/")
  (magit-diff-setup nil (list "--no-index")
                    nil (list (expand-file-name (buffer-file-name (current-buffer)) )
                              (expand-file-name
                               (concat "~/.localhistory-emacs/" (first (last (split-string (buffer-file-name (current-buffer))   "/"))))))))


;;;###autoload
(add-hook 'after-save-hook 'local-history-stage-current-buffer)
(provide 'local-history)
