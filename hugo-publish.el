;;; hugo-publish.el --- hugo utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Masashi Miyaura

;; Author: Masashi Miyaura
;; URL: https://github.com/masasam/emacs-hugo-publish
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Package for writing blogs made with hugo just by Emacs

;;; Code:

(defgroup hugo nil
  "writing blogs made with hugo"
  :group 'hugo)

(defcustom hugo-base-dir
  "~/hugo/"
  "Directory where hugo html source code is placed"
  :type 'string)

(defcustom hugo-domain
  "blogdomain"
  "Domain of hugo at your ~/.ssh/config"
  :type 'string)

(defcustom hugo-root
  "/home/blog/"
  "Root directory of hugo at your server"
  :type 'string)

;;;###autoload
(defun hugo-articlelist ()
  "Open a list of articles written in hugo."
  (interactive)
  (find-file (concat hugo-base-dir "content/post/")))

;;;###autoload
(defun hugo-publish ()
  "Adapt local change to the server with hugo."
  (interactive)
  (let ((default-directory (concat (expand-file-name hugo-base-dir) "/")))
    (shell-command-to-string (concat "rm -rf public"))
    (shell-command-to-string "hugo --destination public")
    (shell-command-to-string (concat "rsync -rtpl --delete public/ " hugo-domain":"hugo-root))
    (message "Blog published")
    ))

;;;###autoload
(defun hugo-newpost ()
  "Create a new post with hugo."
  (interactive)
  (let ((filename (concat "post/" (read-from-minibuffer "Filename: " '(".md" . 1) nil nil nil)))
	(default-directory (expand-file-name hugo-base-dir)))
    (if (file-exists-p (concat hugo-base-dir "content/" filename))
	(error (concat "File exists " filename))
      (apply 'call-process "hugo" nil "*hugo*" t (list "new" filename)))
    (find-file (concat hugo-base-dir "content/" filename))
    (goto-char (point-max))
    (save-buffer)))

(provide 'hugo-publish)

;;; hugo-publish.el ends here
