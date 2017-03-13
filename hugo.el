;;; hugo.el --- hugo utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Masashi Miyaura

;; Author: Masashi Miyaura
;; URL: https://github.com/masasam/emacs-hugo
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

;; hugo utilities

;;; Code:

(defgroup hugo nil
  "hugo with emacs interface"
  :group 'hugo)

(defcustom hugo-base-dir
  "~/hugo/"
  "Base directory of hugo"
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
(defun hugo-edit ()
  "Open a list of articles written in hugo with dired."
  (interactive)
  (find-file (concat hugo-base-dir "content/post/")))

;;;###autoload
(defun hugo-publish ()
  "Adapt local change to the server with hugo."
  (interactive)
  (let ((default-directory (concat (expand-file-name hugo-base-dir) "/")))
    (shell-command-to-string (concat "rm -rf public"))
    (shell-command-to-string "hugo -d public")
    (shell-command-to-string "find public \( -name '*.js' -or -name '*.css' -or -name '*.svg' -or -name '*.html' \) -exec gzip -k9 '{}' \;")
    (shell-command-to-string (concat "rsync -rtpl --delete public/ " hugo-domain":"hugo-root))
    (message "Blog published")
    ))

;;;###autoload
(defun hugo-publish-github-pages ()
  "Adapt local change to the server with hugo."
  (interactive)
  (let ((default-directory (concat (expand-file-name hugo-base-dir) "/")))
    (shell-command-to-string (concat "rm -rf public"))
    (shell-command-to-string "hugo -d public")
    (shell-command-to-string "cd public")
    (shell-command-to-string "git add -A")
    (shell-command-to-string "git commit -m 'hugo commit'")
    (shell-command-to-string "git push origin master")
    (message "Blog published")
    ))

;;;###autoload
(defun hugo-entry ()
  "Create a new entry with hugo."
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
	 (filename (concat "post/" (read-from-minibuffer "Filename: " (replace-regexp-in-string "-\\.md" ".md" (concat (downcase (replace-regexp-in-string "[^a-z0-9]+" "-" title)) ".md")))))
         (path (concat hugo-base-dir "content/" filename))
	 (default-directory (expand-file-name hugo-base-dir)))
    (if (file-exists-p path)
        (message "File exists")
      (apply 'call-process "hugo" nil "*hugo*" t (list "new" filename)))
    (find-file path)
    (hugo-replace-key "title" title)
    (goto-char (point-max))
    (save-buffer)))

(defun hugo-replace-key (key val)
  (save-excursion
    (goto-char (point-min))
    (if (and (re-search-forward (concat key " = \"") nil t)
	     (re-search-forward "[^\"]+" (line-end-position) t))
        (or (replace-match val) t)
      (when (and (re-search-forward (concat key " = ") nil t)
                 (re-search-forward ".+" (line-end-position) t))
        (or (replace-match val) t)))))

(provide 'hugo)

;;; hugo.el ends here
