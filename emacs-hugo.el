;;; emacs-hugo.el --- hugo with emacs interface -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by masasam

;; Author: masasam
;; URL: https://github.com/masasam/emacs-hugo
;; Version: 0.01
;; Package-Requires: (())

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

;; tramp with helm interface

;;; Code:

(defvar hugo-base-dir "~/src/github.com/masasam/blog/")
(defvar hugo-domain "blogdomain")
(defvar hugo-root "/home/blog/")

(defun hugo-edit ()
  (interactive)
  (find-file (concat hugo-base-dir "content/post/")))

(defun hugo-entry ()
  (interactive)
  (let ((title (read-from-minibuffer "Title: "))
	 (filename (concat "post/"
                           (read-from-minibuffer "Filename: "
                                                 (replace-regexp-in-string "-\\.md" ".md"
                                                   (concat (downcase
                                                            (replace-regexp-in-string "[^a-z0-9]+" "-"
                                                                                      title))
                                                           ".md")))))
         (path (concat hugo-base-dir "content/" filename))
	 (default-directory (expand-file-name hugo-base-dir)))
    (if (file-exists-p path)
        (message "File exists!")
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

(defun hugo-publish ()
  (interactive)
  (let* ((default-directory (concat (expand-file-name hugo-base-dir) "/")))
    (shell-command-to-string (concat "rm -rf public"))
    (shell-command-to-string "hugo -d public")
    (shell-command-to-string "find public \( -name '*.js' -or -name '*.css' -or -name '*.svg' -or -name '*.html' \) -exec gzip -k9 '{}' \;")
    (shell-command-to-string (concat "rsync -rtpl --delete public/ " hugo-domain":"hugo-root))
    (message "Blog published")
    ))

;;; emacs-hugo.el ends here
