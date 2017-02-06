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
(defvar hugo-buffer "*hugo*")

(defun hugo-edit ()
  (interactive)
  (find-file (concat hugo-base-dir "content/post/")))

(defun hugo-new-post ()
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
	 (filename (concat "post/"
                           (read-from-minibuffer "Filename: "
                                                 (replace-regexp-in-string "-\\.md" ".md"
                                                   (concat (downcase
                                                            (replace-regexp-in-string "[^a-z0-9]+" "-"
                                                                                      title))
                                                           ".md")))))
         (path (concat hugo-base-dir "content/" filename)))
    (if (file-exists-p path)
        (message "File already exists!")
      (hugo-command "new" filename)
      (find-file path)
      (hugo-replace-key "title" title)
      (goto-char (point-max))
      (save-buffer))))

;;helper functions
(defun hugo-command (&rest args)
  (let ((default-directory (expand-file-name hugo-base-dir)))
    (apply 'call-process "hugo" nil hugo-buffer t args)))

(defun hugo-replace-key (key val)
  (save-excursion
    (goto-char (point-min))
    ; quoted value
    (if (and (re-search-forward (concat key " = \"") nil t)
               (re-search-forward "[^\"]+" (line-end-position) t))
        (or (replace-match val) t) ; ensure we return t
      ; unquoted value
      (when (and (re-search-forward (concat key " = ") nil t)
                 (re-search-forward ".+" (line-end-position) t))
        (or (replace-match val) t)))))

;;publishing the blog
(defun hugo-publish ()
  (interactive)
  (let* ((default-directory (concat (expand-file-name hugo-base-dir) "/")))
    (when (call-process "bash" nil hugo-buffer t  "./upload.sh")
      (message "Blog published"))))

;;; emacs-hugo.el ends here
