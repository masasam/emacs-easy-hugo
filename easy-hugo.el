;;; easy-hugo.el --- Package for writing blogs made with hugo -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Masashi Miyaura

;; Author: Masashi Miyaura
;; URL: https://github.com/masasam/emacs-easy-hugo
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

;; Package for writing blogs made with hugo

;;; Code:

(defgroup easy-hugo nil
  "Writing blogs made with hugo."
  :group 'easy-hugo)

(defcustom easy-hugo-basedir nil
  "Directory where hugo html source code is placed."
  :type 'string)

(defcustom easy-hugo-url nil
  "Url of the site operated by hugo."
  :type 'string)

(defcustom easy-hugo-sshdomain "blogdomain"
  "Domain of hugo at your ~/.ssh/config."
  :type 'string)

(defcustom easy-hugo-root "/home/blog/"
  "Root directory of hugo at your server."
  :type 'string)

(defcustom easy-hugo-previewtime 300
  "Preview display time."
  :type 'integer)

(defvar easy-hugo--server-process nil)

;;;###autoload
(defun easy-hugo-article ()
  "Open a list of articles written in hugo."
  (interactive)
  (when (null easy-hugo-basedir)
    (error "Please set easy-hugo-basedir variable"))
  (find-file (concat easy-hugo-basedir "content/post/")))

;;;###autoload
(defun easy-hugo-publish ()
  "Adapt local change to the server with hugo."
  (interactive)
  (when (null easy-hugo-basedir)
    (error "Please set easy-hugo-basedir variable"))
  (let ((default-directory (concat (expand-file-name easy-hugo-basedir) "/")))
    (shell-command-to-string (concat "rm -rf public"))
    (shell-command-to-string "hugo --destination public")
    (shell-command-to-string (concat "rsync -rtpl --delete public/ " easy-hugo-sshdomain":"easy-hugo-root))
    (message "Blog published")
    (unless (null easy-hugo-url)
      (browse-url easy-hugo-url))))

;;;###autoload
(defun easy-hugo-newpost ()
  "Create a new post with hugo."
  (interactive)
  (when (null easy-hugo-basedir)
    (error "Please set easy-hugo-basedir variable"))
  (let ((filename (concat "post/" (read-from-minibuffer "Filename: " '(".md" . 1) nil nil nil)))
	(default-directory (expand-file-name easy-hugo-basedir)))
    (if (file-exists-p (concat easy-hugo-basedir "content/" filename))
	(error (concat filename "is a file that already exists"))
      (apply 'call-process "hugo" nil "*hugo*" t (list "new" filename)))
    (find-file (concat easy-hugo-basedir "content/" filename))
    (goto-char (point-max))
    (save-buffer)))

;;;###autoload
(defun easy-hugo-preview ()
  "Preview hugo at localhost."
  (interactive)
  (when (null easy-hugo-basedir)
    (error "Please set easy-hugo-basedir variable"))
  (let ((default-directory (expand-file-name easy-hugo-basedir)))
    (if (process-live-p easy-hugo--server-process)
	(browse-url "http://localhost:1313/")
      (progn
	(setq easy-hugo--server-process
	      (start-process "hugo-server" "*Hugo Server*" "hugo" "server"))
	(browse-url "http://localhost:1313/")
	(run-at-time easy-hugo-previewtime nil 'easy-hugo-preview-end)))))

(defun easy-hugo-preview-end ()
  "Finish previewing hugo at localhost."
  (unless (null easy-hugo--server-process)
    (delete-process easy-hugo--server-process)))

(provide 'easy-hugo)

;;; easy-hugo.el ends here
