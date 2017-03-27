;;; easy-hugo.el --- Package for writing blogs made with hugo -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Masashı Mıyaura

;; Author: Masashı Mıyaura
;; URL: https://github.com/masasam/emacs-easy-hugo
;; Version: 0.3.1
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
  :group 'tools)

(defcustom easy-hugo-basedir nil
  "Directory where hugo html source code is placed."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-url nil
  "Url of the site operated by hugo."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-sshdomain nil
  "Domain of hugo at your ~/.ssh/config."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-root nil
  "Root directory of hugo at your server."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-previewtime 300
  "Preview display time."
  :group 'easy-hugo
  :type 'integer)

(defcustom easy-hugo-default-ext ".md"
  "Root directory of hugo at your server."
  :group 'easy-hugo
  :type 'string)

(defvar easy-hugo--server-process nil)

(defconst easy-hugo--buffer-name "*Hugo Server*")

(defconst easy-hugo--formats '("md" "org"))

;;;###autoload
(defun easy-hugo-article ()
  "Open a list of articles written in hugo."
  (interactive)
  (unless easy-hugo-basedir
    (error "Please set easy-hugo-basedir variable"))
  (find-file (expand-file-name "content/post" easy-hugo-basedir)))

(defmacro easy-hugo-with-env (&rest body)
  "Evaluate BODY with `default-directory' set to `easy-hugo-basedir'.
Report an error if hugo is not installed, or if `easy-hugo-basedir' is unset."
  `(progn
     (unless easy-hugo-basedir
       (error "Please set easy-hugo-basedir variable"))
     (unless (executable-find "hugo")
       (error "'hugo' is not installed"))
     (let ((default-directory easy-hugo-basedir))
       ,@body)))

;;;###autoload
(defun easy-hugo-publish ()
  "Adapt local change to the server with hugo."
  (interactive)
  (unless easy-hugo-sshdomain
    (error "Please set easy-hugo-sshdomain variable"))
  (unless easy-hugo-root
    (error "Please set easy-hugo-root variable"))
  (unless (executable-find "rsync")
    (error "'rsync' is not installed"))
  (unless (file-exists-p "~/.ssh/config")
    (error "There is no ~/.ssh/config"))
  (easy-hugo-with-env
   (delete-directory "public" t nil)
   (shell-command-to-string "hugo --destination public")
   (shell-command-to-string (concat "rsync -rtpl --delete public/ " easy-hugo-sshdomain ":" (shell-quote-argument easy-hugo-root)))
   (message "Blog published")
   (when easy-hugo-url
     (browse-url easy-hugo-url))))

(defun easy-hugo--org-headers ()
  "Return a draft org mode header string for a new article."
  (let ((datetimezone
         (concat
          (format-time-string "%Y-%m-%dT%T")
          (easy-hugo--orgtime-format (format-time-string "%z")))))
    (concat
     "#+TITLE: New"
     "\n#+DATE: " datetimezone
     "\n#+PUBLISHDATE: " datetimezone
     "\n#+DRAFT: nil"
     "\n# #+TAGS: nil, nil"
     "\n#+DESCRIPTION: Short description"
     "\n\n")))

;;;###autoload
(defun easy-hugo-newpost (post-file)
  "Create a new post with hugo.
POST-FILE needs to have and extension '.md' or '.org'."
  (interactive (list (read-from-minibuffer "Filename: " `(,easy-hugo-default-ext . 1) nil nil nil)))
  (let ((filename (concat "post/" post-file))
        (file-ext (file-name-extension post-file)))
    (when (not (member file-ext easy-hugo--formats))
      (error "Please enter .md or .org file name"))
    (easy-hugo-with-env
     (when (file-exists-p (file-truename (concat "content/" filename)))
       (error "%s already exists!" (concat easy-hugo-basedir "content/" filename)))
     (if (string-equal file-ext "md")
         (call-process "hugo" nil "*hugo*" t "new" filename))
     (find-file (concat "content/" filename))
     (if (string-equal file-ext "org")
         (insert (easy-hugo--org-headers)))
     (goto-char (point-max))
     (save-buffer))))

;;;###autoload
(defun easy-hugo-preview ()
  "Preview hugo at localhost."
  (interactive)
  (easy-hugo-with-env
   (if (process-live-p easy-hugo--server-process)
       (browse-url "http://localhost:1313/")
     (progn
       (setq easy-hugo--server-process
             (start-process "hugo-server" easy-hugo--buffer-name "hugo" "server"))
       (browse-url "http://localhost:1313/")
       (run-at-time easy-hugo-previewtime nil 'easy-hugo--preview-end)))))

(defun easy-hugo--preview-end ()
  "Finish previewing hugo at localhost."
  (unless (null easy-hugo--server-process)
    (delete-process easy-hugo--server-process)))

(defun easy-hugo--orgtime-format (x)
  "Format orgtime as X."
  (concat (substring x 0 3) ":" (substring x 3 5)))

(provide 'easy-hugo)

;;; easy-hugo.el ends here
