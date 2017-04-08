;;; easy-hugo.el --- Write blogs made with hugo by markdown or org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Masashı Mıyaura

;; Author: Masashı Mıyaura
;; URL: https://github.com/masasam/emacs-easy-hugo
;; Version: 0.5.4
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

;; Package for writing blogs made with hugo by markdown or org-mode

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
  "Default extension when posting new articles."
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
   (when (file-directory-p "public")
     (delete-directory "public" t nil))
   (shell-command-to-string "hugo --destination public")
   (shell-command-to-string (concat "rsync -rtpl --delete public/ " easy-hugo-sshdomain ":" (shell-quote-argument easy-hugo-root)))
   (message "Blog published")
   (when easy-hugo-url
     (browse-url easy-hugo-url))))

(defun easy-hugo--org-headers (file)
  "Return a draft org mode header string for a new article as FILE."
  (let ((datetimezone
         (concat
          (format-time-string "%Y-%m-%dT%T")
          (easy-hugo--orgtime-format (format-time-string "%z")))))
    (concat
     "#+TITLE: " file
     "\n#+DATE: " datetimezone
     "\n#+PUBLISHDATE: " datetimezone
     "\n#+DRAFT: nil"
     "\n#+TAGS: nil, nil"
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
         (insert (easy-hugo--org-headers (file-name-base post-file))))
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

;;;###autoload
(defun easy-hugo-deploy ()
  "Execute deploy.sh script locate at 'easy-hugo-basedir'."
  (interactive)
  (easy-hugo-with-env
   (let ((deployscript (file-truename (concat easy-hugo-basedir "deploy.sh"))))
     (unless (executable-find deployscript)
       (error "%s do not execute" deployscript))
     (shell-command-to-string (shell-quote-argument deployscript))
     (message "Blog deployed")
     (when easy-hugo-url
       (browse-url easy-hugo-url)))))

(defconst easy-hugo-help
  "Easy-hugo

n ... New blog post        D ... Deploy at github-pages
p ... Preview              r ... Refresh easy-hugo
v ... Open view-mode       g ... Refresh easy-hugo
d ... Delete current post  j ... Next line
l ... List of article      k ... Previous line
P ... Publish to server    q ... Quit easy-hugo

"
  "Help of easy-hugo.")

(defconst easy-hugo-first-help
  "Welcome to Easy-hugo

Let's post an article first.
Press n on this screen or M-x easy-hugo-newpost.
Enter a article file name in the minibuffer.
Then M-x easy-hugo again or refresh the screen with r or g key in this buffer,
article which you wrote should appear here.
Enjoy!

"
  "Help of easy-hugo first time.")

(defvar easy-hugo-mode-map
  (let ((map (make-keymap)))
    (define-key map "n" 'easy-hugo-newpost)
    (define-key map "l" 'easy-hugo-article)
    (define-key map "p" 'easy-hugo-preview)
    (define-key map "P" 'easy-hugo-publish)
    (define-key map "o" 'easy-hugo-open)
    (define-key map "\C-m" 'easy-hugo-open)
    (put 'easy-hugo-open :advertised-binding "\C-m")
    (define-key map "d" 'easy-hugo-delete)
    (define-key map "e" 'easy-hugo-open)
    (define-key map "f" 'easy-hugo-open)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map " " 'next-line)
    (define-key map [?\S-\ ] 'previous-line)
    (define-key map "v" 'easy-hugo-view)
    (define-key map "r" 'easy-hugo)
    (define-key map "g" 'easy-hugo)
    (define-key map "D" 'easy-hugo-deploy)
    (define-key map "q" 'easy-hugo-quit)
    map)
  "Keymap for easy-hugo major mode.")

(defvar easy-hugo-mode-buffer nil
  "Main buffer of easy-hugo.")

(defvar easy-hugo-cursor nil
  "Cursor of easy-hugo.")

(defvar easy-hugo-line nil
  "Line of easy-hugo.")

(defconst easy-hugo-buffer-name "*Easy-hugo*"
  "Buffer name of easy-hugo.")

(define-derived-mode easy-hugo-mode special-mode "Easy-hugo"
  "Major mode for easy hugo.")

(defun easy-hugo-quit ()
  "Quit easy hugo."
  (interactive)
  (buffer-live-p easy-hugo-mode-buffer)
  (kill-buffer easy-hugo-mode-buffer))

(defun easy-hugo-open ()
  "Open file."
  (interactive)
  (unless (or (string-match "^
$" (thing-at-point 'line)) (eq (point) (point-max)) (eq (line-number-at-pos) 1))
    (let ((file (expand-file-name (concat "content/post/" (substring (thing-at-point 'line) 20 -1)) easy-hugo-basedir)))
      (when (and (file-exists-p file) (not (file-directory-p file)))
	(find-file file)))))

(defun easy-hugo-view ()
  "Open file with 'view-mode'."
  (interactive)
  (let ((file (expand-file-name (concat "content/post/" (thing-at-point 'filename)) easy-hugo-basedir)))
    (when (and (file-exists-p file) (not (file-directory-p file)))
      (view-file file))))

(defun easy-hugo-delete ()
  "Delete file."
  (interactive)
  (let ((file (expand-file-name (concat "content/post/" (thing-at-point 'filename)) easy-hugo-basedir)))
    (when (and (file-exists-p file) (not (file-directory-p file)))
      (when (y-or-n-p "Do you delete a file? ")
	(setq easy-hugo-line (- (line-number-at-pos) 11))
	(delete-file file)
	(easy-hugo)
	(when (> easy-hugo-line 0)
	  (forward-line easy-hugo-line)
	  (forward-char 20))))))

;;;###autoload
(defun easy-hugo ()
  "Easy hugo."
  (interactive)
  (easy-hugo-with-env
   (unless (file-directory-p (expand-file-name "content/post" easy-hugo-basedir))
     (error "Did you execute hugo new site bookshelf?"))
   (setq easy-hugo-mode-buffer (get-buffer-create easy-hugo-buffer-name))
   (switch-to-buffer easy-hugo-mode-buffer)
   (setq-local default-directory easy-hugo-basedir)
   (setq buffer-read-only nil)
   (erase-buffer)
   (insert easy-hugo-help)
   (setq easy-hugo-cursor (point))
   (let ((files (directory-files (expand-file-name "content/post" easy-hugo-basedir)))
	 (lists (list)))
     (if (eq 2 (length files))
	 (progn
	   (insert easy-hugo-first-help)
	   (easy-hugo-mode)
	   (goto-char easy-hugo-cursor))
       (progn
	 (while files
	   (unless (or (string= (car files) ".") (string= (car files) ".."))
	     (push
	      (concat (format-time-string "%Y-%m-%d %H:%M:%S " (nth 5 (file-attributes (expand-file-name (concat "content/post/" (car files)) easy-hugo-basedir)))) (car files))
	      lists))
	   (pop files))
	 (setq lists (reverse (sort lists 'string<)))
	 (while lists
	   (insert (concat (car lists) "\n"))
	   (pop lists))
	 (goto-char easy-hugo-cursor)
	 (forward-char 20)
	 (easy-hugo-mode))))))

(provide 'easy-hugo)

;;; easy-hugo.el ends here
