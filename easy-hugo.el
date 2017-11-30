;;; easy-hugo.el --- Write blogs made with hugo by markdown or org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Masashı Mıyaura

;; Author: Masashı Mıyaura
;; URL: https://github.com/masasam/emacs-easy-hugo
;; Version: 2.4.19
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

;; Emacs major mode for writing blogs made with hugo
;; by markdown or org-mode or AsciiDoc or reStructuredText or mmark or html.
;; You can publish your blog to the server or Github Pages
;; or Amazon S3 or Google Cloud Storage.

;;; Code:

(require 'cl-lib)

(defgroup easy-hugo nil
  "Writing blogs made with hugo."
  :group 'tools)

(defgroup easy-hugo-faces nil
  "Faces used in `easy-hugo'"
  :group 'easy-hugo :group 'faces)

(defcustom easy-hugo-basedir nil
  "Directory where hugo html source code is placed."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-preview-url "http://localhost:1313/"
  "Preview url of easy-hugo."
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

(defcustom easy-hugo-image-directory "images"
  "Image file directory under 'static' directory."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-default-picture-directory "~"
  "Default directory for selecting images with `easy-hugo-put-image'."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-amazon-s3-bucket-name nil
  "Amazon S3 bucket name."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-google-cloud-storage-bucket-name nil
  "Google Cloud Storage bucket name."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-default-ext ".md"
  "Default extension when posting new articles."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-no-help nil
  "No help flg of easy-hugo."
  :group 'easy-hugo
  :type 'integer)

(defcustom easy-hugo-additional-help nil
  "Additional help flg of easy-hugo."
  :group 'easy-hugo
  :type 'integer)

(defcustom easy-hugo-sort-default-char nil
  "Default setting to sort with charactor."
  :group 'easy-hugo
  :type 'integer)

(defcustom easy-hugo-publish-chmod "Du=rwx,Dgo=rx,Fu=rw,Fog=r"
  "Permission when publish.
The default is drwxr-xr-x."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-github-deploy-script "deploy.sh"
  "Github-deploy-script file name."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-markdown-extension "md"
  "Markdown extension.
Please select md or markdown or mdown.
Because only these three are supported by hugo."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-asciidoc-extension "ad"
  "Asciidoc extension.
Please select ad or asciidoc or adoc.
Because only these three are supported by hugo."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-html-extension "html"
  "Html extension.
Please select html or htm.
Because only two are supported by hugo."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-postdir "content/post"
  "Directory where the theme stores its posts."
  :group 'easy-hugo
  :type 'string)

(defcustom easy-hugo-help-line 7
  "Number of lines of `easy-hugo-help'."
  :group 'easy-hugo
  :type 'integer)

(defcustom easy-hugo-add-help-line 4
  "Number of lines of `easy-hugo-add-help'."
  :group 'easy-hugo
  :type 'integer)

(defvar easy-hugo--preview-loop t
  "Preview loop flg.")

(defvar easy-hugo--server-process nil
  "Hugo process.")

(defvar easy-hugo--unmovable-line (+ easy-hugo-help-line 4)
  "Impossible to move below this line.")

(defvar easy-hugo--draft-list nil
  "Draft list flg.")

(defvar easy-hugo--draft-mode nil
  "Display draft-mode.")

(defvar easy-hugo--publish-timer nil
  "Easy-hugo-publish-timer.")

(defvar easy-hugo--basedir-timer nil
  "Easy-hugo-basedir-timer.")

(defvar easy-hugo--sshdomain-timer nil
  "Easy-hugo-sshdomain-timer.")

(defvar easy-hugo--root-timer nil
  "Easy-hugo-root-timer.")

(defvar easy-hugo--url-timer nil
  "Easy-hugo-url-timer.")

(defvar easy-hugo--github-deploy-timer nil
  "Easy-hugo-github-deploy-timer.")

(defvar easy-hugo--github-deploy-basedir-timer nil
  "Easy-hugo-github-deploy-basedir-timer.")

(defvar easy-hugo--github-deploy-url-timer nil
  "Easy-hugo-github-deploy-url-timer.")

(defvar easy-hugo--amazon-s3-timer nil
  "Easy-hugo-amazon-s3-timer.")

(defvar easy-hugo--amazon-s3-basedir-timer nil
  "Easy-hugo-amazon-s3-basedir-timer.")

(defvar easy-hugo--amazon-s3-url-timer nil
  "Easy-hugo-amazon-s3-url-timer.")

(defvar easy-hugo--amazon-s3-bucket-name-timer nil
  "Easy-hugo-amazon-s3-bucket-name-timer.")

(defvar easy-hugo--google-cloud-storage-timer nil
  "Easy-hugo-google-cloud-storage-timer.")

(defvar easy-hugo--google-cloud-storage-basedir-timer nil
  "Easy-hugo-google-cloud-storage-basedir-timer.")

(defvar easy-hugo--google-cloud-storage-url-timer nil
  "Easy-hugo-google-cloud-storage-url-timer.")

(defvar easy-hugo--google-cloud-storage-bucket-name-timer nil
  "Easy-hugo-google-cloud-storage-bucket-name-timer.")

(defvar easy-hugo--publish-basedir nil
  "Easy-hugo-publish-var.")

(defvar easy-hugo--publish-sshdomain nil
  "Easy-hugo-publish-var.")

(defvar easy-hugo--publish-root nil
  "Easy-hugo-publish-var.")

(defvar easy-hugo--publish-url nil
  "Easy-hugo-publish-var.")

(defvar easy-hugo--github-deploy-basedir nil
  "Easy-hugo-github-deploy-var.")

(defvar easy-hugo--github-deploy-url nil
  "Easy-hugo-github-deploy-var.")

(defvar easy-hugo--amazon-s3-basedir nil
  "Easy-hugo-amazon-s3-var.")

(defvar easy-hugo--amazon-s3-url nil
  "Easy-hugo-amazon-s3-var.")

(defvar easy-hugo--amazon-s3-bucket-name nil
  "Easy-hugo-amazon-s3-var.")

(defvar easy-hugo--google-cloud-storage-basedir nil
  "Easy-hugo-google-cloud-storage-var.")

(defvar easy-hugo--google-cloud-storage-url nil
  "Easy-hugo-google-cloud-storage-var.")

(defvar easy-hugo--google-cloud-storage-bucket-name nil
  "Easy-hugo-google-cloud-storage-var.")

(defvar easy-hugo--current-postdir 0
  "Easy-hugo current postdir.")

(defvar easy-hugo--postdir-list nil
  "Easy-hugo postdir list.")

(defconst easy-hugo--unmovable-line-default easy-hugo--unmovable-line
  "Default value of impossible to move below this line.")

(defconst easy-hugo--preview-buffer "*Hugo Preview*"
  "Easy-hugo preview buffer name.")

(defconst easy-hugo--formats `(,easy-hugo-markdown-extension
			       "org"
			       ,easy-hugo-asciidoc-extension
			       "rst"
			       "mmark"
			       ,easy-hugo-html-extension))

(defface easy-hugo-help-face
  '((((class color) (background light)) (:bold t :foreground "#82c600" :background "#f0f8ff"))
    (((class color) (background dark)) (:bold t :foreground "#82c600" :background "#2f4f4f")))
  ""
  :group 'easy-hugo-faces)

(defvar easy-hugo--mode-buffer nil
  "Main buffer of easy-hugo.")

(defvar easy-hugo--cursor nil
  "Cursor of easy-hugo.")

(defvar easy-hugo--line nil
  "Line of easy-hugo.")

(defvar easy-hugo--sort-time-flg 1
  "Sort time flg of easy-hugo.")

(defvar easy-hugo--sort-char-flg nil
  "Sort char flg of easy-hugo.")

(defvar easy-hugo--refresh nil
  "Refresh flg of easy-hugo.")

(defvar easy-hugo--current-blog 0
  "Current blog number.")

(defcustom easy-hugo-bloglist nil
  "Multiple blog setting."
  :group 'easy-hugo
  :type 'string)

(push `((easy-hugo-basedir . ,easy-hugo-basedir)
	(easy-hugo-url . ,easy-hugo-url)
	(easy-hugo-root . ,easy-hugo-root)
	(easy-hugo-sshdomain . ,easy-hugo-sshdomain)
	(easy-hugo-amazon-s3-bucket-name . ,easy-hugo-amazon-s3-bucket-name)
	(easy-hugo-google-cloud-storage-bucket-name . ,easy-hugo-google-cloud-storage-bucket-name)
	(easy-hugo-github-deploy-script . ,easy-hugo-github-deploy-script)
	(easy-hugo-image-directory . ,easy-hugo-image-directory)
	(easy-hugo-default-picture-directory . ,easy-hugo-default-picture-directory)
	(easy-hugo-postdir . ,easy-hugo-postdir)
	(easy-hugo-publish-chmod . ,easy-hugo-publish-chmod)
	(easy-hugo-previewtime . ,easy-hugo-previewtime)
	(easy-hugo-preview-url . ,easy-hugo-preview-url)
	(easy-hugo-sort-default-char . ,easy-hugo-sort-default-char)
	(easy-hugo-asciidoc-extension . ,easy-hugo-asciidoc-extension)
	(easy-hugo-html-extension . ,easy-hugo-html-extension)
	(easy-hugo-markdown-extension . ,easy-hugo-markdown-extension)
	(easy-hugo-default-ext . ,easy-hugo-default-ext)
	(easy-hugo-postdir . ,easy-hugo-postdir))
      easy-hugo-bloglist)

(defconst easy-hugo--default-github-deploy-script easy-hugo-github-deploy-script
  "Default easy-hugo github-deploy-script.")

(defconst easy-hugo--default-image-directory easy-hugo-image-directory
  "Default easy-hugo image-directory.")

(defconst easy-hugo--default-picture-directory easy-hugo-default-picture-directory
  "Default easy-hugo picture-directory.")

(defconst easy-hugo--default-publish-chmod easy-hugo-publish-chmod
  "Default easy-hugo publish-chmod.")

(defconst easy-hugo--default-previewtime easy-hugo-previewtime
  "Default easy-hugo previewtime.")

(defconst easy-hugo--default-preview-url easy-hugo-preview-url
  "Default easy-hugo preview-url.")

(defconst easy-hugo--default-sort-default-char easy-hugo-sort-default-char
  "Default easy-hugo sort-default-char.")

(defconst easy-hugo--default-asciidoc-extension easy-hugo-asciidoc-extension
  "Default easy-hugo asciidoc-extension.")

(defconst easy-hugo--default-html-extension easy-hugo-html-extension
  "Default easy-hugo html-extension.")

(defconst easy-hugo--default-markdown-extension easy-hugo-markdown-extension
  "Default easy-hugo markdown-extension.")

(defconst easy-hugo--default-ext easy-hugo-default-ext
  "Default easy-hugo default-ext.")

(defconst easy-hugo--default-postdir easy-hugo-postdir
  "Default easy-hugo-postdir.")

(defconst easy-hugo--buffer-name "*Easy-hugo*"
  "Buffer name of easy-hugo.")

(defconst easy-hugo--forward-char 20
  "Forward-char of easy-hugo.")

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

(defmacro easy-hugo-set-bloglist (body)
  "Macros to set variables to `easy-hugo-bloglist' as BODY."
  `(setq ,body
	 (cdr (assoc ',body
		     (nth easy-hugo--current-blog easy-hugo-bloglist)))))

(defmacro easy-hugo-eval-bloglist (body)
  "Macros to eval variables of BODY from `easy-hugo-bloglist'."
  `(cdr (assoc ',body
	       (nth easy-hugo--current-blog easy-hugo-bloglist))))

;;;###autoload
(defun easy-hugo-article ()
  "Open a list of articles written in hugo with dired."
  (interactive)
  (unless easy-hugo-basedir
    (error "Please set easy-hugo-basedir variable"))
  (find-file (expand-file-name easy-hugo-postdir easy-hugo-basedir)))

;;;###autoload
(defun easy-hugo-image ()
  "Generate image link."
  (interactive
   (easy-hugo-with-env
    (unless (file-directory-p (expand-file-name easy-hugo-image-directory (expand-file-name "static" easy-hugo-basedir)))
      (error "%s does not exist" (expand-file-name easy-hugo-image-directory (expand-file-name "static" easy-hugo-basedir))))
    (let ((file (read-file-name "Image file: " nil
				(expand-file-name easy-hugo-image-directory
						  (expand-file-name "static" easy-hugo-basedir))
				t
				(expand-file-name easy-hugo-image-directory
						  (expand-file-name "static" easy-hugo-basedir)))))
      (insert (concat (format "<img src=\"%s%s\""
			      easy-hugo-url
			      (concat "/" easy-hugo-image-directory "/" (file-name-nondirectory file)))
		      " alt=\"\" width=\"100%\"/>"))))))

;;;###autoload
(defun easy-hugo-put-image ()
  "Move image to image directory and generate image link."
  (interactive
   (easy-hugo-with-env
    (unless (file-directory-p (expand-file-name easy-hugo-image-directory (expand-file-name "static" easy-hugo-basedir)))
      (error "%s does not exist" (expand-file-name easy-hugo-image-directory (expand-file-name "static" easy-hugo-basedir))))
    (let ((file (read-file-name "Image file: " nil
				(expand-file-name easy-hugo-default-picture-directory)
				t
				(expand-file-name easy-hugo-default-picture-directory))))
      (copy-file file (expand-file-name (file-name-nondirectory file) (expand-file-name easy-hugo-image-directory "static")))
      (insert (concat (format "<img src=\"%s%s\""
			      easy-hugo-url
			      (concat "/" easy-hugo-image-directory "/" (file-name-nondirectory file)))
		      " alt=\"\" width=\"100%\"/>"))))))

;;;###autoload
(defun easy-hugo-pull-image ()
  "Pull image from internet to image directory and generate image link."
  (interactive
   (easy-hugo-with-env
    (unless (file-directory-p (expand-file-name easy-hugo-image-directory (expand-file-name "static" easy-hugo-basedir)))
      (error "%s does not exist" (expand-file-name easy-hugo-image-directory (expand-file-name "static" easy-hugo-basedir))))
    (let ((url (read-string "URL: " (if (fboundp 'gui-get-selection) (gui-get-selection))))
	  (file (read-file-name "Save as: "
				(expand-file-name easy-hugo-image-directory (expand-file-name "static" easy-hugo-basedir))
				(car (last (split-string (substring-no-properties (gui-get-selection)) "/")))
				nil)))
      (when (file-exists-p (file-truename file))
	(error "%s already exists!" (file-truename file)))
      (url-copy-file url file t)
      (insert (concat (format "<img src=\"%s%s\""
			      easy-hugo-url
			      (concat "/" easy-hugo-image-directory "/" (file-name-nondirectory file)))
		      " alt=\"\" width=\"100%\"/>"))))))

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
   (let ((ret (call-process "hugo" nil "*hugo-publish*" t "--destination" "public")))
     (unless (zerop ret)
       (switch-to-buffer (get-buffer "*hugo-publish*"))
       (error "'hugo --destination public' command does not end normally")))
   (when (get-buffer "*hugo-publish*")
     (kill-buffer "*hugo-publish*"))
   (shell-command-to-string (concat "rsync -rtpl --chmod=" easy-hugo-publish-chmod " --delete public/ " easy-hugo-sshdomain ":" (shell-quote-argument easy-hugo-root)))
   (message "Blog published")
   (when easy-hugo-url
     (browse-url easy-hugo-url))))

;;;###autoload
(defun easy-hugo-publish-timer (n)
  "A timer that publish after the N number of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-hugo--basedir-timer easy-hugo-basedir)
  (setq easy-hugo--sshdomain-timer easy-hugo-sshdomain)
  (setq easy-hugo--root-timer easy-hugo-root)
  (setq easy-hugo--url-timer easy-hugo-url)
  (setq easy-hugo--publish-timer
	(run-at-time (* n 60) nil #'easy-hugo-publish-on-timer)))

;;;###autoload
(defun easy-hugo-cancel-publish-timer ()
  "Cancel timer that publish after the specified number of minutes has elapsed."
  (interactive)
  (if easy-hugo--publish-timer
      (progn
	(cancel-timer easy-hugo--publish-timer)
	(setq easy-hugo--publish-timer nil)
	(message "Publish-timer canceled"))
    (message "There is no reserved publish-timer")))

(defun easy-hugo-publish-on-timer ()
  "Adapt local change to the server with hugo on timer."
  (setq easy-hugo--publish-basedir easy-hugo-basedir)
  (setq easy-hugo-basedir easy-hugo--basedir-timer)
  (setq easy-hugo--publish-sshdomain easy-hugo-sshdomain)
  (setq easy-hugo-sshdomain easy-hugo--sshdomain-timer)
  (setq easy-hugo--publish-root easy-hugo-root)
  (setq easy-hugo-root easy-hugo--root-timer)
  (setq easy-hugo--publish-url easy-hugo-url)
  (setq easy-hugo-url easy-hugo--url-timer)
  (easy-hugo-publish)
  (setq easy-hugo-basedir easy-hugo--publish-basedir)
  (setq easy-hugo-sshdomain easy-hugo--publish-sshdomain)
  (setq easy-hugo-root easy-hugo--publish-root)
  (setq easy-hugo-url easy-hugo--publish-url))

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
POST-FILE needs to have and extension '.md' or '.org' or '.ad' or '.rst' or '.mmark' or '.html'."
  (interactive (list (read-from-minibuffer "Filename: " `(,easy-hugo-default-ext . 1) nil nil nil)))
  (easy-hugo-with-env
   (let ((filename (expand-file-name post-file easy-hugo-postdir))
	 (file-ext (file-name-extension post-file)))
     (when (not (member file-ext easy-hugo--formats))
       (error "Please enter .%s or .org or .%s or .rst or .mmark or .%s file name" easy-hugo-markdown-extension easy-hugo-asciidoc-extension easy-hugo-html-extension))
     (when (file-exists-p (file-truename filename))
       (error "%s already exists!" filename))
     (if (<= 0.25 (easy-hugo--version))
	 (call-process "hugo" nil "*hugo*" t "new" (file-relative-name filename (expand-file-name "content" easy-hugo-basedir)))
       (progn
	 (if (or (string-equal file-ext easy-hugo-markdown-extension)
		 (string-equal file-ext easy-hugo-asciidoc-extension)
		 (string-equal file-ext "rst")
		 (string-equal file-ext "mmark")
		 (string-equal file-ext easy-hugo-html-extension))
	     (call-process "hugo" nil "*hugo*" t "new" (file-relative-name filename (expand-file-name "content" easy-hugo-basedir))))))
     (when (get-buffer "*hugo*")
       (kill-buffer "*hugo*"))
     (find-file filename)
     (when (and (> 0.25 (easy-hugo--version))
		(string-equal file-ext "org"))
       (insert (easy-hugo--org-headers (file-name-base post-file))))
     (goto-char (point-max))
     (save-buffer))))

(defun easy-hugo--version ()
  "Return the version of hugo."
  (let ((source (split-string
		 (with-temp-buffer
		   (shell-command-to-string "hugo version"))
		 " ")))
    (string-to-number (substring (nth 4 source) 1))))

;;;###autoload
(defun easy-hugo-preview ()
  "Preview hugo at localhost."
  (interactive)
  (easy-hugo-with-env
   (if (process-live-p easy-hugo--server-process)
       (easy-hugo--preview-open)
     (progn
       (if (<= 0.25 (easy-hugo--version))
	   (setq easy-hugo--server-process
		 (start-process "hugo-server" easy-hugo--preview-buffer "hugo" "server" "--navigateToChanged"))
	 (setq easy-hugo--server-process
	       (start-process "hugo-server" easy-hugo--preview-buffer "hugo" "server")))
       (while easy-hugo--preview-loop
	 (if (equal (easy-hugo--preview-status) "200")
	     (progn
	       (setq easy-hugo--preview-loop nil)
	       (easy-hugo--preview-open)))
	 (sleep-for 0 100)
	 (if (and (eq (process-status easy-hugo--server-process) 'exit)
		  (equal (process-exit-status easy-hugo--server-process) 255))
	     (progn
	       (switch-to-buffer easy-hugo--preview-buffer)
	       (error "Hugo error look at %s buffer" easy-hugo--preview-buffer))))
       (setq easy-hugo--preview-loop t)
       (run-at-time easy-hugo-previewtime nil 'easy-hugo--preview-end)))))

(defun easy-hugo--preview-open ()
  "Open preview at the file name on the pointer.
If not applicable, return the default preview."
  (if (equal (buffer-name (current-buffer)) easy-hugo--buffer-name)
      (if (not (or (string-match "^$" (thing-at-point 'line))
		   (eq (point) (point-max))
		   (> (+ 1 easy-hugo--forward-char) (length (thing-at-point 'line)))))
	  (progn
	    (let ((file (expand-file-name
			 (substring (thing-at-point 'line) easy-hugo--forward-char -1)
			 easy-hugo-postdir)))
	      (when (and (file-exists-p file)
			 (not (file-directory-p file)))
		(if (equal (easy-hugo--preview-http-status-code
			    (file-name-sans-extension
			     (file-relative-name file (expand-file-name "content" easy-hugo-basedir))))
			   "200")
		    (browse-url (concat easy-hugo-preview-url
					(file-name-sans-extension
					 (file-relative-name file
							     (expand-file-name "content" easy-hugo-basedir)))))
		  (browse-url easy-hugo-preview-url)))))
	(browse-url easy-hugo-preview-url))
    (if buffer-file-name
	(if (equal (easy-hugo--preview-http-status-code
		    (file-name-sans-extension
		     (file-relative-name (file-truename buffer-file-name)
					 (expand-file-name "content" easy-hugo-basedir))))
		   "200")
	    (browse-url (concat easy-hugo-preview-url
				(file-name-sans-extension
				 (file-relative-name (file-truename buffer-file-name)
						     (expand-file-name "content" easy-hugo-basedir)))))
	  (browse-url easy-hugo-preview-url))
      (browse-url easy-hugo-preview-url))))

(defun easy-hugo--preview-http-status-code (url)
  "Return the http status code of the preview URL."
  (nth 1
       (split-string
	(nth 0
	     (split-string
	      (with-current-buffer (url-retrieve-synchronously (concat "http://127.0.0.1:1313/" url))
		(prog1
		    (buffer-string)
		  (kill-buffer)))
	      "\n"))
	" ")))

(defun easy-hugo--preview-status ()
  "Return the http status code of the preview."
  (nth 1
       (split-string
	(nth 0
	     (split-string
	      (with-current-buffer (url-retrieve-synchronously "http://127.0.0.1:1313/")
		(prog1
		    (buffer-string)
		  (kill-buffer)))
	      "\n"))
	" ")))

(defun easy-hugo--preview-end ()
  "Finish previewing hugo at localhost."
  (unless (null easy-hugo--server-process)
    (delete-process easy-hugo--server-process))
  (when (get-buffer easy-hugo--preview-buffer)
    (kill-buffer easy-hugo--preview-buffer)))

(defun easy-hugo--orgtime-format (x)
  "Format orgtime as X."
  (concat (substring x 0 3) ":" (substring x 3 5)))

;;;###autoload
(defun easy-hugo-github-deploy ()
  "Execute `easy-hugo-github-deploy-script' script locate at `easy-hugo-basedir'."
  (interactive)
  (easy-hugo-with-env
   (let ((deployscript (file-truename (expand-file-name easy-hugo-github-deploy-script easy-hugo-basedir))))
     (unless (executable-find deployscript)
       (error "%s do not execute" deployscript))
     (let ((ret (call-process (shell-quote-argument deployscript) nil "*hugo-github-deploy*" t)))
       (unless (zerop ret)
	 (switch-to-buffer (get-buffer "*hugo-github-deploy*"))
	 (error "%s command does not end normally" deployscript)))
     (when (get-buffer "*hugo-github-deploy*")
       (kill-buffer "*hugo-github-deploy*"))
     (message "Blog deployed")
     (when easy-hugo-url
       (browse-url easy-hugo-url)))))

;;;###autoload
(defun easy-hugo-github-deploy-timer (n)
  "A timer that github-deploy after the N number of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-hugo--github-deploy-basedir-timer easy-hugo-basedir)
  (setq easy-hugo--github-deploy-url-timer easy-hugo-url)
  (setq easy-hugo--github-deploy-timer
	(run-at-time (* n 60) nil #'easy-hugo-github-deploy-on-timer)))

;;;###autoload
(defun easy-hugo-cancel-github-deploy-timer ()
  "Cancel timer that github-deploy after the specified number of minutes has elapsed."
  (interactive)
  (if easy-hugo--github-deploy-timer
      (progn
	(cancel-timer easy-hugo--github-deploy-timer)
	(setq easy-hugo--github-deploy-timer nil)
	(message "Github-deploy-timer canceled"))
    (message "There is no reserved github-deploy-timer")))

(defun easy-hugo-github-deploy-on-timer ()
  "Execute `easy-hugo-github-deploy-script' script on timer locate at `easy-hugo-basedir'."
  (setq easy-hugo--github-deploy-basedir easy-hugo-basedir)
  (setq easy-hugo-basedir easy-hugo--github-deploy-basedir-timer)
  (setq easy-hugo--github-deploy-url easy-hugo-url)
  (setq easy-hugo-url easy-hugo--github-deploy-url-timer)
  (easy-hugo-github-deploy)
  (setq easy-hugo-basedir easy-hugo--github-deploy-basedir)
  (setq easy-hugo-url easy-hugo--github-deploy-url))

;;;###autoload
(defun easy-hugo-amazon-s3-deploy ()
  "Deploy hugo source at Amazon S3."
  (interactive)
  (easy-hugo-with-env
   (unless (executable-find "aws")
     (error "'aws' is not installed"))
   (unless easy-hugo-amazon-s3-bucket-name
     (error "Please set 'easy-hugo-amazon-s3-bucket-name' variable"))
   (when (file-directory-p "public")
     (delete-directory "public" t nil))
   (let ((ret (call-process "hugo" nil "*hugo-amazon-s3-deploy*" t "--destination" "public")))
     (unless (zerop ret)
       (switch-to-buffer (get-buffer "*hugo-amazon-s3-deploy*"))
       (error "'hugo --destination public' command does not end normally")))
   (when (get-buffer "*hugo-amazon-s3-deploy*")
     (kill-buffer "*hugo-amazon-s3-deploy*"))
   (shell-command-to-string (concat "aws s3 sync --delete public s3://" easy-hugo-amazon-s3-bucket-name "/"))
   (message "Blog deployed")
   (when easy-hugo-url
     (browse-url easy-hugo-url))))

;;;###autoload
(defun easy-hugo-amazon-s3-deploy-timer (n)
  "A timer that amazon-s3-deploy after the N number of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-hugo--amazon-s3-basedir-timer easy-hugo-basedir)
  (setq easy-hugo--amazon-s3-url-timer easy-hugo-url)
  (setq easy-hugo--amazon-s3-bucket-name-timer easy-hugo-amazon-s3-bucket-name)
  (setq easy-hugo--amazon-s3-timer
	(run-at-time (* n 60) nil #'easy-hugo-amazon-s3-deploy-on-timer)))

;;;###autoload
(defun easy-hugo-cancel-amazon-s3-deploy-timer ()
  "Cancel timer that amazon-s3-deploy after the specified number of minutes has elapsed."
  (interactive)
  (if easy-hugo--amazon-s3-timer
      (progn
	(cancel-timer easy-hugo--amazon-s3-timer)
	(setq easy-hugo--amazon-s3-timer nil)
	(message "AWS-s3-deploy-timer canceled"))
    (message "There is no reserved AWS-s3-deploy-timer")))

(defun easy-hugo-amazon-s3-deploy-on-timer ()
  "Deploy hugo source at Amazon S3 on timer."
  (setq easy-hugo--amazon-s3-basedir easy-hugo-basedir)
  (setq easy-hugo-basedir easy-hugo--amazon-s3-basedir-timer)
  (setq easy-hugo--amazon-s3-url easy-hugo-url)
  (setq easy-hugo-url easy-hugo--amazon-s3-url-timer)
  (setq easy-hugo--amazon-s3-bucket-name easy-hugo-amazon-s3-bucket-name)
  (setq easy-hugo-amazon-s3-bucket-name easy-hugo--amazon-s3-bucket-name-timer)
  (easy-hugo-amazon-s3-deploy)
  (setq easy-hugo-basedir easy-hugo--amazon-s3-basedir)
  (setq easy-hugo-url easy-hugo--amazon-s3-url)
  (setq easy-hugo-amazon-s3-bucket-name easy-hugo--amazon-s3-bucket-name))

;;;###autoload
(defun easy-hugo-google-cloud-storage-deploy ()
  "Deploy hugo source at Google Cloud Storage."
  (interactive)
  (easy-hugo-with-env
   (unless (executable-find "gsutil")
     (error "'Google Cloud SDK' is not installed"))
   (unless easy-hugo-google-cloud-storage-bucket-name
     (error "Please set 'easy-hugo-google-cloud-storage-bucket-name' variable"))
   (when (file-directory-p "public")
     (delete-directory "public" t nil))
   (let ((ret (call-process "hugo" nil "*hugo-google-cloud-storage-deploy*" t "--destination" "public")))
     (unless (zerop ret)
       (switch-to-buffer (get-buffer "*hugo-google-cloud-storage-deploy*"))
       (error "'hugo --destination public' command does not end normally")))
   (when (get-buffer "*hugo-google-cloud-storage-deploy*")
     (kill-buffer "*hugo-google-cloud-storage-deploy*"))
   (shell-command-to-string (concat "gsutil -m rsync -d -r public gs://" easy-hugo-google-cloud-storage-bucket-name "/"))
   (message "Blog deployed")
   (when easy-hugo-url
     (browse-url easy-hugo-url))))

;;;###autoload
(defun easy-hugo-google-cloud-storage-deploy-timer (n)
  "A timer that google-cloud-storage-deploy after the N number of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-hugo--google-cloud-storage-basedir-timer easy-hugo-basedir)
  (setq easy-hugo--google-cloud-storage-url-timer easy-hugo-url)
  (setq easy-hugo--google-cloud-storage-bucket-name-timer easy-hugo-google-cloud-storage-bucket-name)
  (setq easy-hugo--google-cloud-storage-timer
	(run-at-time (* n 60) nil #'easy-hugo-google-cloud-storage-deploy-on-timer)))

;;;###autoload
(defun easy-hugo-cancel-google-cloud-storage-deploy-timer ()
  "Cancel timer that google-cloud-storage-deploy after the specified number of minutes has elapsed."
  (interactive)
  (if easy-hugo--google-cloud-storage-timer
      (progn
	(cancel-timer easy-hugo--google-cloud-storage-timer)
	(setq easy-hugo--google-cloud-storage-timer nil)
	(message "GCS-timer canceled"))
    (message "There is no reserved GCS-timer")))

(defun easy-hugo-google-cloud-storage-deploy-on-timer ()
  "Deploy hugo source at Google Cloud Storage on timer."
  (setq easy-hugo--google-cloud-storage-basedir easy-hugo-basedir)
  (setq easy-hugo-basedir easy-hugo--google-cloud-storage-basedir-timer)
  (setq easy-hugo--google-cloud-storage-url easy-hugo-url)
  (setq easy-hugo-url easy-hugo--google-cloud-storage-url-timer)
  (setq easy-hugo--google-cloud-storage-bucket-name easy-hugo-google-cloud-storage-bucket-name)
  (setq easy-hugo-google-cloud-storage-bucket-name easy-hugo--google-cloud-storage-bucket-name-timer)
  (easy-hugo-google-cloud-storage-deploy)
  (setq easy-hugo-basedir easy-hugo--google-cloud-storage-basedir)
  (setq easy-hugo-url easy-hugo--google-cloud-storage-url)
  (setq easy-hugo-google-cloud-storage-bucket-name easy-hugo--google-cloud-storage-bucket-name))

;;;###autoload
(defun easy-hugo-helm-ag ()
  "Search for blog article with helm-ag."
  (interactive)
  (easy-hugo-with-env
   (if (package-installed-p 'helm-ag)
       (helm-ag (expand-file-name easy-hugo-postdir easy-hugo-basedir))
     (error "'helm-ag' is not installed"))))

;;;###autoload
(defun easy-hugo-open-config ()
  "Open Hugo's config file."
  (interactive)
  (easy-hugo-with-env
   (cond ((file-exists-p (expand-file-name "config.toml" easy-hugo-basedir))
	  (find-file (expand-file-name "config.toml" easy-hugo-basedir)))
	 ((file-exists-p (expand-file-name "config.yaml" easy-hugo-basedir))
	  (find-file (expand-file-name "config.yaml" easy-hugo-basedir)))
	 ((file-exists-p (expand-file-name "config.json" easy-hugo-basedir))
	  (find-file (expand-file-name "config.json" easy-hugo-basedir)))
	 (t (error "Hugo config file not found at %s" easy-hugo-basedir)))))

(defcustom easy-hugo-help
  (if (null easy-hugo-sort-default-char)
      (progn
	"n .. New blog post    R .. Rename file   G .. Deploy GitHub    D .. Draft list
p .. Preview          g .. Refresh       A .. Deploy AWS S3    u .. Undraft file
v .. Open view-mode   s .. Sort time     T .. Publish timer    N .. No help-mode
d .. Delete post      c .. Open config   W .. AWS S3 timer     I .. GCS timer
P .. Publish server   C .. Deploy GCS    a .. Search helm-ag   H .. GitHub timer
< .. Previous blog    > .. Next blog     , .. Pre postdir      . .. Next postdir
F .. Full help [tab]  S .. Sort char     ? .. Describe-mode    q .. Quit easy-hugo
")
    (progn
      "n .. New blog post    R .. Rename file   G .. Deploy GitHub    D .. Draft list
p .. Preview          g .. Refresh       A .. Deploy AWS S3    s .. Sort char
v .. Open view-mode   u .. Undraft file  T .. Publish timer    N .. No help-mode
d .. Delete post      c .. Open config   S .. Sort time        I .. GCS timer
P .. Publish server   C .. Deploy GCS    a .. Search helm-ag   H .. GitHub timer
< .. Previous blog    > .. Next blog     , .. Pre postdir      . .. Next postdir
F .. Full help [tab]  W .. AWS S3 timer  ? .. Describe-mode    q .. Quit easy-hugo
"))
  "Help of easy-hugo."
  :group 'easy-hugo
  :type 'string)

(defconst easy-hugo--first-help
  "Welcome to Easy-hugo

Let's post an article first.
Press n on this screen or M-x easy-hugo-newpost.
Enter a article file name in the minibuffer.
Then M-x easy-hugo again or refresh the screen with r or g key in this buffer,
article which you wrote should appear here.
Enjoy!

"
  "Help of easy-hugo first time.")

(defcustom easy-hugo-add-help
  "O .. Open basedir     r .. Refresh       b .. X github timer   t .. X publish-timer
m .. X s3-timer       i .. X GCS timer   f .. File open        J .. Jump blog-number
k .. Previous-line    j .. Next line     h .. backward-char    l .. forward-char
w .. Write post       o .. Open file     - .. Pre postdir      + .. Next postdir
"
  "Add help of easy-hugo."
  :group 'easy-hugo
  :type 'string)

(defvar easy-hugo-mode-map
  (let ((map (make-keymap)))
    (define-key map "." 'easy-hugo-next-postdir)
    (define-key map "," 'easy-hugo-previous-postdir)
    (define-key map "+" 'easy-hugo-next-postdir)
    (define-key map "-" 'easy-hugo-previous-postdir)
    (define-key map "n" 'easy-hugo-newpost)
    (define-key map "w" 'easy-hugo-newpost)
    (define-key map "a" 'easy-hugo-helm-ag)
    (define-key map "c" 'easy-hugo-open-config)
    (define-key map "p" 'easy-hugo-preview)
    (define-key map "P" 'easy-hugo-publish)
    (define-key map "T" 'easy-hugo-publish-timer)
    (define-key map "W" 'easy-hugo-amazon-s3-deploy-timer)
    (define-key map "t" 'easy-hugo-cancel-publish-timer)
    (define-key map "o" 'easy-hugo-open)
    (define-key map "O" 'easy-hugo-open-basedir)
    (define-key map "R" 'easy-hugo-rename)
    (define-key map "\C-m" 'easy-hugo-open)
    (put 'easy-hugo-open :advertised-binding "\C-m")
    (define-key map "d" 'easy-hugo-delete)
    (define-key map "e" 'easy-hugo-open)
    (define-key map "f" 'easy-hugo-open)
    (define-key map "F" 'easy-hugo-full-help)
    (define-key map [tab] 'easy-hugo-full-help)
    (define-key map [backtab] 'easy-hugo-no-help)
    (define-key map "N" 'easy-hugo-no-help)
    (define-key map "J" 'easy-hugo-nth-blog)
    (define-key map "j" 'easy-hugo-next-line)
    (define-key map "k" 'easy-hugo-previous-line)
    (define-key map "h" 'easy-hugo-backward-char)
    (define-key map "l" 'easy-hugo-forward-char)
    (define-key map " " 'easy-hugo-next-line)
    (define-key map [?\S-\ ] 'easy-hugo-previous-line)
    (define-key map [remap next-line] 'easy-hugo-next-line)
    (define-key map [remap previous-line] 'easy-hugo-previous-line)
    (define-key map [remap forward-char] 'easy-hugo-forward-char)
    (define-key map [remap backward-char] 'easy-hugo-backward-char)
    (define-key map [remap beginning-of-buffer] 'easy-hugo-beginning-of-buffer)
    (define-key map [remap backward-word] 'easy-hugo-backward-word)
    (define-key map [right] 'easy-hugo-forward-char)
    (define-key map [left] 'easy-hugo-backward-char)
    (define-key map "v" 'easy-hugo-view)
    (define-key map "r" 'easy-hugo-refresh)
    (define-key map "g" 'easy-hugo-refresh)
    (if (null easy-hugo-sort-default-char)
	(progn
	  (define-key map "s" 'easy-hugo-sort-time)
	  (define-key map "S" 'easy-hugo-sort-char))
      (progn
	(define-key map "S" 'easy-hugo-sort-time)
	(define-key map "s" 'easy-hugo-sort-char)))
    (define-key map "G" 'easy-hugo-github-deploy)
    (define-key map "H" 'easy-hugo-github-deploy-timer)
    (define-key map "b" 'easy-hugo-cancel-github-deploy-timer)
    (define-key map "A" 'easy-hugo-amazon-s3-deploy)
    (define-key map "m" 'easy-hugo-cancel-amazon-s3-deploy-timer)
    (define-key map "C" 'easy-hugo-google-cloud-storage-deploy)
    (define-key map "I" 'easy-hugo-google-cloud-storage-deploy-timer)
    (define-key map "i" 'easy-hugo-cancel-google-cloud-storage-deploy-timer)
    (define-key map "D" 'easy-hugo-list-draft)
    (define-key map "u" 'easy-hugo-undraft)
    (define-key map "q" 'easy-hugo-quit)
    (define-key map "<" 'easy-hugo-previous-blog)
    (define-key map ">" 'easy-hugo-next-blog)
    map)
  "Keymap for easy-hugo major mode.")

(define-derived-mode easy-hugo-mode special-mode "Easy-hugo"
  "Major mode for easy hugo.")

(defun easy-hugo-quit ()
  "Quit easy hugo."
  (interactive)
  (setq easy-hugo--sort-time-flg 1)
  (setq easy-hugo--sort-char-flg nil)
  (easy-hugo--preview-end)
  (when (buffer-live-p easy-hugo--mode-buffer)
    (kill-buffer easy-hugo--mode-buffer)))

(defun easy-hugo-no-help ()
  "No help easy hugo."
  (interactive)
  (if easy-hugo-no-help
      (progn
	(setq easy-hugo-no-help nil)
	(setq easy-hugo--unmovable-line easy-hugo--unmovable-line-default))
    (progn
      (setq easy-hugo-no-help 1)
      (setq easy-hugo-additional-help nil)
      (setq easy-hugo--unmovable-line 3)))
  (if easy-hugo--draft-list
      (easy-hugo-draft-list)
    (easy-hugo)))

(defun easy-hugo-full-help ()
  "Full help mode of easy hugo."
  (interactive)
  (if easy-hugo-additional-help
      (progn
	(setq easy-hugo-additional-help nil)
	(setq easy-hugo--unmovable-line easy-hugo--unmovable-line-default))
    (progn
      (setq easy-hugo-additional-help 1)
      (setq easy-hugo-no-help nil)
      (setq easy-hugo--unmovable-line (+ easy-hugo-help-line easy-hugo-add-help-line 4))))
  (if easy-hugo--draft-list
      (easy-hugo-draft-list)
    (easy-hugo)))

(defun easy-hugo-list-draft ()
  "List drafts."
  (interactive)
  (if easy-hugo--draft-list
      (progn
	(setq easy-hugo--draft-list nil)
	(setq easy-hugo--draft-mode nil)
	(easy-hugo))
    (progn
      (setq easy-hugo--draft-list 1)
      (setq easy-hugo--draft-mode "  Draft")
      (easy-hugo-draft-list))))

(defun easy-hugo-refresh ()
  "Refresh easy-hugo-mode."
  (interactive)
  (setq easy-hugo--cursor (point))
  (setq easy-hugo--refresh 1)
  (if easy-hugo--draft-list
      (easy-hugo-draft-list)
    (easy-hugo))
  (setq easy-hugo--refresh nil))

(defun easy-hugo-sort-time ()
  "Sort article by time on easy-hugo-mode."
  (interactive)
  (if easy-hugo--draft-list
      (progn
	(setq easy-hugo--sort-char-flg nil)
	(if (eq 1 easy-hugo--sort-time-flg)
	    (setq easy-hugo--sort-time-flg 2)
	  (setq easy-hugo--sort-time-flg 1))
	(easy-hugo-draft-list))
    (progn
      (setq easy-hugo--sort-char-flg nil)
      (if (eq 1 easy-hugo--sort-time-flg)
	  (setq easy-hugo--sort-time-flg 2)
	(setq easy-hugo--sort-time-flg 1))
      (easy-hugo))))

(defun easy-hugo-sort-char ()
  "Sort article by characters on easy-hugo-mode."
  (interactive)
  (if easy-hugo--draft-list
      (progn
	(setq easy-hugo--sort-time-flg nil)
	(if (eq 1 easy-hugo--sort-char-flg)
	    (setq easy-hugo--sort-char-flg 2)
	  (setq easy-hugo--sort-char-flg 1))
	(easy-hugo-draft-list))
    (progn
      (setq easy-hugo--sort-time-flg nil)
      (if (eq 1 easy-hugo--sort-char-flg)
	  (setq easy-hugo--sort-char-flg 2)
	(setq easy-hugo--sort-char-flg 1))
      (easy-hugo))))

(defun easy-hugo-forward-char (arg)
  "Forward-char on easy-hugo-mode.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "^p")
  (when (not (eolp))
    (forward-char (or arg 1))))

(defun easy-hugo-backward-char (arg)
  "Backward-char on easy-hugo-mode.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "^p")
  (when (not (bolp))
    (backward-char (or arg 1))))

(defun easy-hugo-beginning-of-buffer ()
  "Easy-hugo beginning-of-buffer."
  (interactive)
  (goto-char (point-min))
  (forward-line (- easy-hugo--unmovable-line 1))
  (forward-char easy-hugo--forward-char))

(defun easy-hugo-backward-word (&optional arg)
  "Easy-hugo backward-word.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "^p")
  (forward-word (- (or arg 1)))
  (if (< (line-number-at-pos) easy-hugo--unmovable-line)
      (progn
	(goto-char (point-min))
	(forward-line (- easy-hugo--unmovable-line 1)))))

(defun easy-hugo-next-line (arg)
  "Move down lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "^p")
  (let ((line-move-visual)
	(goal-column))
    (line-move arg t))
  (while (and (invisible-p (point))
	      (not (if (and arg (< arg 0)) (bobp) (eobp))))
    (forward-char (if (and arg (< arg 0)) -1 1)))
  (beginning-of-line)
  (forward-char easy-hugo--forward-char))

(defun easy-hugo-previous-line (arg)
  "Move up lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "^p")
  (when (>= (- (line-number-at-pos) arg) easy-hugo--unmovable-line)
    (easy-hugo-next-line (- (or arg 1)))))

(defun easy-hugo-rename (post-file)
  "Renames file on the pointer to POST-FILE."
  (interactive (list (read-from-minibuffer "Rename: " `(,easy-hugo-default-ext . 1) nil nil nil)))
  (easy-hugo-with-env
   (let ((newname (expand-file-name post-file easy-hugo-postdir))
	 (file-ext (file-name-extension post-file)))
     (when (not (member file-ext easy-hugo--formats))
       (error "Please enter .%s or .org or .%s or .rst or .mmark or .%s file name" easy-hugo-markdown-extension easy-hugo-asciidoc-extension easy-hugo-html-extension))
     (when (equal (buffer-name (current-buffer)) easy-hugo--buffer-name)
       (when (file-exists-p (file-truename newname))
	 (error "%s already exists!" newname))
       (unless (or (string-match "^$" (thing-at-point 'line))
		   (eq (point) (point-max))
		   (> (+ 1 easy-hugo--forward-char) (length (thing-at-point 'line))))
	 (let ((oldname (expand-file-name
			 (substring (thing-at-point 'line) easy-hugo--forward-char -1)
			 easy-hugo-postdir)))
	   (rename-file oldname newname 1)
	   (easy-hugo-refresh)))))))

(defun easy-hugo-undraft ()
  "Undraft file on the pointer."
  (interactive)
  (when (equal (buffer-name (current-buffer)) easy-hugo--buffer-name)
    (easy-hugo-with-env
     (when (> 0.25 (easy-hugo--version))
       (error "'easy-hugo-undraft' requires hugo 0.25 or higher"))
     (unless (or (string-match "^$" (thing-at-point 'line))
		 (eq (point) (point-max))
		 (> (+ 1 easy-hugo--forward-char) (length (thing-at-point 'line))))
       (let ((file (expand-file-name
		    (substring (thing-at-point 'line) easy-hugo--forward-char -1)
		    easy-hugo-postdir)))
	 (when (and (file-exists-p file)
		    (not (file-directory-p file)))
	   (shell-command-to-string (concat "hugo undraft " file))
	   (easy-hugo-refresh)))))))

(defun easy-hugo-open ()
  "Open the file on the pointer."
  (interactive)
  (when (equal (buffer-name (current-buffer)) easy-hugo--buffer-name)
    (easy-hugo-with-env
     (unless (or (string-match "^$" (thing-at-point 'line))
		 (eq (point) (point-max))
		 (> (+ 1 easy-hugo--forward-char) (length (thing-at-point 'line))))
       (let ((file (expand-file-name
		    (substring (thing-at-point 'line) easy-hugo--forward-char -1)
		    easy-hugo-postdir)))
	 (when (and (file-exists-p file)
		    (not (file-directory-p file)))
	   (find-file file)))))))

(defun easy-hugo-open-basedir ()
  "Open `easy-hugo-basedir' with dired."
  (interactive)
  (easy-hugo-with-env
   (switch-to-buffer (find-file-noselect easy-hugo-basedir))))

(defun easy-hugo-view ()
  "Open the file on the pointer with 'view-mode'."
  (interactive)
  (easy-hugo-with-env
   (if (equal (buffer-name (current-buffer)) easy-hugo--buffer-name)
       (progn
	 (unless (or (string-match "^$" (thing-at-point 'line))
		     (eq (point) (point-max))
		     (> (+ 1 easy-hugo--forward-char) (length (thing-at-point 'line))))
	   (let ((file (expand-file-name
			(substring (thing-at-point 'line) easy-hugo--forward-char -1)
			easy-hugo-postdir)))
	     (when (and (file-exists-p file)
			(not (file-directory-p file)))
	       (view-file file)))))
     (view-file buffer-file-name))))

(defun easy-hugo-delete ()
  "Delete the file on the pointer."
  (interactive)
  (when (equal (buffer-name (current-buffer)) easy-hugo--buffer-name)
    (easy-hugo-with-env
     (unless (or (string-match "^$" (thing-at-point 'line))
		 (eq (point) (point-max))
		 (> (+ 1 easy-hugo--forward-char) (length (thing-at-point 'line))))
       (let ((file (expand-file-name
		    (substring (thing-at-point 'line) easy-hugo--forward-char -1)
		    easy-hugo-postdir)))
	 (when (and (file-exists-p file)
		    (not (file-directory-p file)))
	   (when (y-or-n-p (concat "Delete " file))
	     (if easy-hugo-no-help
		 (setq easy-hugo--line (- (line-number-at-pos) 4))
	       (setq easy-hugo--line (- (line-number-at-pos) (+ easy-hugo--unmovable-line 1))))
	     (delete-file file)
	     (if easy-hugo--draft-list
		 (easy-hugo-draft-list)
	       (easy-hugo))
	     (when (> easy-hugo--line 0)
	       (forward-line easy-hugo--line)
	       (forward-char easy-hugo--forward-char)))))))))

(defun easy-hugo-next-blog ()
  "Go to next blog."
  (interactive)
  (when (< 1 (length easy-hugo-bloglist))
    (if (eq (- (length easy-hugo-bloglist) 1) easy-hugo--current-blog)
	(setq easy-hugo--current-blog 0)
      (setq easy-hugo--current-blog (+ easy-hugo--current-blog 1)))
    (setq easy-hugo--current-postdir 0)
    (easy-hugo-set-bloglist easy-hugo-basedir)
    (easy-hugo-set-bloglist easy-hugo-url)
    (easy-hugo-set-bloglist easy-hugo-root)
    (easy-hugo-set-bloglist easy-hugo-sshdomain)
    (easy-hugo-set-bloglist easy-hugo-amazon-s3-bucket-name)
    (easy-hugo-set-bloglist easy-hugo-google-cloud-storage-bucket-name)
    (if (easy-hugo-eval-bloglist easy-hugo-github-deploy-script)
	(easy-hugo-set-bloglist easy-hugo-github-deploy-script)
      (setq easy-hugo-github-deploy-script easy-hugo--default-github-deploy-script))
    (if (easy-hugo-eval-bloglist easy-hugo-image-directory)
	(easy-hugo-set-bloglist easy-hugo-image-directory)
      (setq easy-hugo-image-directory easy-hugo--default-image-directory))
    (if (easy-hugo-eval-bloglist easy-hugo-default-picture-directory)
	(easy-hugo-set-bloglist easy-hugo-default-picture-directory)
      (setq easy-hugo-default-picture-directory easy-hugo--default-picture-directory))
    (if (easy-hugo-eval-bloglist easy-hugo-preview-url)
	(easy-hugo-set-bloglist easy-hugo-preview-url)
      (setq easy-hugo-preview-url easy-hugo--default-preview-url))
    (if (easy-hugo-eval-bloglist easy-hugo-publish-chmod)
	(easy-hugo-set-bloglist easy-hugo-publish-chmod)
      (setq easy-hugo-publish-chmod easy-hugo--default-publish-chmod))
    (if (easy-hugo-eval-bloglist easy-hugo-previewtime)
	(easy-hugo-set-bloglist easy-hugo-previewtime)
      (setq easy-hugo-previewtime easy-hugo--default-previewtime))
    (if (easy-hugo-eval-bloglist easy-hugo-sort-default-char)
	(easy-hugo-set-bloglist easy-hugo-sort-default-char)
      (setq easy-hugo-sort-default-char easy-hugo--default-sort-default-char))
    (if (easy-hugo-eval-bloglist easy-hugo-asciidoc-extension)
	(easy-hugo-set-bloglist easy-hugo-asciidoc-extension)
      (setq easy-hugo-asciidoc-extension easy-hugo--default-asciidoc-extension))
    (if (easy-hugo-eval-bloglist easy-hugo-html-extension)
	(easy-hugo-set-bloglist easy-hugo-html-extension)
      (setq easy-hugo-html-extension easy-hugo--default-html-extension))
    (if (easy-hugo-eval-bloglist easy-hugo-markdown-extension)
	(easy-hugo-set-bloglist easy-hugo-markdown-extension)
      (setq easy-hugo-markdown-extension easy-hugo-markdown-extension))
    (if (easy-hugo-eval-bloglist easy-hugo-default-ext)
	(easy-hugo-set-bloglist easy-hugo-default-ext)
      (setq easy-hugo-default-ext easy-hugo--default-ext))
    (if (easy-hugo-eval-bloglist easy-hugo-postdir)
	(easy-hugo-set-bloglist easy-hugo-postdir)
      (setq easy-hugo-postdir easy-hugo--default-postdir))
    (easy-hugo--preview-end)
    (easy-hugo)))

(defun easy-hugo-previous-blog ()
  "Go to previous blog."
  (interactive)
  (when (< 1 (length easy-hugo-bloglist))
    (if (= 0 easy-hugo--current-blog)
	(setq easy-hugo--current-blog (- (length easy-hugo-bloglist) 1))
      (setq easy-hugo--current-blog (- easy-hugo--current-blog 1)))
    (setq easy-hugo--current-postdir 0)
    (easy-hugo-set-bloglist easy-hugo-basedir)
    (easy-hugo-set-bloglist easy-hugo-url)
    (easy-hugo-set-bloglist easy-hugo-root)
    (easy-hugo-set-bloglist easy-hugo-sshdomain)
    (easy-hugo-set-bloglist easy-hugo-amazon-s3-bucket-name)
    (easy-hugo-set-bloglist easy-hugo-google-cloud-storage-bucket-name)
    (if (easy-hugo-eval-bloglist easy-hugo-github-deploy-script)
	(easy-hugo-set-bloglist easy-hugo-github-deploy-script)
      (setq easy-hugo-github-deploy-script easy-hugo--default-github-deploy-script))
    (if (easy-hugo-eval-bloglist easy-hugo-image-directory)
	(easy-hugo-set-bloglist easy-hugo-image-directory)
      (setq easy-hugo-image-directory easy-hugo--default-image-directory))
    (if (easy-hugo-eval-bloglist easy-hugo-default-picture-directory)
	(easy-hugo-set-bloglist easy-hugo-default-picture-directory)
      (setq easy-hugo-default-picture-directory easy-hugo--default-picture-directory))
    (if (easy-hugo-eval-bloglist easy-hugo-preview-url)
	(easy-hugo-set-bloglist easy-hugo-preview-url)
      (setq easy-hugo-preview-url easy-hugo--default-preview-url))
    (if (easy-hugo-eval-bloglist easy-hugo-publish-chmod)
	(easy-hugo-set-bloglist easy-hugo-publish-chmod)
      (setq easy-hugo-publish-chmod easy-hugo--default-publish-chmod))
    (if (easy-hugo-eval-bloglist easy-hugo-previewtime)
	(easy-hugo-set-bloglist easy-hugo-previewtime)
      (setq easy-hugo-previewtime easy-hugo--default-previewtime))
    (if (easy-hugo-eval-bloglist easy-hugo-sort-default-char)
	(easy-hugo-set-bloglist easy-hugo-sort-default-char)
      (setq easy-hugo-sort-default-char easy-hugo--default-sort-default-char))
    (if (easy-hugo-eval-bloglist easy-hugo-asciidoc-extension)
	(easy-hugo-set-bloglist easy-hugo-asciidoc-extension)
      (setq easy-hugo-asciidoc-extension easy-hugo--default-asciidoc-extension))
    (if (easy-hugo-eval-bloglist easy-hugo-html-extension)
	(easy-hugo-set-bloglist easy-hugo-html-extension)
      (setq easy-hugo-html-extension easy-hugo--default-html-extension))
    (if (easy-hugo-eval-bloglist easy-hugo-markdown-extension)
	(easy-hugo-set-bloglist easy-hugo-markdown-extension)
      (setq easy-hugo-markdown-extension easy-hugo-markdown-extension))
    (if (easy-hugo-eval-bloglist easy-hugo-default-ext)
	(easy-hugo-set-bloglist easy-hugo-default-ext)
      (setq easy-hugo-default-ext easy-hugo--default-ext))
    (if (easy-hugo-eval-bloglist easy-hugo-postdir)
	(easy-hugo-set-bloglist easy-hugo-postdir)
      (setq easy-hugo-postdir easy-hugo--default-postdir))
    (easy-hugo--preview-end)
    (easy-hugo)))

(defun easy-hugo-nth-blog (n)
  "Go to blog of number N."
  (interactive "nBlog number:")
  (when (or (< n 0)
	    (>= n (length easy-hugo-bloglist)))
    (error "Blog %s does not exist" n))
  (when (and (< 1 (length easy-hugo-bloglist))
	     (< n (length easy-hugo-bloglist)))
    (setq easy-hugo--current-blog n)
    (setq easy-hugo--current-postdir 0)
    (easy-hugo-set-bloglist easy-hugo-basedir)
    (easy-hugo-set-bloglist easy-hugo-url)
    (easy-hugo-set-bloglist easy-hugo-root)
    (easy-hugo-set-bloglist easy-hugo-sshdomain)
    (easy-hugo-set-bloglist easy-hugo-amazon-s3-bucket-name)
    (easy-hugo-set-bloglist easy-hugo-google-cloud-storage-bucket-name)
    (if (easy-hugo-eval-bloglist easy-hugo-github-deploy-script)
	(easy-hugo-set-bloglist easy-hugo-github-deploy-script)
      (setq easy-hugo-github-deploy-script easy-hugo--default-github-deploy-script))
    (if (easy-hugo-eval-bloglist easy-hugo-image-directory)
	(easy-hugo-set-bloglist easy-hugo-image-directory)
      (setq easy-hugo-image-directory easy-hugo--default-image-directory))
    (if (easy-hugo-eval-bloglist easy-hugo-default-picture-directory)
	(easy-hugo-set-bloglist easy-hugo-default-picture-directory)
      (setq easy-hugo-default-picture-directory easy-hugo--default-picture-directory))
    (if (easy-hugo-eval-bloglist easy-hugo-preview-url)
	(easy-hugo-set-bloglist easy-hugo-preview-url)
      (setq easy-hugo-preview-url easy-hugo--default-preview-url))
    (if (easy-hugo-eval-bloglist easy-hugo-publish-chmod)
	(easy-hugo-set-bloglist easy-hugo-publish-chmod)
      (setq easy-hugo-publish-chmod easy-hugo--default-publish-chmod))
    (if (easy-hugo-eval-bloglist easy-hugo-previewtime)
	(easy-hugo-set-bloglist easy-hugo-previewtime)
      (setq easy-hugo-previewtime easy-hugo--default-previewtime))
    (if (easy-hugo-eval-bloglist easy-hugo-sort-default-char)
	(easy-hugo-set-bloglist easy-hugo-sort-default-char)
      (setq easy-hugo-sort-default-char easy-hugo--default-sort-default-char))
    (if (easy-hugo-eval-bloglist easy-hugo-asciidoc-extension)
	(easy-hugo-set-bloglist easy-hugo-asciidoc-extension)
      (setq easy-hugo-asciidoc-extension easy-hugo--default-asciidoc-extension))
    (if (easy-hugo-eval-bloglist easy-hugo-html-extension)
	(easy-hugo-set-bloglist easy-hugo-html-extension)
      (setq easy-hugo-html-extension easy-hugo--default-html-extension))
    (if (easy-hugo-eval-bloglist easy-hugo-markdown-extension)
	(easy-hugo-set-bloglist easy-hugo-markdown-extension)
      (setq easy-hugo-markdown-extension easy-hugo-markdown-extension))
    (if (easy-hugo-eval-bloglist easy-hugo-default-ext)
	(easy-hugo-set-bloglist easy-hugo-default-ext)
      (setq easy-hugo-default-ext easy-hugo--default-ext))
    (if (easy-hugo-eval-bloglist easy-hugo-postdir)
	(easy-hugo-set-bloglist easy-hugo-postdir)
      (setq easy-hugo-postdir easy-hugo--default-postdir))
    (easy-hugo--preview-end)
    (easy-hugo)))

(defun easy-hugo-next-postdir ()
  "Go to next postdir."
  (interactive)
  (setq easy-hugo--postdir-list (easy-hugo--directory-list (easy-hugo--directory-files-recursively (expand-file-name "content" easy-hugo-basedir) "" t)))
  (setq easy-hugo--postdir-list (delete (expand-file-name easy-hugo-postdir easy-hugo-basedir) easy-hugo--postdir-list))
  (add-to-list 'easy-hugo--postdir-list (expand-file-name "content" easy-hugo-basedir) t)
  (add-to-list 'easy-hugo--postdir-list (expand-file-name easy-hugo-postdir easy-hugo-basedir))
  (if (eq (- (length easy-hugo--postdir-list) 1) easy-hugo--current-postdir)
      (setq easy-hugo--current-postdir 0)
    (setq easy-hugo--current-postdir (+ easy-hugo--current-postdir 1)))
  (setq easy-hugo-postdir (file-relative-name (nth easy-hugo--current-postdir easy-hugo--postdir-list) easy-hugo-basedir))
  (easy-hugo))

(defun easy-hugo-previous-postdir ()
  "Go to previous postdir."
  (interactive)
  (setq easy-hugo--postdir-list (easy-hugo--directory-list (easy-hugo--directory-files-recursively (expand-file-name "content" easy-hugo-basedir) "" t)))
  (setq easy-hugo--postdir-list (delete (expand-file-name "content/post" easy-hugo-basedir) easy-hugo--postdir-list))
  (add-to-list 'easy-hugo--postdir-list (expand-file-name "content" easy-hugo-basedir) t)
  (add-to-list 'easy-hugo--postdir-list (expand-file-name "content/post" easy-hugo-basedir))
  (setq easy-hugo--current-postdir (- easy-hugo--current-postdir 1))
  (when (> 0 easy-hugo--current-postdir)
    (setq easy-hugo--current-postdir (- (length easy-hugo--postdir-list) 1)))
  (setq easy-hugo-postdir (file-relative-name (nth easy-hugo--current-postdir easy-hugo--postdir-list) easy-hugo-basedir))
  (easy-hugo))

(defun easy-hugo--directory-list (list)
  "Return only directories in LIST."
  (if list
      (if (file-directory-p (car list))
	  (cons (car list)
		(easy-hugo--directory-list (cdr list)))
	(easy-hugo--directory-list (cdr list)))))

(defsubst easy-hugo--directory-name-p (name)
  "Return non-nil if NAME ends with a directory separator character."
  (let ((len (length name))
        (lastc ?.))
    (if (> len 0)
        (setq lastc (aref name (1- len))))
    (or (= lastc ?/)
        (and (memq system-type '(windows-nt ms-dos))
             (= lastc ?\\)))))

(defun easy-hugo--directory-files-recursively (dir regexp &optional include-directories)
  "Return list of all files under DIR that have file names matching REGEXP.
This function works recursively.  Files are returned in \"depth first\"
order, and files from each directory are sorted in alphabetical order.
Each file name appears in the returned list in its absolute form.
Optional argument INCLUDE-DIRECTORIES non-nil means also include in the
output directories whose names match REGEXP."
  (let ((result nil)
	(files nil)
	(tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
			'string<))
      (unless (member file '("./" "../"))
	(if (easy-hugo--directory-name-p file)
	    (let* ((leaf (substring file 0 (1- (length file))))
		   (full-file (expand-file-name leaf dir)))
	      (unless (file-symlink-p full-file)
		(setq result
		      (nconc result (easy-hugo--directory-files-recursively
				     full-file regexp include-directories))))
	      (when (and include-directories
			 (string-match regexp leaf))
		(setq result (nconc result (list full-file)))))
	  (when (string-match regexp file)
	    (push (expand-file-name file dir) files)))))
    (nconc result (nreverse files))))

(defun easy-hugo-draft-list ()
  "Drafts list mode of easy-hugo."
  (easy-hugo-with-env
   (when (> 0.25 (easy-hugo--version))
     (error "'List draft' requires hugo 0.25 or higher"))
   (let ((source (split-string
		  (with-temp-buffer
		    (let ((ret (call-process-shell-command "hugo list drafts" nil t)))
		      (unless (zerop ret)
			(error "'Hugo list drafts' command does not end normally"))
		      (buffer-string)))
		  "\n"))
	 (lists (list))
	 (files (list)))
     (dolist (file source)
       (if (equal (file-relative-name easy-hugo-postdir "content") ".")
	   (when (eq (string-match "\\([^/]+\\)$" file) 0)
	     (push (match-string 1 file) files))
	 (when (string-match (concat (file-relative-name easy-hugo-postdir "content") "/\\(.+?\\)$") file)
	   (push (match-string 1 file) files))))
     (unless (file-directory-p (expand-file-name easy-hugo-postdir easy-hugo-basedir))
       (error "%s%s doesn't exist!" easy-hugo-basedir easy-hugo-postdir))
     (setq easy-hugo--mode-buffer (get-buffer-create easy-hugo--buffer-name))
     (switch-to-buffer easy-hugo--mode-buffer)
     (setq-local default-directory easy-hugo-basedir)
     (setq buffer-read-only nil)
     (erase-buffer)
     (if (equal (file-relative-name easy-hugo-postdir "content") ".")
	 (insert (propertize
		  (concat "Easy-hugo  " easy-hugo-url "/" easy-hugo--draft-mode "\n\n")
		  'face
		  'easy-hugo-help-face))
       (insert (propertize
		(concat "Easy-hugo  " easy-hugo-url "/"
			(file-relative-name easy-hugo-postdir "content")
			easy-hugo--draft-mode "\n\n")
		'face
		'easy-hugo-help-face)))
     (unless easy-hugo-no-help
       (insert (propertize easy-hugo-help 'face 'easy-hugo-help-face))
       (when easy-hugo-additional-help
	 (insert (propertize easy-hugo-add-help 'face 'easy-hugo-help-face)))
       (insert (propertize (concat "\n")'face 'easy-hugo-help-face)))
     (unless easy-hugo--refresh
       (setq easy-hugo--cursor (point)))
     (cond ((eq 1 easy-hugo--sort-char-flg) (setq files (reverse (sort files 'string<))))
	   ((eq 2 easy-hugo--sort-char-flg) (setq files (sort files 'string<))))
     (while files
       (push
	(concat
	 (format-time-string "%Y-%m-%d %H:%M:%S "
			     (nth 5 (file-attributes
				     (expand-file-name
				      (car files)
				      easy-hugo-postdir))))
	 (car files))
	lists)
       (pop files))
     (cond ((eq 1 easy-hugo--sort-time-flg) (setq lists (reverse (sort lists 'string<))))
	   ((eq 2 easy-hugo--sort-time-flg) (setq lists (sort lists 'string<))))
     (while lists
       (insert (concat (car lists) "\n"))
       (pop lists))
     (goto-char easy-hugo--cursor)
     (if easy-hugo--refresh
	 (progn
	   (when (< (line-number-at-pos) easy-hugo--unmovable-line)
	     (goto-char (point-min))
	     (forward-line (- easy-hugo--unmovable-line 1)))
	   (beginning-of-line)
	   (forward-char easy-hugo--forward-char))
       (forward-char easy-hugo--forward-char))
     (easy-hugo-mode))))

;;;###autoload
(defun easy-hugo ()
  "Easy hugo mode."
  (interactive)
  (easy-hugo-with-env
   (unless (file-directory-p (expand-file-name easy-hugo-postdir easy-hugo-basedir))
     (error "%s%s doesn't exist!" easy-hugo-basedir easy-hugo-postdir))
   (setq easy-hugo--mode-buffer (get-buffer-create easy-hugo--buffer-name))
   (setq easy-hugo--draft-list nil)
   (switch-to-buffer easy-hugo--mode-buffer)
   (setq-local default-directory easy-hugo-basedir)
   (setq buffer-read-only nil)
   (erase-buffer)
   (if (equal (file-relative-name easy-hugo-postdir "content") ".")
       (insert (propertize
		(concat "Easy-hugo  " easy-hugo-url "/" "\n\n")
		'face
		'easy-hugo-help-face))
     (insert (propertize
	      (concat "Easy-hugo  " easy-hugo-url "/" (file-relative-name easy-hugo-postdir "content") "\n\n")
	      'face
	      'easy-hugo-help-face)))
   (unless easy-hugo-no-help
     (insert (propertize easy-hugo-help 'face 'easy-hugo-help-face))
     (when easy-hugo-additional-help
       (insert (propertize easy-hugo-add-help 'face 'easy-hugo-help-face)))
     (insert (propertize (concat "\n")'face 'easy-hugo-help-face)))
   (unless easy-hugo--refresh
     (setq easy-hugo--cursor (point)))
   (let ((files (directory-files (expand-file-name easy-hugo-postdir easy-hugo-basedir)))
	 (lists (list)))
     (if (eq 2 (length files))
	 (progn
	   (insert easy-hugo--first-help)
	   (easy-hugo-mode)
	   (goto-char easy-hugo--cursor))
       (progn
	 (cond ((eq 1 easy-hugo--sort-char-flg) (setq files (reverse (sort files 'string<))))
	       ((eq 2 easy-hugo--sort-char-flg) (setq files (sort files 'string<))))
	 (while files
	   (unless (or (string= (car files) ".")
		       (string= (car files) "..")
		       (not (member (file-name-extension (car files)) easy-hugo--formats)))
	     (push
	      (concat
	       (format-time-string "%Y-%m-%d %H:%M:%S "
				   (nth 5 (file-attributes
					   (expand-file-name
					    (car files)
					    easy-hugo-postdir))))
	       (car files))
	      lists))
	   (pop files))
	 (cond ((eq 1 easy-hugo--sort-time-flg) (setq lists (reverse (sort lists 'string<))))
	       ((eq 2 easy-hugo--sort-time-flg) (setq lists (sort lists 'string<))))
	 (while lists
	   (insert (concat (car lists) "\n"))
	   (pop lists))
	 (goto-char easy-hugo--cursor)
	 (if easy-hugo--refresh
	     (progn
	       (when (< (line-number-at-pos) easy-hugo--unmovable-line)
		 (goto-char (point-min))
		 (forward-line (- easy-hugo--unmovable-line 1)))
	       (beginning-of-line)
	       (forward-char easy-hugo--forward-char))
	   (forward-char easy-hugo--forward-char))
	 (easy-hugo-mode))))))

(provide 'easy-hugo)

;;; easy-hugo.el ends here
