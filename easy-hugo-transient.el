;;; easy-hugo-transient.el --- Trasnisent menu for easy-hugo -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz@gmail.com>
;; Created: 2021-09-25 21:21:41

;;; Commentary:
;; Call `easy-hugo-enable-menu' after loading package to enable transient menu.
;; Call command like `easy-hugo' or `easy-hugo-list-draft' will open transient menu automaticlly.
;; Or call `easy-hugo-menu' to use menu directly.

;;; Code:

(require 'transient)
(require 'subr-x)

(defun easy-hugo-menu--header ()
  "Header used in `easy-hugo-menu'."
  (let ((dir (file-relative-name easy-hugo-postdir "content"))
        (url (string-remove-suffix "/" easy-hugo-url)))
    (format (propertize "[%s]: %s\t[%s]: %s\t[%s]: %s\t[%s]: %s%s\n" 'face 'bold)
            (propertize "sort" 'face 'font-lock-doc-face)
            (propertize
             (if easy-hugo--sort-char-flg
                 "char      "
               (if easy-hugo--sort-time-flg
                   "time      "
                 "publishday"))
             'face
             'font-lock-variable-name-face)
            (propertize "draft" 'face 'font-lock-doc-face)
            (if easy-hugo--draft-list
                (propertize "on " 'face 'font-lock-variable-name-face)
              (propertize "off" 'face 'font-lock-warning-face))
            (propertize "delay" 'face 'font-lock-doc-face)
            (if (nth easy-hugo--current-blog easy-hugo--publish-timer-list)
                (propertize "on " 'face 'font-lock-variable-name-face)
              (propertize "off" 'face 'font-lock-warning-face))
            (propertize "url" 'face 'font-lock-doc-face)
            (propertize url 'face  'font-lock-variable-name-face)
            (propertize (if (string= dir ".") "" (concat "/" dir))
                        'face
                        'font-lock-warning-face))))

(transient-define-prefix easy-hugo-menu ()
  "Invoke commands for easy-hugo-mode"
  :man-page "hugo"
  :transient-non-suffix 'transient--do-warn
  [:description easy-hugo-menu--header
   ["Project"
    ("M" "Magit status" easy-hugo-magit)
    ("p" "Preview local" easy-hugo-preview)
    ("O" "Open basedir" easy-hugo-open-basedir)
    ("c" "Open config" easy-hugo-open-config)
    ("q" "Quit" easy-hugo-quit)]
   ["Post"
    ("f" "Select post" easy-hugo-select-filename)
    ("K" "List post" easy-hugo)
    ("D" "List draft" easy-hugo-list-draft)
    ("r" "Rg search" easy-hugo-rg)
    ("a" "Ag search" easy-hugo-ag)]
   ["Blog"
    (";" "Select blog" easy-hugo-select-blog)
    ("<" "Prev blog" easy-hugo-previous-blog)
    (">" "Next blog" easy-hugo-next-blog)]
   ["Postdir"
    ("/" "Select postdir" easy-hugo-select-postdir)
    ("." "Next postdir" easy-hugo-next-postdir)
    ("," "Prev postdir" easy-hugo-previous-postdir)]
   ["Publish"
    ("P" "Publish now" easy-hugo-publish-clever)
    ("T" "Publish delay" easy-hugo-publish-timer)
    ("t" "Publish cancel" easy-hugo-cancel-publish-timer)]
   ]
  [:if-mode easy-hugo-mode
   ["Move"
    ("j" "Next post" easy-hugo-next-line :transient t)
    ("k" "Prev post" easy-hugo-previous-line :transient t)
    ("h" "Backward char" easy-hugo-backward-char :transient t)
    ("l" "Forward char" easy-hugo-forward-char :transient t)]
   ["Sort"
    ("s" "By time" easy-hugo-sort-time :transient t)
    ("S" "By char" easy-hugo-sort-char :transient t)
    ("u" "By publishD" easy-hugo-sort-publishday :transient t)]
   ["Edit"
    ("n" "New post" easy-hugo-newpost :transient t)
    ("R" "Rename post" easy-hugo-rename :transient t)
    ("d" "Delete post" easy-hugo-delete :transient t)
    ("g" "Refresh post" easy-hugo-refresh :transient t)]
   ["View"
    ("v" "View post" easy-hugo-view)
    ("V" "View post in other window" easy-hugo-view-other-window)
    ("e" "Open post" easy-hugo-open)
    ("o" "Open post in other window" easy-hugo-open-other-window)]]
  )

(advice-add 'easy-hugo :after #'easy-hugo-menu)
(advice-add 'easy-hugo-list-draft :after #'easy-hugo-menu)

(provide 'easy-hugo-transient)
;;; easy-hugo-transient.el ends here
