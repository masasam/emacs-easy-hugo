;;; easy-hugo-transient.el --- Trasnisent menu for easy-hugo -*- lexical-binding: t no-byte-compile: t -*-

;; Author: 食無魚
;; Created: 2021-09-25 21:21:41

;;; Commentary:
;; Call `easy-hugo-enable-menu' after loading package to enable transient menu.
;; Call command like `easy-hugo' or `easy-hugo-list-draft' will open transient menu automaticlly.
;; Or call `easy-hugo-menu' to use menu directly.

;;; Code:

(require 'transient)

(transient-define-prefix easy-hugo-menu ()
  "Invoke commands for easy-hugo-mode"
  :man-page "hugo"
  :transient-non-suffix 'transient--do-warn
  [:description "Easy Hugo Posts"
   :if-mode easy-hugo-mode
   ["Move"
    ("j" "Next post" easy-hugo-next-line :transient t)
    ("k" "Prev post" easy-hugo-previous-line :transient t)
    ("h" "Backward char" easy-hugo-backward-char :transient t)
    ("l" "Forward char" easy-hugo-forward-char :transient t)]
   ["Sort"
    ("u" "By publishday" easy-hugo-sort-publishday :transient t)
    ("S" "By char" easy-hugo-sort-char :transient t)
    ("s" "By time" easy-hugo-sort-time :transient t)]
   ["Edit"
    ("n" "New post" easy-hugo-newpost :transient t)
    ("R" "Rename post" easy-hugo-rename :transient t)
    ("d" "Delete post" easy-hugo-delete :transient t)
    ("g" "Refresh post" easy-hugo-refresh :transient t)]
   [""
    ("v" "View post" easy-hugo-view)
    ("V" "View post in other window" easy-hugo-view-other-window)
    ("e" "Open post" easy-hugo-open)
    ("o" "Open post in other window" easy-hugo-open-other-window)]]
  [:description "Easy Hugo Menu"
   ["Blog"
    (";" "Select blog" easy-hugo-select-blog)
    ("<" "Prev blog" easy-hugo-previous-blog)
    (">" "Next blog" easy-hugo-next-blog)
    ("J" "Nth blog" easy-hugo-nth-blog)]
   ["Postdir"
    ("/" "Select postdir" easy-hugo-select-postdir)
    ("." "Next postdir" easy-hugo-next-postdir)
    ("," "Prev postdir" easy-hugo-previous-postdir)]
   ["Post"
    ("f" "Select post" easy-hugo-select-filename)
    ("K" "List post" easy-hugo)
    ("D" "List draft" easy-hugo-list-draft)
    ("r" "Rg search" easy-hugo-rg)
    ("a" "Ag search" easy-hugo-ag)]
   ["Publish"
    ("P" "Publish clever" easy-hugo-publish-clever)
    ("T" "Publish timer" easy-hugo-publish-timer)
    ("t" "Publish cancel" easy-hugo-cancel-publish-timer)]]
  ["Deploy"
   ["Github"
    ("G" "Deploy" easy-hugo-github-deploy)
    ("H" "Delay" easy-hugo-github-deploy-timer)
    ("b" "Cancel" easy-hugo-cancel-github-deploy-timer)]
   ["Google"
    ("C" "Deploy" easy-hugo-google-cloud-storage-deploy)
    ("I" "Delay" easy-hugo-google-cloud-storage-deploy-timer)
    ("i" "Cancel" easy-hugo-cancel-google-cloud-storage-deploy-timer)]
   ["Amazon"
    ("A" "Deploy" easy-hugo-amazon-s3-deploy)
    ("W" "Delay" easy-hugo-amazon-s3-deploy-timer)
    ("m" "Cancel" easy-hugo-cancel-amazon-s3-deploy-timer)]
   ["Firebase"
    ("B" "Deploy" easy-hugo-firebase-deploy)
    ("L" "Delay" easy-hugo-firebase-deploy-timer)
    ("!" "Cancel" easy-hugo-cancel-firebase-deploy-timer)]
   ["Open"
    ("O" "Open basedir" easy-hugo-open-basedir)
    ("c" "Open config" easy-hugo-open-config)
    ("p" "Preview localhost" easy-hugo-preview)
    ("M" "Magit status" easy-hugo-magit)
    ("q" "Quit" easy-hugo-quit)]
   ]
  )

(advice-add 'easy-hugo :after #'easy-hugo-menu)
(advice-add 'easy-hugo-list-draft :after #'easy-hugo-menu)


(provide 'easy-hugo-transient)
;;; easy-hugo-transient.el ends here
