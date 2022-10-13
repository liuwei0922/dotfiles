;;; ../repo/dotfiles/doom.d/+elfeed.el -*- lexical-binding: t; -*-


;;elfeed快捷键绑定
(map! :leader :desc "open elfeed" "o e" #'elfeed)

(map! :map elfeed-search-mode-map
      :after elfeed-search
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      ;; :n "q" #'+rss/quit
      :n "e" #'elfeed-update
      :n "r" #'elfeed-search-untag-all-unread
      :n "u" #'elfeed-search-tag-all-unread
      :n "s" #'elfeed-search-live-filter
      :n "RET" #'elfeed-search-show-entry
      :n "P" #'elfeed-show-pdf
      :n "+" #'elfeed-search-tag-all
      :n "-" #'elfeed-search-untag-all
      :n "S" #'elfeed-search-set-filter
      :n "b" #'elfeed-search-browse-url
      :n "y" #'elfeed-search-yank)

(map! :map elfeed-show-mode-map
      :after elfeed-search
      :n "n" #'elfeed-show-next
      :n "p" #'elfeed-show-prev)

(use-package! elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))
(after! elfeed-search
  (setq elfeed-search-filter "@2-week-ago +unread"))
