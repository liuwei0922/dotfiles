;;; $doomdir/config.el -*- lexical-binding: t; -*-

;; place your private configuration here! remember, you do not need to run 'doom
;; sync' after modifying this file!


;; some functionality uses this to identify you, e.g. gpg configuration, email
;; clients, file templates and snippets.
(setq user-full-name "墨墨线"
      user-mail-address "qinmoxiao@qq.com")

;; doom exposes five (optional) variables for controlling fonts in doom. here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; they all accept either a font-spec, font string ("input mono-12"), or xlfd
;; font string. you generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 32 )
      ;doom-variable-pitch-font (font-spec :family "sans" :size 13)
      )

;; there are two ways to load a theme. both assume the theme is installed and
;; available. you can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. this is the default:
(setq doom-theme 'doom-solarized-light)

;; if you use `org' and don't want your org files in the default location below,
;; change `org-directory'. it must be set before org loads!
(setq org-directory "~/org/")

;; this determines the style of line numbers in effect. if set to `nil', line
;; numbers are disabled. for relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; here are some additional functions/macros that could help you configure doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; to get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'k' (non-evil users must press 'c-c c k').
;; this will open documentation for it, including demos of how they are used.
;;
;; you can also try 'gd' (or 'c-c c d') to jump to their definition and see how
;; they are implemented.

;;关闭emacs不用再次确认
(setq confirm-kill-emacs nil)

;; 打开文件时, 光标自动定位到上次停留的位置
(save-place-mode 1)

;;设置中英表格对齐
(use-package! cnfonts
  :config
  (cnfonts-enable))

;;org模式设置
(after! (org)
  (setq org-log-done 'time)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-agenda-files '("~/org/agenda/"))
  (setq org-ellipsis "⤵")
  ;;加入bilibili视频，其中链接为Bv号
  (add-to-list 'org-link-abbrev-alist '("bilibili" . "https://www.bilibili.com/video/%s"))
  (plist-put! org-format-latex-options :scale 3))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" )
        org-superstar-remove-leading-stars t
        org-superstar-prettify-item-bullets t))

(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
                         ("org" . "http://elpa.emacs-china.org/org/")))

()

;;设置org导出到pdf
(with-eval-after-load 'ox-latex
;;设置默认的class为ctexart
  (setq org-latex-default-class "ctexart")
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process
        '(
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "rm -fr %b.out %b.log %b.tex auto"
  ))
  (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
        [no-default-packages]
        \\usepackage[utf8]{inputenc}
        \\usepackage[t1]{fontenc}
        \\usepackage{fixltx2e}
        \\usepackage{graphicx}
        \\usepackage{longtable}
        \\usepackage{float}
        \\usepackage{wrapfig}
        \\usepackage{rotating}
        \\usepackage[normalem]{ulem}
        \\usepackage{amsmath}
        \\usepackage{textcomp}
        \\usepackage{marvosym}
        \\usepackage{wasysym}
        \\usepackage{listings}
        \\usepackage{amssymb}
        \\usepackage{booktabs}
        \\usepackage[colorlinks,linkcolor=black,anchorcolor=red,citecolor=black]{hyperref}
        \\tolerance=1000

        % 设置源码格式
        \\lstset{framexleftmargin=5mm, frame=shadowbox, rulesepcolor=\\color{blue}}
        \\lstset{basicstyle=\\tiny}
        \\lstset{postbreak=\\space, breakindent=5pt, breaklines}

        % 设置verbatim的字体大小
        \\makeatletter
        \\def\\verbatim{\\tiny\\@verbatim \\frenchspacing\\@vobeyspaces \\@xverbatim}
        \\makeatother
        "
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}")
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;;调用打开tilix
(defun tilix ()
  "start a tilix instance based on the directory of current buffer."
  (interactive)
  (let ((tilix "tilix"))
    (start-process tilix nil tilix (expand-file-name "./"))))

;;快捷键绑定

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
      :after elfeed-show
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :nm "q" #'+rss/delete-pane
      :nm "o" #'ace-link-elfeed
      :nm "RET" #'org-ref-elfeed-add
      :nm "n" #'elfeed-show-next
      :nm "p" #'elfeed-show-prev
      :nm "P" #'elfeed-show-pdf
      :nm "+" #'elfeed-show-tag
      :nm "-" #'elfeed-show-untag
      :nm "s" #'elfeed-show-new-live-search
      :nm "y" #'elfeed-show-yank)


(after! elfeed-search
  (set-evil-initial-state! 'elfeed-search-mode 'normal))
(after! elfeed-show-mode
  (set-evil-initial-state! 'elfeed-show-mode   'normal))

(after! evil-snipe
  (push 'elfeed-show-mode   evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))


(after! elfeed
  ;; (elfeed-org)
  (use-package! elfeed-link)

  (setq elfeed-search-filter "@4-week-ago +unread"
        elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 0.6)

  (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground 'nil
                      :weight 'light)

  (defadvice! +rss-elfeed-wrap-h-nicer ()
    "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
    :override #'+rss-elfeed-wrap-h
    (setq-local truncate-lines nil
                shr-width 120
                default-text-properties '(line-height 1.1))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      ;; (setq-local shr-current-font '(:family "Merriweather" :height 1.2))
      (set-buffer-modified-p nil)))

  (defun +rss/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((elfeed-goodies/tag-column-width 40)
           (elfeed-goodies/feed-source-column-width 30)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat (mapconcat 'identity tags ",")))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))

           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left)))

      (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))
      (setq-local line-spacing 0.2)))

  (defun +rss/elfeed-show-refresh--better-style ()
    "Update the buffer to match the selected entry, using a mail-style."
    (interactive)
    (let* ((inhibit-read-only t)
           (title (elfeed-entry-title elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (author (elfeed-meta elfeed-show-entry :author))
           (link (elfeed-entry-link elfeed-show-entry))
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tagsstr (mapconcat #'symbol-name tags ", "))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
           (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
           (type (elfeed-entry-content-type elfeed-show-entry))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
      (erase-buffer)
      (insert "\n")
      (insert (format "%s\n\n" (propertize title 'face 'elfeed-show-title-face)))
      (insert (format "%s\t" (propertize feed-title 'face 'elfeed-search-feed-face)))
      (when (and author elfeed-show-entry-author)
        (insert (format "%s\n" (propertize author 'face 'elfeed-show-author-face))))
      (insert (format "%s\n\n" (propertize nicedate 'face 'elfeed-log-date-face)))
      (when tags
        (insert (format "%s\n"
                        (propertize tagsstr 'face 'elfeed-search-tag-face))))
      ;; (insert (propertize "Link: " 'face 'message-header-name))
      ;; (elfeed-insert-link link link)
      ;; (insert "\n")
      (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
               do (insert (propertize "Enclosure: " 'face 'message-header-name))
               do (elfeed-insert-link (car enclosure))
               do (insert "\n"))
      (insert "\n")
      (if content
          (if (eq type 'html)
              (elfeed-insert-html content base)
            (insert content))
        (insert (propertize "(empty)\n" 'face 'italic)))
      (goto-char (point-min))))
  )
