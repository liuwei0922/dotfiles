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

;; they all accept either a font-spec, font string ("input mono-12"), or xlfd
;; font string. you generally only need these two:

;; ;; DON'T use (`font-family-list'), it's unreliable on Linux
(setq   doom-font (font-spec :family "Fira Code" :size 14)
        doom-variable-pitch-font (font-spec :family "Sarasa Mono SC" :size 18)
        doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 18)
        doom-big-font (font-spec :family "Sarasa Mono SC" :size 18))

;; there are two ways to load a theme. both assume the theme is installed and
;; available. you can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. this is the default:
(setq doom-theme 'doom-solarized-light)

;; if you use `org' and don't want your org files in the default location below,
;; change `org-directory'. it must be set before org loads!
(setq org-directory "~/org/")

;; this determines the style of line numbers in effect. if set to `nil', line
;; numbers are disabled. for relative line numbers, set this to `relative'.
(setq display-line-numbers-type 't)


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


(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

;;设置代理
(load! "+proxy")

;;关闭emacs不用再次确认
(setq confirm-kill-emacs nil)

;;打开文件时, 光标自动定位到上次停留的位置
(save-place-mode 1)

;;org模式设置
(after! (org)
  ;;中文格式化
  (load! "+chinese")
  (setq org-emphasis-regexp-components
      '("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 2))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  ;;设置上下标
  (setq org-pretty-entities-include-sub-superscripts nil)
  (setq org-export-with-sub-superscripts '{})
  (setq org-use-sub-superscripts '{})
  (setq org-show-context-detail t)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-folded 'show2levels)
  (setq org-log-done 'time)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-agenda-files '("~/org/agenda/"))
  (setq org-ellipsis "⤵")
  ;;加入bilibili视频，其中链接为Bv号
  (add-to-list 'org-link-abbrev-alist '("bilibili" . "https://www.bilibili.com/video/%s"))
  (add-to-list 'org-link-abbrev-alist '("mengbai" . "https://mzh.moegirl.org.cn/zh-hans/%s"))
  (plist-put! org-format-latex-options :scale 3))

;;输入法控制
(add-hook! 'org-mode-hook #'toggle-input-method)

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" )
        org-superstar-remove-leading-stars t
        org-superstar-prettify-item-bullets t))

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


;;设置 rust-mode
(setq racer-rust-src-path "/home/zihua/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library")

;;avy快捷键
(map! :leader :desc "swiper-avy" "s a" #'swiper-avy)
(map! :leader :desc "avy-next-line" "a l" #'avy-goto-line)

;;hunspell
;;(add-to-list 'ispell-hunspell-dict-paths-alist '("en_US" "/usr/share/myspell/dicts/en_US"))
;;(setq ispell-hunspell-dictionary-alist '(("en_US")))
;;(setq ispell-alternate-dictionary "zh_CN")
;;(add-to-list 'ispell-local-dictionary-alist "en_US")

;;联网
(defun link-web ()
  "联网用"
  (interactive)
  (shell-command "python ~/repo/dotfiles/scripts/connect.py"))

(map! :leader :desc "校园网连接" "l l" #'link-web)

;;lsp-mode设置
(after! (lsp-mode)
  (setq lsp-diagnostics-provider :none)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-rust-show-hover-context nil)
  (setq lsp-signature-doc-lines 1)
  (setq lsp-signature-render-documentation nil))

;;evil 设置
;;REPL 列表
(setf my/repl-mode-list (make-list 1 'sly-mrepl-mode))
(after! (evil)
  (setcdr evil-insert-state-map nil)
  (setq evil-escape-key-sequence "jo")
;;判断当前模式是否是 REPL 然后加自动 SAVE
  (add-hook! 'evil-insert-state-exit-hook
             (lambda ()
                 (if (not (member major-mode my/repl-mode-list))
                     (save-buffer))))
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  (define-key evil-insert-state-map [escape] 'evil-normal-state))

;;rime 设置
(use-package! rime
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'minibuffer)
  (rime-inline-ascii-trigger 'shift-l))

(after! (vterm evil-collection)
  (add-hook!
   'vterm-mode-hook
   (evil-collection-define-key '(normal insert) 'vterm-mode-map
     (kbd "C-\\") 'toggle-input-method)))

(map! :g "S-SPC" #'toggle-input-method)

(defun rime-evil-escape-advice (orig-fun key)
  "advice for `rime-input-method' to make it work together with `evil-escape'.
Mainly modified from `evil-escape-pre-command-hook'"
  (if rime--preedit-overlay
;; if `rime--preedit-overlay' is non-nil, then we are editing something, do not abort
(apply orig-fun (list key))
    (when (featurep 'evil-escape)
(let* (
       (fkey (elt evil-escape-key-sequence 0))
       (skey (elt evil-escape-key-sequence 1))
       (evt (read-event nil nil evil-escape-delay))
       )
  (cond
   ((and (characterp evt)
	 (or (and (char-equal key fkey) (char-equal evt skey))
	     (and evil-escape-unordered-key-sequence
		  (char-equal key skey) (char-equal evt fkey))))
    (evil-repeat-stop)
    (evil-normal-state))
   ((null evt) (apply orig-fun (list key)))
   (t
    (apply orig-fun (list key))
    (if (numberp evt)
	(apply orig-fun (list evt))
      (setq unread-command-events (append unread-command-events (list evt))))))))))

(advice-add 'rime-input-method :around #'rime-evil-escape-advice)

