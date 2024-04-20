;;; init-org.el --- Initialize Org configurations.	-*- lexical-binding: t -*-

(require 'init-utils)

(defmacro use-package! (package &rest body)
  (if (daemonp)
      `(use-package ,package
	 :if (daemonp)
	 :ensure t
	 ,@body)
    `(use-package ,package
       :ensure nil
       :defer t
       ,@body)))

(use-package! org
  :bind (:map org-mode-map
	      ("C-C a" . org-agenda)
	      ("C-'" . nil)
	      ("M-l" . org-metaright)
	      ("M-h" . org-metaleft)
	      ("M-H" . org-shiftmetaleft)
	      ("M-L" . org-shiftmetaright)
	      ("M-=" . advance-words-count)
	      :map global-map
	      ("C-c n c" . org-capture)
	      )
  :hook
  ((org-mode . (lambda () (unless buffer-read-only
			    (set-input-method "rime"))))
   ;;(org-mode . (lambda () (display-line-numbers-mode -1)))
   (org-mode . (lambda () (setq truncate-lines nil)))  
   (org-mode . (lambda () (yas-activate-extra-mode 'latex-mode)))
   (org-mode . (lambda () (display-line-numbers-mode -1)))
   ;; 设置 ORG 标题样式
   (org-cycle-tab-first . +org-cycle-only-current-subtree-h)
   (org-mode . (lambda ()
		 ;; 设置折行
		 (global-word-wrap-whitespace-mode 1)))
   )
  :custom
  ;; 文件相关设置
  (org-directory "~/org/")
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  ;; 整体美化相关设置
  (org-ellipsis "⤵")
  (org-startup-indented t)
  (org-fontify-todo-headline nil)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")))
  ;; 设置 ORG 标题范围，使上面的尾标可以正确显示
  (org-cycle-separator-lines 2)
  ;; 上下标控制
  (org-export-with-sub-superscripts '{})
  (org-use-sub-superscripts '{})
  (org-use-sub-superscripts '{})
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  ;; org 中图片大小
  (org-image-actual-width nil)
  ;; 其他设置
  (org-imenu-depth 4)
  (org-clone-delete-id t)
  (org-use-sub-superscripts '{})
  (org-yank-adjusted-subtrees t)
  (org-ctrl-k-protect-subtree 'error)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-return-follows-link nil)
  ;; 启动时缩进
  (org-startup-indented t)
  ;; 启动时折叠内容
  (org-startup-folded t)
  ;; TODO 结束时加上时间
  (org-log-done 'time)
  ;; TODO 设置
  (org-todo-keywords '((sequence "TODO(t)"
				 "DOING(i!)"
				 "|"
				 "DONE(d!)"
				 "KILLED(k!)")))
  (org-todo-keyword-faces '(("DONE"       :foreground "#7c7c75" :weight bold)
                            ("DOING"       :foreground "#feb24c" :weight bold)
                            ("TODO"       :foreground "#50a14f" :weight bold)
                            ("KILLED"  :foreground "#ff6480" :weight bold)))
  ;; 时间戳格式
  (org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-priority-faces '((?A :foreground "red")
                        (?B :foreground "orange")
                        (?C :foreground "yellow")))
  (org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %CLOCKSUM")
  ;; Remove CLOSED: [timestamp] after switching to non-DONE states
  (org-closed-keep-when-no-todo t)
  ;; refile
  (org-refile-use-cache nil)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; tags, e.g. #+TAGS: keyword in your file
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)
  ;; id
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; abbreviation for url
  (org-link-abbrev-alist '(("GitHub" . "https://github.com/")
                           ("GitLab" . "https://gitlab.com/")
                           ("Google" . "https://google.com/search?q=")
                           ("RFCs"   . "https://tools.ietf.org/html/")
                           ("LWN"    . "https://lwn.net/Articles/")
                           ("WG21"   . "https://wg21.link/")
			   ("bilibili" . "https://www.bilibili.com/video/%s")
			   ("mengbai" . "https://mzh.moegirl.org.cn/zh-hans/%s")
			   ("handian" . "https://www.zdic.net/hans/%s")))
  ;; 设置默认的class为ctexart
  (org-latex-default-class "ctexart")
  (org-latex-pdf-process
   '(
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "rm -fr %b.bbl %b.out %b.log %b.tex auto"
     ))
  (org-preview-latex-default-process 'xdvsvgm)
  (org-preview-latex-image-directory "~/org/ltximg/")
  (org-preview-latex-process-alist
   '((dvipng :programs
	     ("latex" "dvipng")
	     :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
	     (1.0 . 1.0)
	     :latex-compiler
	     ("latex -interaction nonstopmode -output-directory %o %F")
	     :image-converter
	     ("dvipng -D %D -T tight -o %O %F")
	     :transparent-image-converter
	     ("dvipng -D %D -T tight -bg Transparent -o %O %F"))
     (xdvsvgm :programs
	      ("xelatex" "dvisvgm")
	      :description "xdv > svg"
	      :message "you need to install the programs: xelatex and dvisvgm."
	      :image-input-type "xdv"
	      :image-output-type "svg"
	      :image-size-adjust (1.7 . 1.5)
	      :latex-compiler
	      ("xelatex -interaction nonstopmode -no-pdf -output-directory %o %F")
	      :image-converter
	      ("dvisvgm %F --no-fonts --exact-bbox --scale=%S --output=%O"))
     (imagemagick :programs
		  ("latex" "convert")
		  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
		  (1.0 . 1.0)
		  :latex-compiler
		  ("pdflatex -interaction nonstopmode -output-directory %o %F")
		  :image-converter
		  ("convert -density %D -trim -antialias %F -quality 100 %O"))))

  :config
  ;; 显示上下标
  ;;(org-toggle-pretty-entities)
  ;; 设置 TAB 键只有两层循环
  (defun +org-cycle-only-current-subtree-h (&optional arg)
    "Toggle the local fold at the point (as opposed to cycling through all levels
with `org-cycle')."
    (interactive "P")
    (unless (eq this-command 'org-shifttab)
      (save-excursion
	(org-beginning-of-line)
	(let (invisible-p)
          (when (and (org-at-heading-p)
                     (or org-cycle-open-archived-trees
			 (not (member org-archive-tag (org-get-tags))))
                     (or (not arg)
			 (setq invisible-p (outline-invisible-p (line-end-position)))))
            (unless invisible-p
              (setq org-cycle-subtree-status 'subtree))
            (org-cycle-internal-local)
            t)))))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "okular"))
  )

(use-package ox-latex
  :ensure nil
  :after org
  :config
  ;; 设置 latex 图片缩放比例
  (plist-put org-format-latex-options :scale 1.5)
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :foreground 'default)
  (add-to-list 'org-latex-classes
	       '("ctexart" "\\documentclass[11pt]{ctexart}
[NO-DEFAULT-PACKAGES]
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
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
\\usepackage[margin=1in]{geometry}
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


;;; org-agenda
(use-package org-agenda
  :ensure nil
  :hook
  (org-agenda-finalize . org-agenda-to-appt)
  :custom
  (org-agenda-files (cond ((eq system-type 'gnu/linux) '("~/org/agenda/agenda.org"))
			  ((eq system-type 'windows-nt) '("d:/onedrive/OneDrive - cumt.edu.cn/org/agenda"))))
  ;; 在 org-agenda 中不显示所有日期
  (org-agenda-show-all-dates nil)
  ;; 设置查看完成的项目
  (org-agenda-custom-commands
   '(("r" "记录回顾"
      ((agenda ""
	       ((org-agenda-skip-function
		 #'(lambda ()
		     (org-back-to-heading t)
		     (let ((level (org-current-level))
			   (next-heading (save-excursion
					   (outline-next-heading))))
		       (if (or (< level +org-agenda-set-level)
			       (= level +org-agenda-set-level))
			   nil
			 next-heading))))
		(org-agenda-overriding-header "记录回顾")
		(org-agenda-span 'year)
		(org-agenda-start-on-weekday nil)
		(org-agenda-start-day "-y")
		(org-agenda-show-log 'only)
		(org-agenda-show-all-dates nil)
		)))
      ((+org-agenda-set-level (string-to-number (read-string "level:")))))))
  (org-agenda-current-time-string "现在 - - - - - - - - - - - - -")
  (org-agenda-scheduled-leaders '("预 " "应%02d天前开始 "))
  (org-agenda-deadline-leaders '("止 " "过%02d天后到期 " "已经过期%02d天 "))
  (org-agenda-format-date #'+org-agenda-format-date-aligned)
  :config
  (defun +org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date))
	   (day (cadr date))
	   (day-of-week (calendar-day-of-week date))
	   (month (car date))
	   (monthname (calendar-month-name month))
	   (year (nth 2 date))
	   (iso-week (org-days-to-iso-week
		      (calendar-absolute-from-gregorian date)))
	   ;; (weekyear (cond ((and (= month 1) (>= iso-week 52))
	   ;;        	  (1- year))
	   ;;        	 ((and (= month 12) (<= iso-week 1))
	   ;;        	  (1+ year))
	   ;;        	 (t year)))
	   (weekstring (if (= day-of-week 1)
			   (format " W%02d" iso-week)
			 "")))
      (format "%4d年 - %s - %2d日 %-10s"
	      year monthname day dayname)))
  (defvar +org-agenda-set-level 2 "回顾时用到的层级数"))


(use-package parse-time
  :after org-agenda
  :ensure nil
  :config
  (setq parse-time-months
        (append '(("yiyue" . 1) ("eryue" . 2) ("sanyue" . 3)
                  ("siyue" . 4) ("wuyue" . 5) ("liuyue" . 6)
                  ("qiyue" . 7) ("bayue" . 8) ("jiuyue" . 9)
                  ("shiyue" . 10) ("shiyiyue" . 11) ("shieryue" . 12))
                parse-time-months))
  (setq parse-time-weekdays
        (append '(("zri" . 0) ("zqi" . 0)
                  ("zyi" . 1) ("zer" . 2) ("zsan" . 3)
                  ("zsi" . 4) ("zwu" . 5) ("zliu" . 6)
                  ("zr" . 0) ("zq" . 0)
                  ("zy" . 1) ("ze" . 2) ("zs" . 3)
                  ("zsi" . 4) ("zw" . 5) ("zl" . 6))
                parse-time-weekdays)))

(use-package calendar
  :after (:or org-agenda hledger-mode)
  :ensure nil
  :config
  (setq calendar-week-start-day 1)
  (setq calendar-month-name-array
	["一月" "二月" "三月" "四月" "五月" "六月"
         "七月" "八月" "九月" "十月" "十一月" "十二月"])
  (setq calendar-day-name-array
        ["星期天" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六"])
  )

;;; org-capture 
(use-package org-capture
  :ensure nil
  :hook
  (org-capture-mode . +org-capture-setup)
  :config
  (with-no-warnings
    (defun +org-capture-setup ()
      (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
  (defun +capture-filename ()
    (interactive)
    (let* ((filename (read-from-minibuffer "文件名："))
	   (cmd (concat "fd --search-path " denote-directory " " filename))
	   (files (shell-command-to-string cmd)))
      ;;搜索为空说明没有找到
      (if (string-empty-p files)
	  (let* ((title filename)
		 (file-type 'org)
		 (keyws (read-string "关键词："))
		 (kws  (if (string-empty-p keyws)
			   nil
			 (split-string keyws ",")))
		 (date (current-time))
		 (id (format-time-string denote-id-format date))
		 (directory (denote-directory))
		 (template "")
		 (signature "")
		 (path (denote--path title kws directory id file-type signature))
		 (header (denote--format-front-matter title
						      (denote--date date file-type)
						      kws
						      (format-time-string denote-id-format date)
						      file-type))
		 (buffer (find-file path)))
	    (denote-barf-duplicate-id id)
	    (with-current-buffer buffer
	      (insert header)
	      (insert template)
	      (save-buffer)
	      (kill-buffer))
	    path)
	(progn
	  (let* ((lines (split-string files "[\n]+"))
		 (searched-file (ivy-read (format "choose the file[%s]: " default-directory) lines)))
	    searched-file)))))

  (defun +capture-title ()
    (interactive)
    (let ((candidates (consult--outline-candidates)))
      (consult--read candidates
		     :prompt "标题："
		     :sort nil
		     :require-match t
		     :lookup #'consult--line-match
		     )))
  ;; (defun +capture-title ()
  ;;   (interactive)
  ;;   ;;设置搜索的正则表达式
  ;;   (let ((settings (cdr (assq major-mode counsel-outline-settings))))
  ;;     (ivy-read "标题：" (counsel-outline-candidates settings)
  ;; 		:action #'(lambda (x)
  ;; 			    ;;如果搜到了，X 是 list，否则是字符串
  ;; 			    (if (listp x)  
  ;; 				(progn  
  ;; 				  (goto-char (cdr x))
  ;; 				  (or (bolp)
  ;; 				      (insert "\n"))
  ;; 				  (org-end-of-subtree))
  ;; 			      (progn
  ;; 				;;移动到最后
  ;; 				(end-of-buffer)
  ;; 				(or (bolp)
  ;; 				    (insert "\n"))
  ;; 				(when (/= (point) (point-min))
  ;; 				  (org-end-of-subtree t t))
  ;; 				(insert  "* " x "\n")
  ;; 				(org-end-of-subtree)))))))
  
  (defun +capture-set-id ()
    (let ((org-id-method 'uuid))
      (org-id-new)))
  
  (defun +org-add-link ()
    (interactive)
    (let* ((buffer (if buffer-file-name
		       (current-buffer)
		     (org-capture-get :buffer 'local)))
	   (id (with-current-buffer buffer
		 (let* ((settings (cdr (assq major-mode counsel-outline-settings)))
			(candidates (counsel-outline-candidates settings))
			heading)
		   (ivy-read "标题：" candidates
			     :action (lambda (x)
				       (if (listp x)
					   (setq heading (cdr x)))))
		   (org-id-get heading))))
	   (description (read-string "描述：")))
      (insert (format "[[id:%s][%s]]" id description))))
  :custom
  (org-capture-bookmark nil)
  (org-capture-use-agenda-date t)
  (org-capture-templates
   '(("t" "TODO" entry (file+headline org-agenda-files "Collect")
      "* TODO %? %^G \n  %U" :empty-lines 1)
     ("s" "Scheduled TODO" entry (file+headline "" "Collect")
      "* TODO %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 1)
     ("d" "Deadline" entry (file+headline "" "Collect")
      "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
     ("n" "Note" plain (file+function +capture-filename +capture-title)
      "** %^{标题} \n:PROPERTIES:\n:CREATED: %U\n:ID: %(+capture-set-id) \n:END: \n%?")
     ("j" "Journal" entry (file+datetree "~/org/nichijou.org")
      "* %^{标题} \nChanged at %U\n%?"))))



;;; org-src
(use-package org-src
  :ensure nil
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (:map org-src-mode-map
              ;; consistent with separedit/magit
              ("C-c C-c" . org-edit-src-exit))
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'other-window)
  (org-src-lang-modes '(("C"      . c)
                        ("C++"    . c++)
                        ("bash"   . sh)
                        ("cpp"    . c++)
                        ("dot"    . graphviz-dot) ;; was `fundamental-mode'
                        ("elisp"  . emacs-lisp)
                        ("ocaml"  . tuareg)
                        ("shell"  . sh)))
  (org-babel-load-languages '((C          . t)
                              (dot        . t)
                              (eshell     . t)
                              (emacs-lisp . t)
                              (shell      . t)
                              (python     . t))))


;;; 美化工具
(use-package org-bullets
  :ensure t
  :defer t
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))


;;; denote
(use-package denote
  :ensure t
  :defer t
  :after org
  :bind
  (("C-c n n" . denote)
   ("C-c n i" . denote-link-or-create)
   ("C-c n I" . denote-link)
   ("C-c n b" . denote-link-backlinks)
   ("C-c n a" . denote-add-front-matter)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   )
  :config
  (setq denote-directory (expand-file-name "~/org/notes/")
	denote-known-keywords '("read")
	denote-infer-keywords t
	denote-sort-keywords t
	denote-allow-multi-word-keywords t
	denote-date-prompt-use-org-read-date t
	denote-link-fontify-backlinks t
	denote-front-matter-date-format 'org-timestamp
	denote-prompts '(title keywords)))


;;; org-download
(use-package org-download
  :ensure t
  :defer t
  :after org
  :hook
  ((org-mode dired-mode) . org-download-enable)
  :config
  (when (eq +system-type 'wsl)
    (defun org-download-clipboard (&optional basename)
	  "Capture the image from the clipboard and insert the resulting file."
	  (interactive)
	  (let ((org-download-screenshot-method
		 (if (executable-find "wl-paste")
		     "wl-paste -t image/bmp | convert bmp:- %s"
		   (user-error
		    "Please install the \"wl-paste\" program included in wl-clipboard"))))
	    (org-download-screenshot basename))))
  ;; (defun +org-download-method (link)
  ;;   (org-download--fullname (org-link-unescape link)))
  ;; :custom
  ;; (org-download-method #'+org-download-method)
  ;; (org-download-annotate-function (lambda (_link) ""))
  ;; (org-download-method 'attach)
  ;; (org-download-screenshot-method "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")
  )



;;; xenops
(use-package xenops
  :ensure t
  :defer t
  :after (latex)
  :hook
  ;; (org-mode . (lambda ()
  ;; 		(unless (and (boundp 'org-capture-mode)
  ;; 			     org-capture-mode)
  ;; 		  (xenops-mode)
  ;; 		  ;; 不激活 keymap
  ;; 		  (setq-local minor-mode-map-alist
  ;; 			      (assq-delete-all 'xenops-mode minor-mode-map-alist))
  ;; 		  )))
  (latex-mode . xenops-mode)
  (Latex-mode . xenops-mode)
  :custom
  (xenops-font-height 5)
  :config
  (when (eq +system-type 'wsl)
    (setq display-mm-dimensions-alist '(("wayland-0" . (286 . 179))))
    )
  )
;;(use-package org-fragtog)


;;; laas
;; (use-package laas
;;   :ensure nil
;;   :after (:any org latex)
;;   :hook
;;   (LaTeX-mode . laas-mode)
;;   (org-mode . laas-mode)
;;   )



;;; org-citar
(use-package citar
  :ensure t
  :defer t
  :after (:any org latex)
  :custom
  (citar-bibliography '("~/org/bibliography/references.bib"))
  (citar-notes-paths '("~/org/notes"))
  (org-cite-global-bibliography '("~/org/bibliography/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-library-paths '("~/org/bibtex-pdfs"))
  (citar-at-point-function 'embark-act)
  ;;(advice-add 'citar-file-open :override #'citar-file-open-external)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind
  (:map org-mode-map
	("C-c b" . #'org-cite-insert))
  :config
  (defun +okular-open-pdf (file)
    (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))
    (pcase +system-type
      ('wsl (async-shell-command (concat "wslview " file)))
      ))
  (add-to-list 'citar-file-open-functions '("pdf" . +okular-open-pdf)))

(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :config
  (citar-embark-mode)
  )


(use-package biblio
  :ensure t
  :after org
  :custom
  (biblio-download-directory "~/org/bibtex-pdfs/")
  )

(use-package ebib
  :ensure t
  :after org
  :custom
  (ebib-default-directory "~/org/bibliography/")
  (ebib-bib-search-dirs "~/org/bibliography/")
  (ebib-file-search-dirs '("~/org/bibtex-pdfs"))
  (ebib-preload-bib-files '("~/org/bibliography/references.bib"))
  (ebib-bibtex-dialect 'biblatex)
  (ebib-file-associations '(("pdf" . +okular-open-pdf)))
  )

(use-package ebib-biblio
  :after (ebib biblio)
  :bind (:map ebib-index-mode-map
              ("B" . ebib-biblio-import-doi)
              :map biblio-selection-mode-map
              ("e" . ebib-biblio-selection-import)))

;; (use-package org-modern
;;   :ensure nil
;;   :after org
;;   :hook
;;   (org-mode . org-modern-mode)
;;   (org-mode . (lambda ()
;; 		(org-indent-mode -1)))
;;   (org-modern-mode . (lambda ()
;; 		       (global-display-line-numbers-mode -1)
;; 		       (line-number-mode -1)
;; 		       (display-line-numbers-mode -1))))



;;; org-ref
;; (use-package ivy-bibtex
;;   :ensure nil
;;   :after ivy org 
;;   :config
;;   (setq bibtex-completion-bibliography '("~/org/bibliography/references.bib"
;; 					 "~/org/bibliography/archive.bib")
;; 	bibtex-completion-library-path '("~/org/bibliography/bibtex-pdfs/")
;; 	bibtex-completion-notes-path "~/org/bibliography/notes/"
;; 	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

;; 	bibtex-completion-additional-search-fields '(keywords)
;; 	bibtex-completion-display-formats
;; 	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
;; 	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
;; 	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
;; 	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
;; 	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
;; 	bibtex-completion-pdf-open-function
;; 	(lambda (fpath)
;; 	  (call-process "open" nil 0 nil fpath)))
;;   :bind
;;   ("C-c n b" . org-ref-bibtex-new-entry/body))

;; (use-package org-ref
;;   :ensure nil
;;   :after org
;;   :init
;;   (require 'bibtex)
;;   (setq bibtex-autokey-year-length 4
;; 	bibtex-autokey-name-year-separator "-"
;; 	bibtex-autokey-year-title-separator "-"
;; 	bibtex-autokey-titleword-separator "-"
;; 	bibtex-autokey-titlewords 2
;; 	bibtex-autokey-titlewords-stretch 1
;; 	bibtex-autokey-titleword-length 5)
;;   (require 'org-ref-ivy)
;;   (require 'org-ref-arxiv)
;;   (require 'org-ref-scopus)
;;   (require 'org-ref-wos)
;;   :bind
;;   (
;;    :map bibtex-mode-map
;;    ("C-c b" . org-ref-bibtex-hydra/body)
;;    :map org-mode-map
;;    ("C-c ]" . org-ref-insert-link)
;;    ("M-[" . org-ref-insert-link-hydra/body)
;;    )
;;   )

;; (use-package org-ref-ivy
;;   :ensure nil
;;   :after org-ref
;;   :init
;;   (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
;; 	org-ref-insert-cite-function 'org-ref-cite-insert-ivy
;; 	org-ref-insert-label-function 'org-ref-insert-label-link
;; 	org-ref-insert-ref-function 'org-ref-insert-ref-link
;; 	org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))







;; 设置表格对齐
;; (use-package org-faces
;;   :config (when (display-graphic-p)
;; 	    (set-face-attribute 'org-table nil :family "" :height 100 :weight 'normal)
;;             ;;(set-face-attribute 'org-table nil :family "Sarasa Mono SC")
;; 	    ))
;; (use-package valign
;;   :ensure nil
;;   :after (:any org markdown)
;;   :hook
;;   (org-mode . valign-mode)
;;  )
;; (use-package ftable
;;   :ensure nil
;;   :after valign)



;;; org-download
;; (defun my-yank-image-from-win-clipboard-through-powershell()
;;   "to simplify the logic, use c:/Users/Public as temporary directoy, and move it into current directoy"
;;   (interactive)
;;   (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
;;          (file-name (format-time-string "screenshot_%Y%m%d_%H%M%S.png"))
;;          ;; (file-path-powershell (concat "c:/Users/\$env:USERNAME/" file-name))
;;          (file-path-wsl (concat "./images/" file-name))
;;          )
;;     ;; (shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/\\$env:USERNAME/" file-name "\\\")\""))
;;     (shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/Public/" file-name "\\\")\""))
;;     (rename-file (concat "/mnt/c/Users/Public/" file-name) file-path-wsl)
;;     (insert (concat "[[file:" file-path-wsl "]]"))
;;     (message "insert DONE.")
;;     ))

;; markdown-mode
;; (use-package markdown-mode
;;   :ensure nil)


(provide 'init-org)
;;; init-org.el ends here
