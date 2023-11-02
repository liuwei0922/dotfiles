;;; init.el -*- lexical-binding: t; -*-

;;; 设置用户名和邮箱
(setq user-full-name "qinmoxiao")
(setq user-mail-address "qinmoxiao@qq.com")


;;; 加载配置路径
;;(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; 文件相关设置
;; 设置环境变量
(when (eq system-type 'windows-nt)
  (setenv "PATH" (concat "C:\\msys64\\mingw64\\bin;"
			 "C:\\msys64\\usr\\bin;"
			 "D:\\Program Files\\Steel Bank Common Lisp;"
			 "D:\\program Files\\Rust\\.cargo\\bin;"
			 "D:\\Program Files\\Rust\\.rustup\\toolchains\\table-x86_64-pc-windows-gnu\\bin;"
			 (getenv "PATH")))
  (setq exec-path (split-string (getenv "PATH") path-separator))
  ;; 设置各种文件编码
  (setq buffer-file-coding-system 'utf-8-unix
	default-file-name-coding-system 'utf-8-unix
	default-keyboard-coding-system 'utf-8-unix
	default-process-coding-system '(utf-8-unix . utf-8-unix)
	default-sendmail-coding-system 'utf-8-unix
	default-terminal-coding-system 'utf-8-unix)
  )
(setq buffer-file-coding-system 'utf-8-unix)

;; 禁止 NATIVE-COMPILE
;;(setq no-native-compile t)
;; 禁止Emacs自动生成备份文件
(setq make-backup-files nil)
;; 选中一段文字后，输入一个字符会替换掉选中文字
(delete-selection-mode 1)
;; 替换yes/no为y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; 默认文件编码
(prefer-coding-system 'utf-8)

;; (when (eq system-type 'windows-nt)
;;   (setq locale-coding-system 'gb18030)  ;此句保证中文字体设置有效
;;   (setq w32-unicode-filenames 'nil)       ; 确保file-name-coding-system变量的设置不会无效
;;   (setq file-name-coding-system 'gb18030) ; 设置文件名的编码为gb18030
;;   )
;; 自动保存
(auto-save-mode t)
(auto-save-visited-mode t)
;; 打开文件时, 光标自动定位到上次停留的位置
(save-place-mode t)
;; 设置帮助模式窗口
(setq help-window-select t)
;; 输入法相关
(defun +open-ime ()
  (interactive)
  (unless (w32-get-ime-open-status)
    (w32-set-ime-open-status t)))
(defun +close-ime ()
  (interactive)
  (when (w32-get-ime-open-status)
    (w32-set-ime-open-status nil)))
;; 启动时关闭系统输入法
(when (eq system-type 'windows-nt)
  (add-hook 'after-init-hook #'+close-ime))

(defun +toggle-ime ()
  (interactive)
  (if (w32-get-ime-open-status)
      (w32-set-ime-open-status nil)
    (w32-set-ime-open-status t)))
;(global-set-key (kbd "<shift>") #'+toggle-ime)
;;(require 'flypy)
;; 设置内置输入法
;;(setq default-input-method "chinese-flypy")
;; 设置 C-l 的滚动状态
(setq recenter-positions '(middle 0.05 bottom))
;; 设置像素滚动
(pixel-scroll-precision-mode t)

;;浏览器设置 chrome
(when (and (eq system-type 'gnu/linux)
           (string-match
            ".*microsoft.*"
            (shell-command-to-string "uname -r")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))


;;; 加载包设置
(require 'package)
(setq package-archives '(;;("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("gnu" . "http://1.15.88.122/gnu/")
                         ;;("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("melpa-stable" . "http://1.15.88.122/stable-melpa/")
			 ("melpa" . "http://1.15.88.122/melpa/")
			 ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
			 ("gnu-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
			 ;;("gnu-devel" . "https://elpa.gnu.org/devel/")
			 ))
;; 初始化包
(setq package-check-signature nil)
(package-initialize)

;;; use-package 加载
(unless (package-installed-p 'use-package)
  ;; 更新本地缓存
  (package-refresh-contents)
  ;; 下载安装
  (package-install 'use-package))


(require 'use-package)
;;(setq use-package-always-ensure t)

;; quelpa - For those packages which are not in MELPA
(use-package quelpa
  :config ; 在 (require) 之后需要执行的表达式
  (use-package quelpa-use-package) ; 把 quelpa 嵌入 use-package 的宏扩展
  (quelpa-use-package-activate-advice)) ; 启用这个 advice


;;; 收发邮件
;; (use-package wanderlust
;;   :ensure t
;;   :config
;;   (require 'mail))


;;; 输入法设置
(when (string= "fcitx" (getenv "GTK_IM_MODULE"))
  (use-package sis
    :ensure t
    ;; :hook
    ;; enable the /context/ and /inline region/ mode for specific buffers
    ;; (((text-mode prog-mode) . sis-context-mode)
    ;;  ((text-mode prog-mode) . sis-inline-mode))

    :config
    ;; For MacOS
    (sis-ism-lazyman-config

     ;; English input source may be: "ABC", "US" or another one.
     ;; "com.apple.keylayout.ABC"
     "2"

     ;; Other language input source: "rime", "sogou" or another one.
     ;; "im.rime.inputmethod.Squirrel.Rime"
     "1"
     'fcitx5)

    ;; enable the /cursor color/ mode
    (sis-global-cursor-color-mode t)
    ;; enable the /respect/ mode
    (sis-global-respect-mode t)
    ;; enable the /context/ mode for all buffers
    (sis-global-context-mode t)
    ;; enable the /inline english/ mode for all buffers
    (sis-global-inline-mode t)
    )
  )
(unless (string= "fcitx" (getenv "GTK_IM_MODULE"))
  (use-package rime
    :hook
    (org-mode . +open-flypy)
    :config
    ;; 设置输入法
    (defun +open-flypy ()
      (interactive)
      (set-input-method "rime"))
    (setq default-input-method "rime")
    (cond ((eq system-type 'windows-nt)
	   (setq rime-share-data-dir "c:/msys64/mingw64/share/rime-data"))
	  ((eq system-type 'gnu/linux)
	   (setq rime-share-data-dir "/home/liuwei/.local/share/fcitx5/rime")
	   ))
    (setq rime-cursor "˰")
    (set-face-attribute 'rime-preedit-face nil :background "black" :foreground "gray")))


;;; 外观设置
;; 关闭工具栏
(tool-bar-mode -1)
;; 关闭菜单栏
(menu-bar-mode -1)
;; 关闭鼠标滚动标志
(scroll-bar-mode -1)
;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)
;; 设置行号
(global-display-line-numbers-mode 1)
;; 在菜单中添加最近编辑过的文件选项
;;(require 'recentf)
;;(recentf-mode 1)
;;(setq recentf-max-menu-items 10)
;; 设置光标样式为竖线
(setq-default cursor-type 'bar)
;; 设置光标不闪烁
(blink-cursor-mode -1)
;; 高亮所在行
(global-hl-line-mode t)
;; 设置背景透明
;;(add-to-list 'default-frame-alist '(alpha-background . 70))
(when (eq system-type 'windows-nt)
  (set-frame-parameter nil 'alpha '(90 . 100))
  )
(when (eq system-type 'gnu/linux)
  (setq default-frame-alist '((alpha-background . 85)))
  )
;; 打开全屏
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; emacsclient frame
(setq initial-frame-alist default-frame-alist)
(add-to-list 'initial-frame-alist '(display . "127.0.0.1:0.0"))

;; 状态栏显示时间
(display-time-mode t)
(setq display-time-day-and-date t)
(setq system-time-locale "zh_CN.UTF-8")
(setq display-time-format "%Y-%m-%d %A %H:%M")
;; 设置时间区域
;;(set (make-local-variable 'system-time-locale) "")
;;(setq display-time-format "%I " )
;; 打开 BUFFER 时不显示空格开头的 BUFFER
(setq buffer-invisibility-spec nil)
;; 设置主题
(use-package doom-themes
  :config
     (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
           doom-themes-enable-italic t) ; if nil, italics is universally disabled)
     (load-theme 'doom-one-light t)
     (doom-themes-visual-bell-config)
     (doom-themes-neotree-config)
     (doom-themes-org-config))
;; 设置状态栏
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 45
	doom-modeline-icon nil
	doom-modeline-buffer-name t
	doom-modeline-lsp t
	doom-modeline-buffer-modification-icon t
	doom-modeline-major-mode-icon t))
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-center-theme))



;;; 设置字体
;; 判断字体是否存在
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
;; 设置 GUI 下的字体
(defun +init-font (&optional frame)
  (when (display-graphic-p)
    ;; 设置英语字体
    (cl-loop for font in '("Consolas" "Cascadia Code" "SF Mono" "Source Code Pro"
                           "Fira Code" "Menlo" "Monaco" "Dejavu Sans Mono"
                           "Lucida Console" "SAS Monospace")
             when (font-installed-p font)
             return (set-face-attribute
                     'default frame
                     :font (font-spec :family font
                                      :weight 'normal
                                      :slant 'normal
                                      :size (cond ((eq system-type 'gnu/linux) 30)
                                                  ((eq system-type 'windows-nt) 12.5)))))
    ;; 设置 emoji 字体
    (cl-loop for font in '("all-the-icons" "OpenSansEmoji" "Noto Color Emoji" "Segoe UI Emoji"
                           "EmojiOne Color" "Apple Color Emoji" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode
                                      (font-spec :family font
						 :size (cond ((eq system-type 'gnu/linux) 30)
                                                             ((eq system-type 'windows-nt) 12.5)))
                                      nil 'prepend))
    ;; 设置中文字体
    (cl-loop for font in '("Sarasa Mono SC" "微软雅黑 CN" "思源黑体 CN" "思源宋体 CN" 
                           "Source Han Sans CN" "Source Han Serif CN"
                           "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                           "Microsoft Yahei UI" "Microsoft Yahei")
             when (font-installed-p font)
             return (set-fontset-font t '(#x4e00 . #x9fff)
                                      (font-spec :name font
						 :weight 'normal
						 :slant 'normal
						 :size (cond ((eq system-type 'gnu/linux) 32)
                                                             ((eq system-type 'windows-nt) 13.5)))))
    (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
             when (font-installed-p font)
             return (set-fontset-font t '(#x20000 . #x2A6DF)
                                      (font-spec :name font
						 :weight 'normal
						 :slant 'normal
						 :size (cond ((eq system-type 'gnu/linux) 32)
                                                             ((eq system-type 'windows-nt) 12.5)))))))
(if (daemonp)
    (add-hook 'after-make-frame-functions #'+init-font)
  (+init-font nil))

(use-package all-the-icons
  :if (display-graphic-p))

;;; company-mode
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
	company-tooltip-limit 9
	;; 显示编号
	company-show-numbers 9
	;;company-candidates-length 
	;; 延时弹出时间
	company-idle-delay 0.4
	;; 补全字符开始数量
	company-minimum-prefix-length 2
	company-capf--sorted t
	)
  (use-package company-statistics
    :hook
    (after-init . company-statistics-mode)))

;;; yasnippet
(use-package yasnippet
  :hook
  ((prog-mode text-mode) . yas-minor-mode)
  :custom
  (yas-indent-line 'fixed)
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :after (yasnippet))


;;; ivy
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook
  (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (add-to-list 'ivy-ignore-buffers "\\*[[:ascii:]]+\\*")
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-wrap t)
  :bind
  (("C-c C-r" . ivy-resume)))

;;; counsel
(use-package counsel
  :ensure t
  :hook
  (after-init . counsel-mode)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-switch-buffer)
   ("C-x C-b" . counsel-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ))

;;; swiper
(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper-isearch)
   ))

;;; amx
(use-package amx
  :ensure t)

;;; which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))


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
(use-package org-download
  :ensure t
  :hook ((org-mode dired-mode) . org-download-enable)
  :config
  (defun +org-download-method (link)
    (org-download--fullname (org-link-unescape link)))
  (setq org-download-method '+org-download-method)
  (setq org-download-annotate-function (lambda (_link) "")
        org-download-method 'attach
	org-download-screenshot-method "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\""
        ))


;;; org-mode
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



;;; xenops
(use-package xenops)
;;(use-package org-fragtog)
(setq display-mm-dimensions-alist '(("wayland-0" . (286 . 179))))
(display-monitor-attributes-list)

(use-package org
  :ensure t
  :bind (:map org-mode-map
	      ("C-'" . nil)
	      ("M-l" . org-metaright)
	      ("M-h" . org-metaleft)
	      ("C-<tab>" . "M-<tab>")
	      ("C-c o" . counsel-outline)
	      ("M-=" . advance-words-count)
	      )
  :hook
  (
   ;;(org-mode . +open-flypy)
   (org-mode . (lambda () (display-line-numbers-mode -1)))
   (org-mode . (lambda () (org-bullets-mode 1)))
   (org-mode . (lambda () (setq truncate-lines nil)))
   (org-mode . xenops-mode)
   ;;(org-mode . org-fragtog-mode)
   
					;  (org-mode . (lambda () (valign-mode 1)))
   )
  :config
  ;; 设置 ORG 标题样式
  (add-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
  (use-package org-bullets)
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    ;;(custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) "")
    (custom-declare-face '+org-todo-cancel '((t (:color "red"))) "")
    )
  ;; advance-words-count
  (add-to-list 'load-path (concat user-emacs-directory "lisp/advance-words-count"))
  (require 'advance-words-count)
  (defun +words-count--format-message (cons &optional arg)
    "Format a string to be shown for `words-count--message'.
Using the CONS passed form `advance-words-count'. See
`count-lines' & `count-words'. When ARG is specified, display
verbosely."
    (let ((start (car cons))
          (end (cdr cons))
          list)
      (setq list (advance-words-count start end))
      (format
       (if arg
           "
-----------~*~ Words Count ~*~----------

 Characters (without Space) .... %d
 Characters (all) .............. %d
 Number of Lines ............... %d
 ANSCII Words .................. %d
%s
========================================
"
	 "字符（除空格）：%d, 字符（带空格）：%d, 行数：%d, ANSCI 词数：%d, %s")
       (cadr list)
       (- end start)
       (count-lines start end)
       (car (last list))
       (if (= 0 (car list))
           (format (if arg
                       " Latin Words ................... %d\n"
                     "拉丁字母词数：%d")
                   (count-words start end))
	 (format (if arg
                     " CJK Chars ..................... %d
 Word Count .................... %d\n"
                   "中文：%d, 中英文总字数：%d")
		 (car list)
		 (+ (car list) (car (last list))))))))
  (advice-add 'words-count--format-message :override #'+words-count--format-message)
  ;; 设置表格对齐
  ;; (use-package valign
  ;;  :config
  ;;  (use-package ftable))
  (setq org-modules-loaded t)
  ;; 设置 ORG-KEYWORDS
  (setq org-todo-keywords '((sequence "TODO(t)"
				      "|"
				      "DONE(d)"
				      "Kill(k)"))
	org-todo-keyword-faces '(("[-]"  . +org-todo-active)
				 ("Kill" . "red")))
  ;;时间戳设置
  (setq org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
  ;; 启动时缩进
  (setq org-startup-indented t)
  ;; 显示上下标
  (org-toggle-pretty-entities)
  ;; 上下标控制
  (setq org-export-with-sub-superscripts '{}
	org-use-sub-superscripts '{}
	org-use-sub-superscripts '{}
	org-hide-emphasis-markers t
	org-pretty-entities t)
  ;; 启动时折叠内容
  (setq org-startup-folded t)
  ;; TODO 结束时加上时间
  (setq org-log-done 'time)
  (setq org-directory "~/org/")
  (setq org-agenda-files (cond ((eq system-type 'gnu/linux) '("~/org/agenda/"))
			       ((eq system-type 'windows-nt) '("d:/onedrive/OneDrive - cumt.edu.cn/org/agenda"))))
  (setq org-ellipsis "⤵")
  ;; 设置 ORG 标题范围，使上面的尾标可以正确显示
  (setq org-cycle-separator-lines 1)
  ;; 设置 ORG 可以通过 <s TAB 插入模板
  ;;(add-to-list 'org-modules 'org-tempo t)
  ;; 加入bilibili视频，其中链接为Bv号
  (add-to-list 'org-link-abbrev-alist '("bilibili" . "https://www.bilibili.com/video/%s") t)
  ;; 加入萌百链接
  (add-to-list 'org-link-abbrev-alist '("mengbai" . "https://mzh.moegirl.org.cn/zh-hans/%s") t)
  ;; 设置 latex 图片缩放比例
  (plist-put org-format-latex-options :scale 1.5)
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :foreground 'default)
  ;; org 中图片大小
  (setq org-image-actual-width nil)
  (setq org-preview-latex-process-alist
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
  (setq org-preview-latex-default-process 'xdvsvgm)
  (setq org-preview-latex-image-directory "~/org/ltximg/")
  (require 'ox-latex)
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
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; 设置默认的class为ctexart
  (setq org-latex-default-class "ctexart")
  (setq org-latex-pdf-process
	'(
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "rm -fr %b.out %b.log %b.tex auto"
          ))
  (use-package org-elp
  :config
  (setq org-elp-idle-time 0.5
        org-elp-split-fraction 0.25))
  ;; (setq org-capture-templates
  ;;       '(("t" "TODO" entry (file+headline as/gtd "Collect")
  ;;       "* TODO %? %^G \n  %U" :empty-lines 1)
  ;;       ("s" "Scheduled TODO" entry (file+headline as/gtd "Collect")
  ;;       "* TODO %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 1)
  ;;       ("d" "Deadline" entry (file+headline as/gtd "Collect")
  ;;           "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
  ;;       ("p" "Priority" entry (file+headline as/gtd "Collect")
  ;;       "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
  ;;       ("a" "Appointment" entry (file+headline as/gtd "Collect")
  ;;       "* %? %^G \n  %^t")
  ;;       ("l" "Link" entry (file+headline as/gtd "Collect")
  ;;       "* TODO %a %? %^G\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
  ;;       ("n" "Note" entry (file+headline as/gtd "Notes")
  ;;           "* %? %^G\n%U" :empty-lines 1)
  ;;       ("j" "Journal" entry (file+datetree "/Users/andrew/org/agenda/journal.org")
  ;;       "* %? %^G\nEntered on %U\n")))
  )


;;; denote
(use-package denote
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
	denote-prompts '(title keywords))
  )



;;; aas
;; (use-package aas
;;   :hook
;;   (LaTeX-mode . aas-activate-for-major-mode)
;;   (latex-mode . aas-activate-for-major-mode)
;;   (org-mode . aas-activate-for-major-mode)
;;   )


;;; laas
(use-package laas
  :ensure t
  :hook
  (LaTeX-mode . laas-mode)
  (org-mode . laas-mode)
  :config				; do whatever here
  (aas-set-snippets 'laas-mode
    ;; set condition!
    :cond #'texmathp			; expand only while in math
    ;;"supp" "\\supp"
    ;; bind to functions!
    "Sum" (lambda () (interactive)
            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
    "Span" (lambda () (interactive)
             (yas-expand-snippet "\\Span($1)$0"))
    ;; add accent snippets
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))


;;; avy
(use-package avy
  :ensure t
  :bind (:map global-map
	      ("C-'" . avy-goto-char-timer) ; Control + 单引号
              ;; 复用上一次搜索
              ("C-c C-j" . avy-resume))
  :config
  (setq avy-background t    ; 打关键字时给匹配结果加一个灰背景，更醒目
        avy-all-windows t   ; 搜索所有 window，即所有「可视范围」
        avy-timeout-seconds 0.3)) ; 「关键字输入完毕」信号的触发时间


;;; 快速选中区域
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))


;;; 彩虹括号
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))


;;; paredit
(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'sly-mrepl-mode-hook 'paredit-mode)
  ;(add-hook 'rust-mode-hook 'paredit-mode)
  )


;;; sly
(use-package sly
  :defer t
  :bind
  (:map sly-mode-map
	("C-c C-x C-b" . sly-eval-buffer))
  :config
  (add-hook 'sly-mrepl-mode-hook #'company-mode)
  (add-hook 'lisp-mode-hook #'sly)
  (setq inferior-lisp-program "sbcl")
  (setq sly-contribs '(sly-fancy)))


;;; magit
(use-package magit
  :defer t)

;;; rust
;; rust-mode
(use-package rust-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))


;;; eglot
(use-package eglot
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c++-ts-mode c-mode) "clangd")))
(use-package eldoc)


;;; dirvish
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dired-mouse-drag-files t)   
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind	     ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map	   ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump)	; remapped `describe-mode'
   ("s"   . dirvish-quicksort)	; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)	; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

;;; tramp
;; (use-package tramp
;;   :config
;;   ;; Enable full-featured Dirvish over TRAMP on certain connections
;;   ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
;;   (add-to-list 'tramp-connection-properties
;;                (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
;;                      "direct-async-process" t))
;;   ;; Tips to speed up connections
;;   (setq tramp-verbose 0)
;;   (setq tramp-chunksize 2000)
;;   (setq tramp-use-ssh-controlmaster-options nil))


;;; helpful
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;;; elisp-demos
(use-package elisp-demos
  :ensure t
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  )


;;; eshell
(use-package eshell
  :hook
  ;;取消下划线
  (eshell-mode . (lambda () (setq global-hl-line-mode nil))))

;;; vterm
(use-package vterm
  :ensure t
  :hook
  (vterm-mode . (lambda () (global-hl-line-mode -1))))

;;; server
(use-package server
  :config
  (if (not (server-running-p))
      (server-start))
  (add-hook 'server-switch-hook #'(lambda ()
				    (select-frame-set-input-focus (selected-frame))))
  (add-hook 'server-switch-hook #'raise-frame)
  )



;;; tree-sitter
(use-package tree-sitter
  :hook
  (prog-mode . turn-on-tree-sitter-mode)
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (require tree-sitter-langs)
  (setq major-mode-remap-alist
	'((c++-mode . c++-ts-mode)))
  )


;;; fanyi
(use-package fanyi
  :ensure t)




;; (require 'with-proxy )
;; (with-proxy
;;     :http-server "172.17.224.1:10811"
;;     ;;(call-interactively #'all-the-icons-install-fonts)
;;     (package-install 'tree-sitter)
;;     (package-install 'tree-sitter-langs)
;;     )
;; (with-proxy
;;   :http-server "172.17.224.1:10811"
;;   )


;;; 自定义函数
;;  快速打开配置
(defun open-init-file ()
  "快速打开配置文件"
  (interactive)
  (find-file "c:/Users/qinmo/.emacs.d/init.el"))


;;; 自定义快捷键
(global-set-key (kbd "C-;") #'set-mark-command) 
;; 设置 SHIFT 中英转换
(global-set-key (kbd "<shift>") #'toggle-input-method)
(global-set-key (kbd "M-\\") #'toggle-input-method)
;; 设置 HELP-MODE 中的快捷键
(define-key help-mode-map (kbd "n") (kbd "C-n"))
(define-key help-mode-map (kbd "p") (kbd "C-p"))
;; 设置 READ—ONLY-MODE 中的快捷键
(defvar +read-only-mode-map (make-sparse-keymap)
  "keymap for my read-only mode.")
(define-key +read-only-mode-map (kbd "n") (kbd "C-n"))
(define-key +read-only-mode-map (kbd "p") (kbd "C-p"))
(define-key +read-only-mode-map (kbd "ll") (kbd "C-l"))
(define-key +read-only-mode-map (kbd "f") (kbd "C-f"))
(define-key +read-only-mode-map (kbd "b") (kbd "C-b"))
(define-key +read-only-mode-map (kbd "i") #'read-only-mode)



;;; 自定义的 HOOK
;; (add-hook 'read-only-mode-hook #'(lambda ()
;; 				   (use-local-map +read-only-mode-map)))

;;; 自定义的 ADVICE
(advice-add 'read-only-mode :around #'(lambda (orig-fun &rest args)
					(cond ((and (boundp 'read-only-mode--state)
						    read-only-mode--state)
					       (let ()
						 (apply orig-fun args)
						 (use-local-map nil)))
					      (t
					       (apply orig-fun args)
					       (use-local-map +read-only-mode-map)))))


;;; 设置 wsl 下的复制粘贴
(when (and (getenv "WAYLAND_DISPLAY") (not (equal (getenv "GDK_BACKEND") "x11")))
  (unless (executable-find "wl-copy")
    (setq system-packages-use-sudo (executable-find "sudo")) ;container doesn't have "sudo" command
    (shell-command "apt update")
    (system-packages-install "wl-clipboard" " -y")
    )
  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
					:buffer nil
					:command '("wl-copy" "-f" "-n")
					:connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
	nil	  ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modules nil)
 '(package-selected-packages
   '(auto-package-update company company-statistics denote diminish
			 doom-modeline doom-themes elisp-demos
			 expand-region fanyi ftable
			 gnu-elpa-keyring-update helpful ivy laas
			 magit org-download org-elp org-fragtog
			 ox-latex powerline quelpa quelpa-use-package
			 rime rust-mode sis "tree-sitter" tree-sitter
			 tree-sitter-langs use-package valign vterm
			 wanderlust with-proxy))
 '(warning-suppress-log-types '(((defvaralias losing-value org-tab-first-hook)) (comp)))
 '(warning-suppress-types '((use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )




