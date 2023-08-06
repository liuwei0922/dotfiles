;;; 设置用户名和邮箱
(setq user-full-name "qinmoxiao")
(setq user-mail-address "qinmoxiao@qq.com")


;;; 加载配置路径
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

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

;;


;;; 加载包设置
(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ;;("melpa" . "http://1.15.88.122/melpa/")
			 ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
			 ("gnu-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
			 ("gnu-devel" . "https://elpa.gnu.org/devel/")))
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
(setq use-package-always-ensure t)

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
(use-package rime
  :config
  (setq default-input-method "rime")
  (cond ((eq system-type 'windows-nt)
	 (setq rime-share-data-dir "c:/msys64/mingw64/share/rime-data"))
	((eq system-type 'gnu/linux)
	 (setq rime-share-data-dir "/home/liuwei/.local/share/fcitx5/rime")
	 ))
  (setq rime-cursor "˰")
  (set-face-attribute 'rime-preedit-face nil :background "black" :foreground "gray"))

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
;; 打开全屏
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; 设置背景透明
;;(add-to-list 'default-frame-alist '(alpha-background . 70))
(set-frame-parameter nil 'alpha '(90 . 100))
;; 状态栏显示时间
(display-time-mode t)
(setq display-time-day-and-date t)
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
  (setq doom-modeline-icon nil
	doom-modeline-height 35
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
(when (display-graphic-p)
  ;; 设置英语字体
  (cl-loop for font in '("Consolas" "Cascadia Code" "SF Mono" "Source Code Pro"
                         "Fira Code" "Menlo" "Monaco" "Dejavu Sans Mono"
                         "Lucida Console" "SAS Monospace")
           when (font-installed-p font)
           return (set-face-attribute
                   'default nil
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
  (cl-loop for font in '("思源黑体 CN" "思源宋体 CN" "微软雅黑 CN"
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
                                                           ((eq system-type 'windows-nt) 12.5))))))
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
	company-capf--sorted t)
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
   ("C-h v" . counsel-describe-variable))
  )

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
;; 设置输入法
(defun +open-flypy ()
  (interactive)
  (set-input-method "rime"))

(use-package org
  :ensure t
  :bind (:map org-mode-map
	      ("C-'" . nil)
	      ("M-l" . org-metaright)
	      ("M-h" . org-metaleft)
	      ("C-<tab>" . "M-<tab>"))
  :hook
  (
  (org-mode . +open-flypy)
  (org-mode . (lambda () (display-line-numbers-mode -1)))
  (org-mode . (lambda () (org-bullets-mode 1)))
  (org-mode . (lambda () (setq truncate-lines nil)))
;  (org-mode . (lambda () (valign-mode 1)))
  )
  :config
  ;; 设置 ORG 标题样式
  (add-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
  (use-package org-bullets)
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))
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
				 ("KILL" . +org-todo-cancel)))
  ;;时间戳设置
  (setq system-time-locale "zh_CN.UTF-8")
  (setq org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
  ;; 启动时缩进
  (setq org-startup-indented t)
  ;; 显示上下标
  (org-toggle-pretty-entities)
  ;; 上下标控制
  (setq org-export-with-sub-superscripts '{}
	org-use-sub-superscripts '{}
	org-use-sub-superscripts '{}
	org-hide-emphasis-markers t)
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
  (plist-put org-format-latex-options :scale 1.5))



;;; avy
(use-package avy
  :ensure t
  :bind (:map global-map
	 ("C-'" . avy-goto-char-timer) ; Control + 单引号
         ;; 复用上一次搜索
         ("C-c C-j" . avy-resume))
  :config
  (setq avy-background t ; 打关键字时给匹配结果加一个灰背景，更醒目
        avy-all-windows t ; 搜索所有 window，即所有「可视范围」
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


;;; dirvish
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
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
(use-package tramp
  :config
  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
                     "direct-async-process" t))
  ;; Tips to speed up connections
  (setq tramp-verbose 0)
  (setq tramp-chunksize 2000)
  (setq tramp-use-ssh-controlmaster-options nil))


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


;;; eshell
(use-package eshell
  :hook
  ;;取消下划线
  (eshell-mode . (lambda () (setq global-hl-line-mode nil))))

;;; server
(use-package server
  :config
  (if (not (server-running-p))
      (server-start)))


;;; tree-sitter
(use-package tree-sitter
    :hook
    (prog-mode . turn-on-tree-sitter-mode)
    (tree-sitter-after-on . tree-sitter-hl-mode)
    :config
    (require tree-sitter-langs)
    (setq major-mode-remap-alist
	  '((c++-mode . c++-ts-mode))))
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

;; (use-package tree-sitter
;;   :ensure t
;;   :hook
;;   (after-init . global-tree-sitter-mode)
;;   (tree-sitter-after-on . tree-sitter-hl-mode)
;;   :config
;;   )


;;; 自定义函数
;;  快速打开配置
(defun open-init-file ()
  "快速打开配置文件"
  (interactive)
  (find-file "c:/Users/qinmo/.emacs.d/init.el"))



;;; 自定义的 HOOK


;;; 自定义快捷键
(global-set-key (kbd "C-;") #'set-mark-command) 
;; 设置 SHIFT 中英转换
(global-set-key (kbd "<shift>") #'toggle-input-method)
(global-set-key (kbd "M-\\") #'toggle-input-method)
;; 设置 HELP-MODE 中的快捷键
(define-key help-mode-map (kbd "n") (kbd "C-n"))
(define-key help-mode-map (kbd "p") (kbd "C-p"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modules nil)
 '(package-selected-packages
   '(tree-sitter-langs "tree-sitter" tree-sitter with-proxy helpful rime
		       rust-mode ftable company-statistics wanderlust
		       valign powerline quelpa-use-package quelpa
		       diminish doom-themes expand-region
		       gnu-elpa-keyring-update doom-modeline magit
		       use-package ivy company))
 '(warning-suppress-log-types '(((defvaralias losing-value org-tab-first-hook)) (comp)))
 '(warning-suppress-types '((use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


