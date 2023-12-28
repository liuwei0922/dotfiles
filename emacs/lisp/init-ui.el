;;; init-ui.el --- Emacs ui config                   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; 设置字体
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
                                      :size (cond ((eq system-type 'gnu/linux) 34)
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


(when (fboundp 'menu-bar-mode)
  ;; 关闭菜单栏
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  ;; 关闭工具栏
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  ;; 关闭鼠标滚动标志
  (set-scroll-bar-mode nil))
;; 设置光标样式为竖线
(setq-default cursor-type 'bar)
;; 设置光标不闪烁
(blink-cursor-mode -1)
;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)
;; 打开全屏
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; emacsclient frame
(setq initial-frame-alist default-frame-alist)
(add-to-list 'initial-frame-alist '(display . "127.0.0.1:0.0"))
;; 设置背景透明
;;(add-to-list 'default-frame-alist '(alpha-background . 70))
(when (eq system-type 'windows-nt)
  (set-frame-parameter nil 'alpha '(90 . 100))
  )
(when (eq system-type 'gnu/linux)
  (setq default-frame-alist '((alpha-background . 85)))
  )
;; 在菜单中添加最近编辑过的文件选项
;;(require 'recentf)
;;(recentf-mode 1)
;;(setq recentf-max-menu-items 10)

;; 设置行号
(global-display-line-numbers-mode 1)
;; Show line/column number and more
(use-package simple
  :ensure nil
  :custom
  ;; show line/column/filesize in modeline
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  ;; No visual feedback on copy/delete.
  (copy-region-blink-delay 0)
  (delete-pair-blink-delay 0)
  ;; confusing if no fringes (GUI only).
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  ;; don't save current clipboard text before replacing it
  (save-interprogram-paste-before-kill nil)
  ;; eliminate duplicates
  (kill-do-not-save-duplicates t)
  ;; include '\n' when point starts at the beginning-of-line
  (kill-whole-line t)
  ;; show cwd when `shell-command' and `async-shell-command'
  ;;(shell-command-prompt-show-cwd t)
  ;; show the name of character in `what-cursor-position'
  (what-cursor-show-names t)
  ;; List only applicable commands.
  ;;
  ;; ``` elisp
  ;; (defun foo ()
  ;;   (interactive nil org-mode)
  ;;   (message "foo"))
  ;; ```
  ;;
  ;; M-x foo should only be available in `org-mode` or modes derived from `org-mode`.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Highlight current line in GUI
(use-package hl-line
  :ensure nil
  :when (display-graphic-p)
  :hook (after-init . global-hl-line-mode))


;; 设置主题
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t) ; if nil, bold is universally disabled
  (doom-themes-enable-italic t); if nil, italics is universally disabled
  :config
  (load-theme 'doom-one-light t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; 设置状态栏
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-center-theme))
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 45)
  (doom-modeline-icon nil)
  (doom-modeline-buffer-name t)
  (doom-modeline-lsp t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-major-mode-icon t))
;; 状态栏显示时间
(display-time-mode t)
(setq display-time-day-and-date t)
(setq system-time-locale "zh_CN.UTF-8")
(setq display-time-format "%Y-%m-%d %A %H:%M")

;; all-the-icons
(use-package all-the-icons
  :if (display-graphic-p))


(provide 'init-ui)
;;; init-ui.el ends here
