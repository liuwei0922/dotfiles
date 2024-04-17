;;; init-base.el --- Emacs init config               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  qinmoxiao

;; Author: qinmoxiao <qinmoxiao@qq.com>
;; Keywords: 


;;; Commentary:


;;; Code:

(require 'init-utils)

;;; 设置用户名和邮箱
(setq user-full-name "qinmoxiao")
(setq user-mail-address "qinmoxiao@qq.com")


;;; 文件相关设置
;; 设置环境变量
(when (eq +system-type 'windows-nt)
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
  ;; (setq locale-coding-system 'gb18030)  ;此句保证中文字体设置有效
  ;; (setq w32-unicode-filenames 'nil)       ; 确保file-name-coding-system变量的设置不会无效
  ;; (setq file-name-coding-system 'gb18030) ; 设置文件名的编码为gb18030
  )
;; 默认文件编码
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8-unix)
;; 自动保存
(add-hook 'find-file-hook
	  (lambda ()
	    (auto-save-mode t)
	    (auto-save-visited-mode t)))
;; 禁止 NATIVE-COMPILE
(setq no-native-compile t)
;; 禁止Emacs自动生成备份文件
(setq make-backup-files nil)
;; 选中一段文字后，输入一个字符会替换掉选中文字
(delete-selection-mode 1)
;; 替换yes/no为y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
(setq y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)
;; 设置聚焦帮助模式窗口
(setq help-window-select t)
;; 设置 C-l 的滚动状态
(setq recenter-positions '(middle 0.05 bottom))
;; 设置像素滚动
(pixel-scroll-precision-mode t)
;; 浏览器设置 chrome
(if (eq +system-type 'wsl)
    (setq browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
	  browse-url-generic-args     '("/c" "start")
	  browse-url-browser-function #'browse-url-generic))
;; Pixelwise resize
(setq ;;window-resize-pixelwise t
      frame-resize-pixelwise t
      )
;; No lock files
(setq create-lockfiles nil)

;; Cutting and pasting use primary/clipboard
(setq select-enable-primary t
      select-enable-clipboard t)
;; 设置 wsl 下的复制粘贴
;; (when (eq +system-type 'wsl)
;;   (set-clipboard-coding-system 'gbk-dos))
(when ;;(and (getenv "WAYLAND_DISPLAY") (not (equal (getenv "GDK_BACKEND") "x11")))
    (eq +system-type 'wsl)
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
  (setq interprogram-cut-function #'wl-copy)
  (setq interprogram-paste-function #'wl-paste))

;; No gc for font caches
(setq inhibit-compacting-font-caches t)

;; Improve display
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)

;; No annoying bell
(setq ring-bell-function 'ignore)

;; No eyes distraction
(setq blink-cursor-mode nil)

;; 打开 BUFFER 时不显示空格开头的 BUFFER
(setq buffer-invisibility-spec nil)

(use-package isearch
  :ensure nil
  :config
  ;; 这样可以在literal的isearch中，把空格直接当成正则里面的.*匹
  (setq isearch-lax-whitespace t)
  (setq search-whitespace-regexp ".*")
  ;; 在搜正则时不开启这个功能，空格就是空格
  (setq isearch-regexp-lax-whitespace nil)
  ;; 搜到尾然后从头开始
  (defun isearch-no-fail ()
    (unless isearch-success
      ;;(ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
      ;;(ad-activate 'isearch-search)
      (isearch-repeat (if isearch-forward 'forward))
      ;;(ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
      ;;(ad-activate 'isearch-search)
      ))
  (advice-add 'isearch-search :after #'isearch-no-fail)
  :bind
  (:map isearch-mode-map
	("DEL" . isearch-del-char)))


;;; autorevert
(use-package autorevert
  :after org-agenda
  :config
  (add-hook 'org-mode-hook #'turn-on-auto-revert-mode))


;;; server
(use-package server
  :ensure t
  :hook
  (server-switch . (lambda ()
		     (select-frame-set-input-focus (selected-frame))))
  (server-switch . raise-frame)
  (server-after-make-frame . (lambda ()
			       ;; 全屏
			       (let ((fullscreen (frame-parameter nil 'fullscreen)))
				 (unless (eq fullscreen 'maximized)
				   (set-frame-parameter nil 'fullscreen 'maximized)))
			       (+init-font)
			       (when (fboundp 'set-scroll-bar-mode)
				 ;; 关闭鼠标滚动标志
				 (set-scroll-bar-mode nil))
			       (if (display-graphic-p)
				   (corfu-terminal-mode -1)
				 (corfu-terminal-mode 1)
				 )
			       ))
  (server-after-make-frame . (lambda ()
			       (if (display-graphic-p)
				   (progn
				     (use-package expand-region
				       :bind
				       ("C-=" . #'er/expand-region)
				       ("C--" . #'er/contract-region))
				     (global-set-key (kbd "C-;") #'set-mark-command)
				     (global-unset-key (kbd "M-;")))
				 (progn
				   (use-package expand-region
				     :bind
				     ("M-=" . #'er/expand-region)
				     ("M--" . #'er/contract-region))
				   (global-set-key (kbd "M-;") #'set-mark-command)))))
  :config
  (if (not (server-running-p))
      (server-start))
  (defun +corfu-terminal ()
    (unless (display-graphic-p)
      (corfu-terminal-mode +1) 
      ))
  ;;(add-hook 'after-make-frame-functions #'+corfu-terminal)
  )



;;; Type text
(use-package text-mode
  :ensure nil
  :custom
  ;; better word wrapping for CJK characters
  (word-wrap-by-category t)
  ;; paragraphs
  (sentence-end-double-space nil))


;; Back to the previous position
;; 打开文件时, 光标自动定位到上次停留的位置
(use-package saveplace
  :ensure nil
  :hook (find-file . save-place-mode))


;; Workaround with minified source files
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))


;; transparent remote access
(use-package tramp
  :ensure nil
  :custom
  ;; Always use file cache when using tramp
  (remote-file-name-inhibit-cache nil)
  (tramp-default-method "sshx")
  :config
  (setq tramp-verbose 10)
  (add-to-list 'tramp-remote-path "/home/lhaaso/wliu/.local/bin")
  (add-to-list 'tramp-remote-path "/home/liuwei/.local/bin")
  ;; (connection-local-set-profile-variables
  ;;  'remote-bash
  ;;  '((explicit-shell-file-name . "/bin/bash")
  ;;    (explicit-bash-args . ("-i"))
  ;;    ))
  ;; (connection-local-set-profiles
  ;;  '(:application tramp :machine "detect" :user "liuwei")
  ;;  'remote-bash)
  ;; (let ((process-environment tramp-remote-process-environment))
  ;;   (setenv "ENV" "$HOME/.profile")
  ;;   (setq tramp-remote-process-environment process-environment))
  )





;; Press C-c s to search
;; (use-package rg
;;   :ensure nil
;;   :hook (after-init . rg-enable-default-bindings))


;; Translator for Emacs
;; M-x fanyi-dwim{,2}, that's all.
(use-package fanyi
  :ensure t
  :commands fanyi-dwim fanyi-dwim2)


;; which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))


;; avy
(use-package avy
  :ensure t
  :bind (:map global-map
	      ("C-'" . avy-goto-char-timer) ; Control + 单引号
              ;; 复用上一次搜索
              ("C-c C-j" . avy-resume))
  :custom
  (avy-background t)    ; 打关键字时给匹配结果加一个灰背景，更醒目
  (avy-all-windows t)   ; 搜索所有 window，即所有「可视范围」
  (avy-timeout-seconds 0.3)) ; 「关键字输入完毕」信号的触发时间





;; paredit
(use-package paredit
  :ensure t
  :defer t
  :hook
  (emacs-lisp-mode . paredit-mode)
  (lisp-mode-hook . paredit-mode)
  (sly-mrepl-mode-hook . paredit-mode)
  ;;(add-hook 'rust-mode-hook 'paredit-mode)
  :bind
  (:map paredit-mode-map
	("M-s" . nil))
  )

;; 快速选中区域
(when (display-graphic-p)
    (use-package expand-region
      :ensure t
      :bind
      ("C-=" . #'er/expand-region)
      ("C--" . #'er/contract-region))
      )
(if (daemonp))


;;; magit
(use-package magit
  :ensure nil
  :defer t)


;; eshell
(use-package eshell
  :ensure nil
  :defer t
  :hook
  ;;取消下划线
  (eshell-mode . (lambda () (setq global-hl-line-mode nil)))
  (eshell-mode . (lambda () (display-line-numbers-mode -1))))

;;; vterm
(use-package vterm
  :ensure nil
  :defer t
  :hook
  (vterm-mode . (lambda () (global-hl-line-mode -1))))

;;; eat
(use-package eat
  :ensure nil
  :defer t
  :hook
  (eat-mode . (lambda ()
		(global-hl-line-mode -1)
		(global-display-line-numbers-mode -1))))


;;; hledger-mode
(use-package hledger-mode
  :ensure nil
  :defer t
  :pin manual
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :commands hledger-enable-reporting
  :custom
  (hledger-currency-string  "¥")
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defface hledger-warning-face
    '((((background dark))
       :background "Red" :foreground "White")
      (((background light))
       :background "Red" :foreground "White")
      (t :inverse-video t))
    "Face for warning"
    :group 'hledger)

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  :bind (("C-c h j" . hledger-run-command)
         :map hledger-mode-map
         ("C-c e" . hledger-jentry)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry))
  :init
  (setq hledger-jfile
        (expand-file-name "~/org/account/account.journal")
        ;; hledger-email-secrets-file (expand-file-name "secrets.el"
        ;;                                              emacs-assets-directory)
	)
  ;; Expanded account balances in the overall monthly report are
  ;; mostly noise for me and do not convey any meaningful information.
  (setq hledger-show-expanded-report nil)

  (when (boundp 'my-hledger-service-fetch-url)
    (setq hledger-service-fetch-url
          my-hledger-service-fetch-url))

  :config
  (add-hook 'hledger-view-mode-hook #'hl-line-mode)
					;(add-hook 'hledger-view-mode-hook #'center-text-for-reading)

  (add-hook 'hledger-view-mode-hook
            (lambda ()
              (run-with-timer 1
                              nil
                              (lambda ()
                                (when (equal hledger-last-run-command
                                             "balancesheet")
                                  ;; highlight frequently changing accounts
                                  (highlight-regexp "^.*\\(savings\\|cash\\).*$")
                                  (highlight-regexp "^.*credit-card.*$"
                                                    'hledger-warning-face))))))

  (add-hook 'hledger-mode-hook
            (lambda ()
              ;;(make-local-variable 'company-backends)
	      (setq-local completion-at-point-functions
			  (list (cape-company-to-capf #'hledger-company)))
              ;;(add-to-list 'company-backends 'hledger-company)
	      ))
  (add-hook 'hledger-mode-hook
	    (lambda ()
	      (setq-local tab-width 4)
	      (setq-local indent-tabs-mode nil)))
  (defun +hledger-insert-date--advice ()
    (insert (org-read-date) " "))
  (advice-add 'hledger-insert-date :override #'+hledger-insert-date--advice))


(use-package hledger-input
  :ensure nil
  :defer t
  :pin manual
  :bind (("C-c h e" . hledger-capture)
         :map hledger-input-mode-map
         ("C-c C-b" . popup-balance-at-point))
  :preface
  (defun popup-balance-at-point ()
    "Show balance for account at point in a popup."
    (interactive)
    (if-let ((account (thing-at-point 'hledger-account)))
        (message (hledger-shell-command-to-string (format " balance -N %s "
                                                          account)))
      (message "No account at point")))

  :config
  (setq hledger-input-buffer-height 20)
  (add-hook 'hledger-input-post-commit-hook #'hledger-show-new-balances)
  (add-hook 'hledger-input-mode-hook #'auto-fill-mode)
  ;; (add-hook 'hledger-input-mode-hook
  ;;           (lambda ()
  ;;             (make-local-variable 'company-idle-delay)
  ;;             (setq-local company-idle-delay 0.1)
  ;; 	      ))
  )

(use-package exec-path-from-shell
  :if (daemonp)
  :ensure t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;;; yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :bind
  (:map yas-minor-mode-map
	("<tab>" . nil)
	("TAB" . nil)
	("C-c y" . yas-expand)
	("<space>" . yas-maybe-expand))
  :hook
  ((prog-mode text-mode) . yas-minor-mode)
  :custom
  (yas-indent-line 'fixed)
  :config
  (yas-global-mode)
  )
(use-package yasnippet-snippets
  :ensure t 
  :after (yasnippet)
  ;; :config
  ;; (defvar company-mode/enable-yas t
  ;;   "Enable yasnippet for all backands")

  ;; (defun company-mode/backend-with-yas (backend)
  ;;   (if (or (not company-mode/enable-yas)
  ;; 	    (and (listp backend)
  ;; 		 (member 'company-yasnippet backend)))
  ;; 	backend
  ;;     (append (if (consp backend) backend (list backend))
  ;; 	      '(:with company-yasnippet))))

  ;; (setq company-backends
  ;; 	(mapcar #'company-mode/backend-with-yas  company-backends))
  )


;; consult-yasnippet
;; (use-package consult-yasnippet
;;   :ensure nil
;;   )

;; (use-package ivy-yasnippet
;;   :ensure t
;;   :after ivy yasnippet
;;   :bind
;;   ("C-<tab>" . ivy-yasnippet))



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

;; 收发邮件
;; (use-package wanderlust
;;   :ensure t
;;   :config
;;   (require 'mail))


(provide 'init-base)
;;; init-base.el ends here
