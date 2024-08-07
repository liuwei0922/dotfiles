;;; init-ui.el --- Emacs ui config                   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'init-utils)
;; 设置字体
;; 判断字体是否存在
(defun +font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name))) 
;; 设置 GUI 下的字体
(defun +init-font ()
  (when (display-graphic-p)  
    ;; 设置英语字体
    (cl-loop for font in '("Fira Code"  "FiraCode Nerd Font" "iosevka"
			   "Consolas" "Cascadia Code" "SF Mono" "Source Code Pro"
                            "Menlo" "Monaco" "Dejavu Sans Mono"
                           "Lucida Console" "SAS Monospace")
             when (+font-installed-p font)
             return (set-face-attribute
                     'default nil
                     :font (font-spec :family font
                                      :weight 'normal
                                      :slant 'normal
                                      :size (cond ((eq system-type 'gnu/linux) 36)
                                                  ((eq system-type 'windows-nt) 12.5)))))

    (cl-loop for font in '("Sarasa UI SC" "Symbol" "Symbola" "Segoe UI Symbol"  )
             when (+font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))
    
    ;; 设置 emoji 字体
    (cl-loop for font in '("Sarasa UI SC" "nerd-icons" "Symbola" "Noto Color Emoji"  
                           "EmojiOne Color" "Apple Color Emoji" "Segoe UI Emoji"
			   "OpenSansEmoji" )
             when (+font-installed-p font) 
             return (set-fontset-font t 'emoji
                                      (font-spec :family font)
                                      nil 'prepend)) 
    ;; 设置中文字体
    (cl-loop for font in '("LXGW Wenkai" "Sarasa Mono SC" "微软雅黑 CN" "思源黑体 CN" "思源宋体 CN" 
                           "Source Han Sans CN" "Source Han Serif CN"
                           "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                           "Microsoft Yahei UI" "Microsoft Yahei")
             when (+font-installed-p font)
             return (set-fontset-font t 'chinese-gbk ;;'(#x4e00 . #x9fff)
                                      (font-spec :name font
						 :weight 'normal
						 :slant 'normal
						 :size (cond ((eq system-type 'gnu/linux) 36)
                                                             ((eq system-type 'windows-nt) 13.5)))))
    ;; (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
    ;;          when (+font-installed-p font)
    ;;          return (set-fontset-font t '(#x20000 . #x2A6DF)
    ;;                                   (font-spec :name font
    ;; 						 :weight 'normal
    ;; 						 :slant 'normal
    ;; 						 :size (cond ((eq system-type 'gnu/linux) 32)
    ;;                                                          ((eq system-type 'windows-nt) 12.5)))))
    ))

(+init-font)



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
(setq initial-frame-alist default-frame-alist)
;;(add-to-list 'initial-frame-alist '(display . "127.0.0.1:0.0"))
;; 设置背景透明
;;(add-to-list 'default-frame-alist '(alpha-background . 70))
(when (eq system-type 'windows-nt)
  (set-frame-parameter nil 'alpha '(90 . 100))
  )
(when (eq system-type 'gnu/linux)
  (setq default-frame-alist '((alpha-background . 85)))
  )
;; 在菜单中添加最近编辑过的文件选项
(use-package recentf
  :config
  (setq recentf-save-file (concat user-emacs-directory "recentf"))
  ;; 不自动清理 recentf 记录。
  (setq recentf-auto-cleanup 'never)
  ;; emacs 退出时清理 recentf 记录。
  ;;(add-hook 'kill-emacs-hook #'recentf-cleanup)
  ;; 每 5min 以及 emacs 退出时保存 recentf-list。
  ;;(run-at-time nil (* 5 60) 'recentf-save-list)
  ;;(add-hook 'kill-emacs-hook #'recentf-save-list)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 200) ;; default 20
  ;; recentf-exclude 的参数是正则表达式列表，不支持 ~ 引用家目录。
  ;; emacs-dashboard 不显示这里排除的文件。  
  (setq recentf-exclude `(,(recentf-expand-file-name "~\\(straight\\|ln-cache\\|etc\\|var\\|.cache\\|backup\\|elfeed\\)/.*")
                          ;;,(recentf-expand-file-name "~\\(recentf\\|bookmarks\\|archived.org\\)")
                          ,tramp-file-name-regexp ;; 不在 recentf 中记录 tramp 文件，防止 tramp 扫描时卡住。
                          "^/tmp" "\\.bak\\'" "\\.gpg\\'" "\\.gz\\'" "\\.tgz\\'" "\\.xz\\'" "\\.zip\\'" "^/ssh:" "\\.png\\'"
                          "\\.jpg\\'" "/\\.git/" "\\.gitignore\\'" "\\.log\\'" "COMMIT_EDITMSG" "\\.pyi\\'" "\\.pyc\\'"
                          "/private/var/.*" "^/usr/local/Cellar/.*" ".*/vendor/.*"
                          ,(concat package-user-dir "/.*-autoloads\\.egl\\'")))
  (recentf-mode +1))

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
  :ensure t
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
  (doom-modeline-height 90)
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
;; (use-package all-the-icons
;;   :ensure t
;;   :if (display-graphic-p))
;; (use-package nerd-icons
;;   :ensure nil
;;   :custom
;;   (nerd-icons-font-family "Sarasa Term SC Nerd")
;;   :config
;;   (when (and (display-graphic-p)
;; 	     (not (+font-installed-p nerd-icons-font-family)))
;;     (nerd-icons-install-fonts t))
;;   )

(use-package kind-icon
  :ensure t
  :after corfu
  ;; :custom
  ;; (kind-icon-blend-background t)
  ;; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (setq kind-icon-mapping
	'((array "a" :icon "symbol-array" :face font-lock-type-face :collection "vscode")
          (boolean "b" :icon "symbol-boolean" :face font-lock-builtin-face :collection "vscode")
          (color "#" :icon "symbol-color"  :face success :collection "vscode")
          (command "cm"  :icon "chevron-right" :face default :collection "vscode")
          (constant "co"  :icon "symbol-constant" :face font-lock-constant-face :collection "vscode")
          (class "c"   :icon "symbol-class" :face font-lock-type-face :collection "vscode")
          (constructor "cn"  :icon "symbol-method" :face font-lock-function-name-face :collection "vscode")
          (enum "e"   :icon "symbol-enum" :face font-lock-builtin-face :collection "vscode")
          (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face :collection "vscode")
          (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face :collection "vscode")
          (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face :collection "vscode")
          (field "fd"  :icon "symbol-field" :face font-lock-variable-name-face :collection "vscode")
          (file  "f"   :icon "symbol-file"        :face font-lock-string-face  :collection "vscode")
          (folder         "d"   :icon "folder"             :face font-lock-doc-face :collection "vscode")
          (function  "f"   :icon "symbol-method"      :face font-lock-function-name-face :collection "vscode")
          (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face  :collection "vscode")
          (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face :collection "vscode")
          (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
          (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face :collection "vscode")
          (method "m"   :icon "symbol-method"      :face font-lock-function-name-face  :collection "vscode")
          (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
          (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face :collection "vscode")
          (operator "op"  :icon "symbol-operator" :face font-lock-comment-delimiter-face :collection "vscode")
          (param          "pa"  :icon "gear"               :face default :collection "vscode")
          (property "pr" :icon "symbol-property" :face font-lock-variable-name-face :collection "vscode")
          (reference "rf"  :icon "library"  :face font-lock-variable-name-face     :collection "vscode")
          (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face :collection "vscode")
          (string         "s"   :icon "symbol-string"      :face font-lock-string-face :collection "vscode")
          (struct "%" :icon "symbol-structure" :face font-lock-variable-name-face :collection "vscode")
          (text  "tx"  :icon "symbol-key"         :face font-lock-doc-face      :collection "vscode")
          (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face  :collection "vscode")
          (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face  :collection "vscode")
          (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face :collection "vscode")
          (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face  :collection "vscode")
          (variable "va" :icon "symbol-variable" :face font-lock-variable-name-face     :collection "vscode")
          (t              "."   :icon "question"           :face font-lock-warning-fac :collection "vscode")))
  (when (eq +system-type 'wsl)
    (plist-put kind-icon-default-style :height 0.4))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; 启动时关闭系统输入法
(when (eq system-type 'windows-nt)
  (add-hook 'after-init-hook #'+close-ime)
  (defun +open-ime ()
    (interactive)
    (unless (w32-get-ime-open-status)
      (w32-set-ime-open-status t)))
  (defun +close-ime ()
    (interactive)
    (when (w32-get-ime-open-status)
      (w32-set-ime-open-status nil)))
  (defun +toggle-ime ()
    (interactive)
    (if (w32-get-ime-open-status)
	(w32-set-ime-open-status nil)
      (w32-set-ime-open-status t)))
  (+close-ime))
(when (eq system-type 'gnu/linux)
  ;; 由于 fcitx 的显示不是很好，所以暂时在 wsl 中 emacs 中不使用系统输入法
  ;;(setq pgtk-use-im-context-on-new-connection nil)
  )
;; rime 输入法
(use-package rime
  :ensure t
  :unless (eq system-type 'windows-nt)
  :custom
  (default-input-method "rime")
  (rime-cursor "˰")
  (rime-disable-predicates
   '(rime-predicate-prog-in-code-p
     ;;rime-predicate-punctuation-after-ascii-p
     ;;rime-predicate-current-uppercase-letter-p     
     ;;rime-predicate-after-alphabet-char-p
     ))
  (rime-inline-predicates
   '(rime-predicate-space-after-cc-p
     rime-predicate-after-ascii-char-p
     ))
  :bind
  (:map rime-mode-map
   ("M-j" . rime-force-enable))
  :hook
  (kill-emacs . rime-lib-finalize)
  :config
  (when (eq +linux-type 'nixos)
    (setq rime-emacs-module-header-root
     (file-truename (concat
		(file-name-directory (directory-file-name (file-truename invocation-directory)))
		"include")))
    (setq rime-librime-root "~/.local/state/nix/profiles/home-manager/home-path"))
  (defun +open-flypy ()
    (interactive)
    (set-input-method "rime"))
  (cond ((eq system-type 'windows-nt)
	 (setq rime-share-data-dir "~/Rime"))
	((eq system-type 'gnu/linux)
	 (setq rime-share-data-dir "~/.local/share/fcitx5/rime")
	 (setq rime-user-data-dir "~/.config/fcitx5/rime")
	 ))  
  (set-face-attribute 'rime-preedit-face nil :background "black" :foreground "gray"))


;; 彩虹括号
(use-package rainbow-delimiters
  :ensure t
  :hook
  (lisp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode))

;; helpful
(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . #'helpful-callable)
  ([remap describe-command] . #'helpful-command)
  ([remap describe-variable] . #'helpful-variable)
  ([remap describe-key] . #'helpful-key))

;; elisp-demos
(use-package elisp-demos
  :ensure t
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  )


;; dirvish
(use-package dirvish
  :ensure t
  :hook
  (after-init . dirvish-override-dired-mode)
  :config
  (setq dired-mouse-drag-files t)   
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
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


(provide 'init-ui)
;;; init-ui.el ends here
