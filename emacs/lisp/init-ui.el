;;; init-ui.el --- Emacs ui config                   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'init-utils)
;; 设置字体
;; 判断字体是否存在
(defun +font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :family font-name)))
;;(+font-installed-p "霞鹜文楷") 
;; 设置 GUI 下的字体
(defvar +english-font-size (cond ((eq system-type 'gnu/linux) 36)
				 ((eq system-type 'windows-nt) 12.5)))
(defun +init-font (size)
  (when (display-graphic-p)  
    ;; 设置英语字体
    (cl-loop for font in '("Fira Code"  "FiraCode Nerd Font" "Iosevka NF")
             when (+font-installed-p font)
             return (set-face-attribute
                     'default nil
                     :font (font-spec :family font
                                      :weight 'normal
                                      :slant 'normal
                                      :size size)))
    ;; 设置中文字体
    (cl-loop for font in '("霞鹜文楷" "LXGW Wenkai" "Sarasa Mono SC" "微软雅黑 CN" "思源黑体 CN" "思源宋体 CN" 
                           "Source Han Sans CN" "Source Han Serif CN"
                           "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                           "Microsoft Yahei UI" "Microsoft Yahei")
             when (+font-installed-p font)
             return (set-fontset-font t 'chinese-gbk ;;'(#x4e00 . #x9fff)
                                      (font-spec :family font)))

    (cl-loop for font in '( "Segoe UI Symbol" "Sarasa UI SC" "更纱黑体 UI SC Nerd Font" "Symbola")
             when (+font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))
    ;;(set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'append)
    ;; 设置 emoji 字体 
    ;; (cl-loop for font in '("Symbola" "Sarasa UI SC" "更纱黑体 UI SC Nerd Font"  "nerd-icons" "Noto Color Emoji"  
    ;;                        "EmojiOne Color" "Apple Color Emoji" "Segoe UI Emoji"
    ;; 			   "OpenSansEmoji" )
    ;;          when (+font-installed-p font) 
    ;;          return (set-fontset-font t 'emoji
    ;;                                   (font-spec :family font)
    ;;                                   nil 'prepend)) 
    (cl-loop for font in '( "Segoe UI Emoji" )
             when (+font-installed-p font) 
             return (set-fontset-font t 'emoji
                                      (font-spec :family font)
                                      nil 'prepend)) 
    
    ;; (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
    ;;          when (+font-installed-p font)
    ;;          return (set-fontset-font t '(#x20000 . #x2A6DF)
    ;;                                   (font-spec :name font
    ;; 						 :weight 'normal
    ;; 						 :slant 'normal
    ;; 						 :size (cond ((eq system-type 'gnu/linux) 32)
    ;;                                                          ((eq system-type 'windows-nt) 12.5)))))
    ))
(setq face-font-rescale-alist '(("霞鹜文楷" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2)))
(+init-font +english-font-size)

(defvar +english-font-size-steps '(9 10.5 11.5 12 12.5 13 14 16 18 20 22 40))
(defun +step-frame-font-size (step)
  (let ((steps +english-font-size-steps)
        next-size)
    (when (< step 0)
      (setq steps (reverse +english-font-size-steps)))
    (setq next-size
          (cadr (member +english-font-size steps)))
    (when next-size
      (+init-font next-size)
      (setq +english-font-size next-size)
      (message "Your font size is set to %.1f" next-size))))

(global-set-key (kbd "C-_") (lambda () (interactive) (+step-frame-font-size -1)))
(global-set-key (kbd "C-+") (lambda () (interactive) (+step-frame-font-size 1)))



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
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
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
  :hook (after-init . global-hl-line-mode))


;; 设置主题
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t) ; if nil, bold is universally disabled
  (doom-themes-enable-italic t); if nil, italics is universally disabled
  :config
  (if (display-graphic-p)
      (load-theme 'doom-one-light t)
    (load-theme 'doom-dark+ t)
  )
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  )

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

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   ;; :custom
;;   ;; (kind-icon-blend-background t)
;;   ;; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
;;   :config
;;   (setq kind-icon-mapping
;; 	'((array "a" :icon "symbol-array" :face font-lock-type-face :collection "vscode")
;;           (boolean "b" :icon "symbol-boolean" :face font-lock-builtin-face :collection "vscode")
;;           (color "#" :icon "symbol-color"  :face success :collection "vscode")
;;           (command "cm"  :icon "chevron-right" :face default :collection "vscode")
;;           (constant "co"  :icon "symbol-constant" :face font-lock-constant-face :collection "vscode")
;;           (class "c"   :icon "symbol-class" :face font-lock-type-face :collection "vscode")
;;           (constructor "cn"  :icon "symbol-method" :face font-lock-function-name-face :collection "vscode")
;;           (enum "e"   :icon "symbol-enum" :face font-lock-builtin-face :collection "vscode")
;;           (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face :collection "vscode")
;;           (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face :collection "vscode")
;;           (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face :collection "vscode")
;;           (field "fd"  :icon "symbol-field" :face font-lock-variable-name-face :collection "vscode")
;;           (file  "f"   :icon "symbol-file"        :face font-lock-string-face  :collection "vscode")
;;           (folder         "d"   :icon "folder"             :face font-lock-doc-face :collection "vscode")
;;           (function  "f"   :icon "symbol-method"      :face font-lock-function-name-face :collection "vscode")
;;           (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face  :collection "vscode")
;;           (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face :collection "vscode")
;;           (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
;;           (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face :collection "vscode")
;;           (method "m"   :icon "symbol-method"      :face font-lock-function-name-face  :collection "vscode")
;;           (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
;;           (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face :collection "vscode")
;;           (operator "op"  :icon "symbol-operator" :face font-lock-comment-delimiter-face :collection "vscode")
;;           (param          "pa"  :icon "gear"               :face default :collection "vscode")
;;           (property "pr" :icon "symbol-property" :face font-lock-variable-name-face :collection "vscode")
;;           (reference "rf"  :icon "library"  :face font-lock-variable-name-face     :collection "vscode")
;;           (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face :collection "vscode")
;;           (string         "s"   :icon "symbol-string"      :face font-lock-string-face :collection "vscode")
;;           (struct "%" :icon "symbol-structure" :face font-lock-variable-name-face :collection "vscode")
;;           (text  "tx"  :icon "symbol-key"         :face font-lock-doc-face      :collection "vscode")
;;           (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face  :collection "vscode")
;;           (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face  :collection "vscode")
;;           (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face :collection "vscode")
;;           (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face  :collection "vscode")
;;           (variable "va" :icon "symbol-variable" :face font-lock-variable-name-face     :collection "vscode")
;;           (t              "."   :icon "question"           :face font-lock-warning-fac :collection "vscode")))
;;   (when (eq +system-type 'wsl)
;;     (plist-put kind-icon-default-style :height 0.4))
;;   (when (eq system-type 'windows-nt)
;;     (plist-put kind-icon-default-style :height 0.4))
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; 启动时关闭系统输入法
(when (eq system-type 'windows-nt)
  ;;(add-hook 'after-init-hook #'+close-ime)  
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
  (setq +close-ime-timer (run-with-idle-timer 20 t #'+close-ime))
  (+close-ime))

(when (eq +system-type 'wsl)
  ;; 由于 fcitx 的显示不是很好，所以暂时在 wsl 中 emacs 中不使用系统输入法
  ;;(setq pgtk-use-im-context-on-new-connection nil)
  )

;; pyim - Chinese input methods
(use-package pyim
  :init
  (setq default-input-method "pyim")
  :config
  (setq-default pyim-dcache-auto-update nil)
  (setq-default default-input-method "pyim")
  (setq-default pyim-default-scheme 'rime)
  (setq-default pyim-page-tooltip 'minibuffer)
  (setq-default pyim-page-style 'one-line)
  (setq-default pyim-cloudim nil)
  (setq-default pyim-candidates-search-buffer-p nil)
  (setq-default pyim-enable-shortcode nil)
  (pyim-scheme-add
   '(flypy
     :document "小鹤音形输入法"
     :class xingma
     :first-chars "abcdefghijklmnopqrstuvwxyz;"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :code-prefix "flypy/" ;词库中所有的 code 都以 "flypy/" 开头，防止和其它词库冲突。
     :code-split-length 4 ;默认将用户输入切成 4 个字符长的 code 列表（不计算 code-prefix）
     :code-maximum-length 4 ;小鹤音形词库中，code 的最大长度（不计算 code-prefix）
     :prefer-triggers nil
     :cregexp-support-p t))
  (pyim-extra-dicts-add-dict
   `(:name "flypy" :file ,(concat user-emacs-directory "/pyim/flypy.pyim")))
  ;;; 用分号做次选按键
  (defun semicolon-first-input-p (key)
    (= key 59))  
  (defun pyim-input-method-advice (fn &rest key)
    (if (semicolon-first-input-p (car key))
	(apply fn key)
      (define-key pyim-mode-map ";"
		  (lambda ()
		    (interactive
		     (pyim-select-word-by-number 2))))
      (let ((result (apply fn key)))
	(define-key pyim-mode-map ";" #'pyim-self-insert-command)
	result)))
  (advice-add 'pyim-input-method :around #'pyim-input-method-advice)
  ;; (define-key pyim-mode-map "'"
  ;;             (lambda ()
  ;;               (interactive)
  ;;               (pyim-select-word-by-number 2)))
  (pyim-default-scheme 'flypy)
  (defun pyim-autoselector--xingma-override (fn split-length entered candidates last-candidates)
    "`pyim-autoselector-xingma' 内部使用的函数的 advice。"
    (cond
     ((and (<= (length entered) split-length)
	   (>= (length entered) 2)
	   (= (length candidates) 1))
      '(:select current))
     ((and (> (length entered) split-length)
           (equal (substring entered 0 split-length)
                  (car last-candidates)))
      ;; 自动清除错误输入模式，类似微软五笔：敲第五个字母的时候，前面四个字母自
      ;; 动清除。
      '(:select last :replace-with ""))
     ((> (length entered) split-length)
      '(:select last))
     (t nil)))
  (advice-add 'pyim-autoselector--xingma :around #'pyim-autoselector--xingma-override)
  )

;; (use-package pyim
  ;; :config
  ;; (setq-default pyim-dcache-auto-update nil)
  ;; (setq-default default-input-method "pyim")
  ;; (setq-default pyim-default-scheme 'rime)
  ;; (setq-default pyim-page-tooltip 'minibuffer)
  ;; (setq-default pyim-page-style 'one-line)
;;   )
;; (use-package liberime
;;   :config
;;   (setq-default liberime-shared-data-dir)
;;   (setq liberime-share-data-dir "~/.local/share/fcitx5/rime")
;;   (setq liberime-user-data-dir "~/.config/fcitx5/rime")
;;   )
;; rime 输入法 
;; (use-package rime 
;;   :ensure t
;;   :custom
;;   (default-input-method "rime")
;;   (rime-cursor "˰")
;;   (rime-disable-predicates
;;    '(rime-predicate-prog-in-code-p
;;      ;;rime-predicate-punctuation-after-ascii-p
;;      ;;rime-predicate-current-uppercase-letter-p     
;;      ;;rime-predicate-after-alphabet-char-p
;;      ))
;;   (rime-inline-predicates
;;    '(rime-predicate-space-after-cc-p
;;      rime-predicate-after-ascii-char-p
;;      ))
;;   :bind
;;   (:map rime-mode-map
;;    ("M-j" . rime-force-enable))
;;   ;;:hook
;;   ;;( . rime-lib-finalize)
;;   :config
;;   (set-fontset-font t ?˰ "Symbola")
;;   (when (eq +linux-type 'nixos)
;;     (setq rime-emacs-module-header-root
;; 	  (file-truename (concat
;; 			  (file-name-directory (directory-file-name (file-truename invocation-directory)))
;; 			  "include")))
;;     (setq rime-librime-root "~/.local/state/nix/profiles/home-manager/home-path"))
;;   (defun +open-flypy ()
;;     (interactive)
;;     (set-input-method "rime"))
;;   (cond  ((eq system-type 'gnu/linux)
;; 	 (setq rime-share-data-dir "~/.local/share/fcitx5/rime")
;; 	 (setq rime-user-data-dir "~/.config/fcitx5/rime")
;; 	 ))  
;;   (set-face-attribute 'rime-preedit-face nil :background "black" :foreground "gray"))


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
;; (use-package dirvish
;;   :ensure t
;;   :hook
;;   (after-init . dirvish-override-dired-mode)
;;   :config
;;   (setq dired-mouse-drag-files t)   
;;   (setq dirvish-mode-line-format
;;         '(:left (sort symlink) :right (omit yank index)))
;;   (setq dirvish-attributes
;;         '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
;;   (setq delete-by-moving-to-trash t)
;;   (setq dired-listing-switches
;;         "-l --almost-all --human-readable --group-directories-first --no-group")
;;   :bind	     ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
;;   (("C-c f" . dirvish-fd)
;;    :map dirvish-mode-map	   ; Dirvish inherits `dired-mode-map'
;;    ("a"   . dirvish-quick-access)
;;    ("f"   . dirvish-file-info-menu)
;;    ("y"   . dirvish-yank-menu)
;;    ("N"   . dirvish-narrow)
;;    ("^"   . dirvish-history-last)
;;    ("h"   . dirvish-history-jump)	; remapped `describe-mode'
;;    ("s"   . dirvish-quicksort)	; remapped `dired-sort-toggle-or-edit'
;;    ("v"   . dirvish-vc-menu)	; remapped `dired-view-file'
;;    ("TAB" . dirvish-subtree-toggle)
;;    ("M-f" . dirvish-history-go-forward)
;;    ("M-b" . dirvish-history-go-backward)
;;    ("M-l" . dirvish-ls-switches-menu)
;;    ("M-m" . dirvish-mark-menu)
;;    ("M-t" . dirvish-layout-toggle)
;;    ("M-s" . dirvish-setup-menu)
;;    ("M-e" . dirvish-emerge-menu)
;;    ("M-j" . dirvish-fd-jump)))


(provide 'init-ui)
;;; init-ui.el ends here
