;;; init-minibuffer.el --- minibuffer                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  qinmoxiao

;; Author: qinmoxiao <qinmoxiao@qq.com>

;;; Code:


;;; vertico
(use-package vertico
  :ensure t
  :hook
  ((after-init . vertico-mode)
   (minibuffer-setup . vertico-repeat-save))
  :custom
  (vertico--sort-function nil))


;;; embark
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ;;("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   :map minibuffer-local-map
   ("M-o" . embark-act)
   ("C-c C-c" . embark-export)
   ("C-c C-o" . embark-collect))
  :init
  ;; Optionally replace the key help with a completing-read interface
  ;;(setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\*Embark Actions\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
  


;;; consult
(use-package consult
  :ensure t
  :bind
  (([remap imenu] . consult-imenu)
   ([remap isearch-forward] . consult-line)
   ([remap goto-line] . consult-goto-line)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap jump-to-register] . consult-register-load)
   ([remap point-to-register] . consult-register-store)
   ([remap switch-to-buffer] . consult-buffer)
   ;;:map org-mode-map
   ("C-c o" . consult-outline)
   )
  :config
  ;; (with-no-warnings
  ;;   (consult-customize consult-ripgrep
  ;; 		       consult-git-grep
  ;; 		       consult-grep
  ;; 		       consult-bookmark
  ;; 		       consult-recent-file
  ;; 		       consult-buffer
  ;; 		       :preview-key nil))
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  ;;筛选去掉带 * 的 buffer
  (add-to-list 'consult-buffer-filter "\\`\\*Messages\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Async-native-compile-log\\*\\'")
  :custom
  (consult--fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))


;;; embark-consult
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;;; marginalia
(use-package marginalia
  :ensure t
  :hook
  (after-init . marginalia-mode))


;;; orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; company-mode
;; (use-package company
;;   :ensure t
;;   :hook (after-init . global-company-mode)
;;   :custom
;;   (company-tooltip-align-annotations t)
;;   (company-tooltip-limit 9)
;;   ;; 显示编号
;;   (company-show-numbers 9)
;;   ;; 延时弹出时间
;;   (company-idle-delay 0.4)
;;   ;; 补全字符开始数量
;;   (company-minimum-prefix-length 2)
;;   (company-capf--sorted t)
;;   )
;; (use-package company-prescient
;;   :ensure t
;;   :hook
;;   (company-mode . company-prescient-mode))


;;; corfu
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)  ;; Enable auto completion
  (corfu-auto-prefix 2)
  ;;(corfu-separator ?\s)	;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;;(corfu-quit-no-match t) ;; Never quit, even if there is no match
  (corfu-preview-current nil) ;; Disable current candidate preview
  ;;(corfu-preselect 'prompt)	  ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
	 (shell-mode . corfu-mode)
	 (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  ;; Optionally:
  (setq nerd-icons-corfu-mapping
	'((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          (t :style "cod" :icon "code" :face font-lock-warning-face))))


;; Add extensions
(use-package cape
  :ensure t
  :after corfu
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  (unless (package-installed-p 'company)
    (package-install 'company))
  (require 'company)
  )


(unless (display-graphic-p)
    (use-package corfu-terminal
      :ensure t
      ;;:custom
      ;;(corfu-terminal-mode t)
      :config
      (corfu-terminal-mode +1)
      ))
(when (daemonp)
  (require 'corfu-terminal))


;; Use Dabbrev with Corfu!
(use-package dabbrev
  :ensure t
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  ;;(add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))


;; (use-package company-statistics
;;   :ensure t
;;   :after company
;;   :hook
;;   (after-init . company-statistics-mode))

;;; ivy
;; (use-package ivy
;;   :ensure t
;;   :diminish ivy-mode
;;   :hook
;;   (after-init . ivy-mode)
;;   :custom
;;   (ivy-count-format "%d/%d ")
;;   (ivy-display-style 'fancy)
;;   (ivy-initial-inputs-alist nil)
;;   (ivy-wrap t)
;;   (ivy-use-virtual-buffers t)
;;   :config
;;   (add-to-list 'ivy-ignore-buffers "\\*[[:ascii:]]+\\*")
;;   :bind
;;   (("C-c C-r" . ivy-resume)))
;; ;; counsel
;; (use-package counsel
;;  :ensure t
;;  :after ivy
;;  :hook
;;  (after-init . counsel-mode)
;;  :bind
;;  (("M-x" . counsel-M-x)
;;   ("C-x b" . counsel-switch-buffer)
;;   ("C-x C-b" . counsel-switch-buffer)
;;   ("C-x C-f" . counsel-find-file)
;;   ("C-x C-r" . counsel-recentf)
;;   ;;("C-h f" . counsel-describe-function)
;;   ;;("C-h v" . counsel-describe-variable)
;;   ))
;; ;; swiper
;; (use-package swiper
;;  :ensure t
;;  :bind
;;  (("C-s" . swiper-isearch)
;;   ))

;; (use-package amx
;;   :ensure t
;;   :hook
;;   (ivy-mode . amx-mode))

;; (use-package ivy-prescient
;;   :ensure t
;;   :hook
;;   (ivy-mode . ivy-prescient-mode))


(provide 'init-minibuffer)
