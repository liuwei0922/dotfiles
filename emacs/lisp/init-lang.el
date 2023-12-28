;;; init-lang.el --- Emacs config langs              -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

;; sly
(use-package sly
  :ensure nil
  :defer t
  :bind
  (:map sly-mode-map
	("C-c C-x C-b" . sly-eval-buffer))
  :config
  (add-hook 'sly-mrepl-mode-hook #'company-mode)
  (add-hook 'lisp-mode-hook #'sly)
  (setq inferior-lisp-program "sbcl")
  (setq sly-contribs '(sly-fancy)))




;;; rust
;; rust-mode
(use-package rust-mode
  :ensure nil
  :defer t 
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))


;;; eglot
(use-package eglot
  :ensure nil
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c++-ts-mode c-mode) "clangd")))
(use-package eldoc
  :ensure nil)


;;; tree-sitter
;; 1. 编译emacs29.1  
;; 2. 添加动态模块
(add-to-list 'treesit-extra-load-path "~/.emacs.d/tree-sitter/tree-sitter-module")
;; 3. melpa 下载 tree-auto
(use-package treesit-auto
 :ensure t
 :hook (after-init . global-treesit-auto-mode)
 :config
 ;;(global-treesit-auto-mode)
 ;;(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
 (setq treesit-auto-install nil)
 ;; 这句话最管用,一下子从白茫茫一片变好了.
 (setq treesit-font-lock-level 4)
 ;; 4. 添加模式链接
 (setq major-mode-remap-alist
	'((c-mode . c-ts-mode))))


(provide 'init-lang)
;;; init-lang.el ends here
