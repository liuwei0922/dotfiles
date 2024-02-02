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
  :ensure t
  :defer t
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c++-ts-mode c-mode) "clangd")))
(use-package eldoc
  :defer t
  :ensure t)


;;; tree-sitter
;; 1. 编译emacs29.1  
;; 2. 添加动态模块
(add-to-list 'treesit-extra-load-path "~/.config/emacs/tree-sitter/")
;;3. melpa 下载 tree-auto
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-install nil)
  ;; 这句话最管用,一下子从白茫茫一片变好了.
  (setq treesit-font-lock-level 4)
  ;; 4. 添加模式链接
  ;; (setq major-mode-remap-alist
  ;; 	'((c-mode . c-ts-mode)
  ;; 	  (python-mode . python-ts-mode)
  ;; 	  (c++-mode . c++-ts-mode)))
  ;; (setq treesit-load-name-override-list
  ;; 	'((c++ "libtree-sitter-cpp" "tree_sitter_cpp")))
  )


(provide 'init-lang)
;;; init-lang.el ends here
