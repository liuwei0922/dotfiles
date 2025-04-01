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
  ;;(python-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c++-ts-mode c-mode) "clangd"))
  ;;(add-to-list 'eglot-server-programs '((python-mode) "pylsp"))
  )
(use-package eldoc
  :defer t
  :ensure t
  :config
  (setq eldoc-echo-area-use-multiline-p nil))


;;; format-all
;; 这个东西是需要 clang-format 的
(use-package format-all
  :ensure t)


;;; copilot
;; (use-package copilot
;;   :load-path "~/.emacs.d/elpa/copilot.el/"
;;   ;; :hook
;;   ;;(prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word))
;;   :config
;;   ;;(add-to-list 'copilot-major-mode-alist '())
;;   ;;(setq copilot-node-executable "c:/Users/qinmo/scoop/apps/nvm-windows/current/nodejs/nodejs/node.exe")
;;   (setq copilot-network-proxy '(:host "127.0.0.1" :port 10809))
;;   )
;; you can utilize :map :hook and :config to customize copilot

;;; tree-sitter
;; 1. 编译emacs29.1  
;; 2. 添加动态模块
;;(add-to-list 'treesit-extra-load-path "~/.config/emacs/tree-sitter/")
;;3. melpa 下载 tree-auto
(use-package treesit-auto
  :unless (eq system-type 'windows-nt)
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

(use-package docstr
  :ensure t
  )
(use-package cmake-mode
  :ensure t)


;;; python
;; (use-package jedi
;;   :ensure t
;;   :hook
;;   (python-mode . #'jedi:setup)
;;   )


(provide 'init-lang)
;;; init-lang.el ends here
