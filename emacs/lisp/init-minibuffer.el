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
  (:map minibuffer-local-map
	("M-o" . embark-act)
	("C-c C-c" . embark-export)
	("C-c C-o" . embark-collect)))


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
  (add-to-list 'consult-buffer-filter "^\*.+\*$")
  :custom
  (consult--fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))


;;; embark-consult
(use-package embark-consult
  :ensure t)


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
