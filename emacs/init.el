;;; init.el -*- lexical-binding: t; -*-


;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it by the
;; `gcmh' package.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; `lsp-mode' benefits from that.
(setq read-process-output-max (* 4 1024 1024))
(setq custom-file (concat user-emacs-directory "custom.el"))

;;; 加载包设置
(require 'package)
;; (setq package-archives '(;;("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;; 			 ("gnu" . "http://1.15.88.122/gnu/")
;;                          ;;("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;; 			 ("melpa-stable" . "http://1.15.88.122/stable-melpa/")
;; 			 ("melpa" . "http://1.15.88.122/melpa/")
;; 			 ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;; 			 ("gnu-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
;; 			 ;;("gnu-devel" . "https://elpa.gnu.org/devel/")
;; 			 ))
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; 初始化包
(setq package-check-signature nil)
;;(package-initialize)

;;; use-package 加载
(unless (package-installed-p 'use-package)
  ;; 更新本地缓存
  (package-refresh-contents)
  ;; 下载安装
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure nil
	use-package-always-defer nil
	use-package-always-demand nil
	use-package-expand-minimally nil
	use-package-enable-imenu-support t))
(eval-and-compile
  (require 'use-package))


;; quelpa - For those packages which are not in MELPA
(use-package quelpa
  :ensure t
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p  nil)
  ) 
(use-package quelpa-use-package
  :ensure t
  :after quelpa
  :config
  (quelpa-use-package-activate-advice); 启用这个 advice
  ) ; 把 quelpa 嵌入 use-package 的宏扩展
  

;;; 加载配置路径
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir)))


(require 'init-utils)
;; 外观设置
(require 'init-ui)
;; 补全
(require 'init-minibuffer)
(require 'init-base)
(require 'init-lang)
;; org-mode
(require 'init-org)
;; count-words
(require 'init-words)


;;; 自定义函数


;;; 自定义快捷键
(global-set-key (if (display-graphic-p)
		    (kbd "C-;")
		  (kbd "M-;")) #'set-mark-command) 
;; 设置 SHIFT 中英转换
(global-set-key (kbd "M-\\") #'toggle-input-method)
;; 设置 HELP-MODE 中的快捷键
(define-key help-mode-map (kbd "n") (kbd "C-n"))
(define-key help-mode-map (kbd "p") (kbd "C-p"))
;; 设置 READ—ONLY-MODE 中的快捷键
(defvar +read-only-mode-map (make-sparse-keymap)
  "keymap for my read-only mode.")
(define-key +read-only-mode-map (kbd "n") (kbd "C-n"))
(define-key +read-only-mode-map (kbd "p") (kbd "C-p"))
(define-key +read-only-mode-map (kbd "ll") (kbd "C-l"))
(define-key +read-only-mode-map (kbd "f") (kbd "C-f"))
(define-key +read-only-mode-map (kbd "b") (kbd "C-b"))
(define-key +read-only-mode-map (kbd "i") #'read-only-mode)



;;; 自定义的 HOOK
;; (add-hook 'read-only-mode-hook #'(lambda ()
;; 				   (use-local-map +read-only-mode-map)))

;;; 自定义的 ADVICE
(advice-add 'read-only-mode :around #'(lambda (orig-fun &rest args)
					(cond ((and (boundp 'read-only-mode--state)
						    read-only-mode--state)
					       (let ()
						 (apply orig-fun args)
						 (use-local-map nil)))
					      (t
					       (apply orig-fun args)
					       (use-local-map +read-only-mode-map)))))
