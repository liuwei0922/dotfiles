;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; tool-bar mode
(tool-bar-mode 1)

(defun android-toggle-keyboard ()
  (interactive)
  (if touch-screen-display-keyboard
      (progn
        (setq touch-screen-display-keyboard nil)
        (tool-bar-add-item
         "keyboard-off" 'android-toggle-keyboard
         'android-toggle-keyboard
         :help "Toggle Keyboard")
        (message "Disable virtual keyboard"))
    (setq touch-screen-display-keyboard t)
    (tool-bar-add-item
     "keyboard" 'android-toggle-keyboard
     'android-toggle-keyboard
     :help "Toggle keyboard")
    (message "Enable virtual keyboard")))

(defun android-tool-bar-configs ()
  (when (and (fboundp 'tool-bar-mode)
             (string-equal system-type "android"))
    (tool-bar-mode +1)
    (setq! tool-bar-position 'bottom)
    (setq! tool-bar-button-margin 27)
    (setq tool-bar-map '(keymap nil))
    (add-to-list 'image-load-path
                 (expand-file-name
                  "tool-bar" doom-private-dir))
    (android-general-tool-bar
     'tool-bar-add-item nil)))

(defun android-general-tool-bar (fn map)
  (mapc (lambda (args)
          (apply fn args))
        `(("keyboard-esc" tool-bar-item-escape  keyboard-esc ,map)
          ("apple-keyboard-command" tool-bar-item-ctrl apple-keyboard-command ,map)
          ("apple-keyboard-option" tool-bar-item-alt apple-keyboard-option ,map)

          ;;("arrow-down-bold" scroll-up arrow-down-bold ,map)\
          ("arrow-up-thin" tool-bar-item-up arrow-up-thin ,map)
	  ("menu" execute-extended-command menu ,map)          
          ;;("arrow-up-bold" scroll-down arrow-up-bold ,map)
          ("pen-plus" switch-to-buffer mark ,map)

          ("file-find" find-file file-find ,map)
          ("content-save" save-buffer content-save ,map)
          ("feather" +company/complete complete ,map)
          ("kill-buffer" delete-window kill-buffer ,map)
          
          ("transfer-left" tool-bar-item-cg quit ,map)
          ("keyboard-tab" tool-bar-item-tab keyboard-tab ,map)
	  ("arrow-left-thin" tool-bar-item-left arrow-left-thin ,map)
          ("arrow-down-thin" tool-bar-item-down arrow-down-thin ,map)
          ("arrow-right-thin" tool-bar-item-right arrow-right-thin ,map)

          ("content-cut" kill-region content-cut ,map)
          ("content-copy" kill-ring-save content-copy ,map)
          ("content-paste" yank content-paste ,map)
          ("selection" er/expand-region selection ,map)
	  ("keyboard-off" android-toggle-keyboard android-toggle-keyboard ,map))
)

(define-key key-translation-map
            [tool-bar apple-keyboard-command]
            #'tool-bar-event-apply-control-modifier)
(define-key key-translation-map
            [tool-bar apple-keyboard-option]
            #'tool-bar-event-apply-alt-modifier)
(define-key key-translation-map
            [tool-bar keyboard-esc]
            [escape])
(define-key key-translation-map
            [tool-bar quit]
            (kbd "C-g"))
(define-key key-translation-map
            [tool-bar keyboard-tab]
            (kbd "TAB"))
(define-key key-translation-map
            [tool-bar arrow-up-thin]
            [up])
(define-key key-translation-map
            [tool-bar arrow-down-thin]
            [down])
(define-key key-translation-map
            [tool-bar arrow-left-thin]
            [left])
(define-key key-translation-map
            [tool-bar arrow-right-thin]
            [right])
(android-tool-bar-configs)


;;; org-mode 
(use-package! org
  :hook
  (
   (org-mode . (lambda () (setq truncate-lines nil)))  
   (org-mode . (lambda () (display-line-numbers-mode -1)))
   (org-mode . (lambda ()
		 ;; 设置折行
		 (global-word-wrap-whitespace-mode 1)))
   ;; 设置 ORG 标题样式
   (org-cycle-tab-first . +org-cycle-only-current-subtree-h)
   )
  :custom
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  ;; 整体美化相关设置
  ;;(org-ellipsis "⤵")
  (org-fontify-todo-headline nil)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")))
  ;; 设置 ORG 标题范围，使上面的尾标可以正确显示
  (org-cycle-separator-lines 2)
  ;; 上下标控制
  (org-export-with-sub-superscripts '{})
  (org-use-sub-superscripts '{})
  (org-use-sub-superscripts '{})
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  ;; org 中图片大小
  (org-image-actual-width nil)
  ;; 其他设置
  (org-imenu-depth 4)
  (org-clone-delete-id t)
  (org-use-sub-superscripts '{})
  (org-yank-adjusted-subtrees t)
  (org-ctrl-k-protect-subtree 'error)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-return-follows-link nil)
  ;; 启动时缩进
  (org-startup-indented t)
  ;; 启动时折叠内容
  (org-startup-folded t)
  ;; TODO 结束时加上时间
  (org-log-done 'time)
  ;; TODO 设置
  (org-todo-keywords '((sequence "TODO(t)"
				 "DOING(i!)"
				 "|"
				 "DONE(d!)"
				 "KILLED(k!)")))
  (org-todo-keyword-faces '(("DONE"       :foreground "#7c7c75" :weight bold)
                            ("DOING"       :foreground "#feb24c" :weight bold)
                            ("TODO"       :foreground "#50a14f" :weight bold)
                            ("KILLED"  :foreground "#ff6480" :weight bold)))
  ;; 时间戳格式
  (org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-priority-faces '((?A :foreground "red")
                        (?B :foreground "orange")
                        (?C :foreground "yellow")))
  (org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %CLOCKSUM")
  ;; Remove CLOSED: [timestamp] after switching to non-DONE states
  (org-closed-keep-when-no-todo t)
  ;; refile
  (org-refile-use-cache nil)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; tags, e.g. #+TAGS: keyword in your file
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)
  ;; id
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; abbreviation for url
  (org-link-abbrev-alist '(("GitHub" . "https://github.com/")
                           ("GitLab" . "https://gitlab.com/")
                           ("Google" . "https://google.com/search?q=")
                           ("RFCs"   . "https://tools.ietf.org/html/")
                           ("LWN"    . "https://lwn.net/Articles/")
                           ("WG21"   . "https://wg21.link/")
			   ("bilibili" . "https://www.bilibili.com/video/%s")
			   ("mengbai" . "https://mzh.moegirl.org.cn/zh-hans/%s")
			   ("handian" . "https://www.zdic.net/hans/%s")))
  :config
  (defun +org-cycle-only-current-subtree-h (&optional arg)
    "Toggle the local fold at the point (as opposed to cycling through all levels
with `org-cycle')."
    (interactive "P")
    (unless (eq this-command 'org-shifttab)
      (save-excursion
	(org-beginning-of-line)
	(let (invisible-p)
          (when (and (org-at-heading-p)
                     (or org-cycle-open-archived-trees
			 (not (member org-archive-tag (org-get-tags))))
                     (or (not arg)
			 (setq invisible-p (outline-invisible-p (line-end-position)))))
            (unless invisible-p
              (setq org-cycle-subtree-status 'subtree))
            (org-cycle-internal-local)
            t)))))
  )

;;; UI Setting

