;;; init-mail.el --- email configration               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  liuwei

;; Author: lyuweii <wliu@ihep.ac.cn>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 收发邮件

;;; Code:

;;; notmuch





;;; wanderlust
;; (use-package wanderlust
;;   :ensure t
;;   :config
;;   (require 'mail)
;;   (if (boundp 'mail-user-agent)
;;     (setq mail-user-agent 'wl-user-agent))
;;   (if (fboundp 'define-mail-user-agent)
;;       (define-mail-user-agent
;; 	'wl-user-agent
;; 	'wl-user-agent-compose
;; 	'wl-draft-send
;; 	'wl-draft-kill
;; 	'mail-send-hook)))

;; (use-package wanderlust
;;   :ensure t)
;; (use-package wl
;;   :bind (:map wl-folder-mode-map
;;               (("q" . wl-folder-suspend)))
;;   :init
;;   (setq wl-init-file (expand-file-name "wl.el" user-emacs-directory))
;;   :config
;;   (if (boundp 'mail-user-agent)
;;       (setq mail-user-agent 'wl-user-agent))
;;   (if (fboundp 'define-mail-user-agent)
;;       (define-mail-user-agent
;;         'wl-user-agent
;;         'wl-user-agent-compose
;;         'wl-draft-send
;;         'wl-draft-kill
;;         'mail-send-hook))
;;   (setq wl-quicksearch-folder "[]")
;;   (setq wl-message-ignored-field-list
;; 	'(".")
;; 	wl-message-visible-field-list
;; 	'("^\\(To\\|Cc\\):"
;;           "^Subject:"
;;           "^\\(From\\|Reply-To\\):"
;;           "^\\(Posted\\|Date\\):"
;;           "^Organization:"
;;           "^X-\\(Face\\(-[0-9]+\\)?\\|Weather\\|Fortune\\|Now-Playing\\):")
;; 	wl-message-sort-field-list
;; 	(append wl-message-sort-field-list
;; 		'("^Reply-To" "^Posted" "^Date" "^Organization")))
;;   ;; windows 上 mu find 返回的路径以 /cygdrive/ 开头，我们需要自己处理一下
;;   (defun my--elmo-search-parse-filename-list ()
;;     )
;;   (let (bol locations)
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         (beginning-of-line)
;;         (when (and elmo-search-use-drive-letter
;;                    (looking-at "^\\([A-Za-z]\\)\\([:|]\\)?/"))
;;           (replace-match "/\\1:/")
;;           (beginning-of-line))
;;         (unless (looking-at "^file://")
;;           (insert "file://")
;;           (beginning-of-line))
;;         (setq bol (point))
;;         (end-of-line)
;;         (setq locations (cons (buffer-substring bol (point)) locations))
;;         (forward-line))
;;       (nreverse locations))
;;   (elmo-search-register-engine
;;    'mu-msys 'local-file
;;    :prog "mu"
;;    :args '("find" elmo-search-split-pattern-list "--fields" "l")
;;    :charset 'utf-8
;;    :parser 'my--elmo-search-parse-filename-list)
;;   (setq elmo-search-default-engine 'mu-msys)
;;   ;; mu 的输入要用 gbk 编码，不然无法输入中文
;;   (add-to-list 'process-coding-system-alist '("mu" utf-8 . gbk))
;;   ;; mime 附件保存目录
;;   (setq mime-save-directory (expand-file-name "~/mails/mime"))
;;   )



;; (defun my-*require-wanderlust (&rest _)
;;   (require 'wl))

;;   (advice-add 'compose-mail :before #'my-*require-wanderlust)
;; (use-package gnus-demon
;;   :ensure nil
;;   :after gnus
;;   :config
;;   ;; 自定义的函数用于检测新邮件并提醒
;;   (defun freedom--gnus-check-new-mail ()
;;     (message "开始刷新邮件")
;;     ;; 检查是否有新邮件
;;     (when (gnus-group-unread (gnus-group-group-name))
;;       (posframe-show "*Gnus News Posframe*"
;;                      :string "接收到新邮件."
;;                      :left-fringe 5
;;                      :right-fringe 5
;;                      :position (point)
;;                      :poshandler #'posframe-poshandler-frame-center
;;                      :border-width 5	 ;; 外边框大小
;;                      :border-color "red" ;; 边框颜色
;;                      )
;;       (when IS-Windows
;;         (shell-command (concat "PowerShell -Command New-BurntToastNotification -AppLogo " (concat org-directory  "/emacs.png") " -Text  'Gnus News' , '接收到新邮件.'  ")))
;;       ))
;;   ;; 在邮件获取后检查是否有新邮件
;;   (add-hook 'gnus-after-getting-new-news-hook 'freedom--gnus-check-new-mail)
;;   ;; 设置 Gnus 定期获取邮件, 分钟
;;   (gnus-demon-add-handler 'gnus-group-get-new-news 10 nil)
;;   ;; 每次运行 gnus 初始化 demo
;;   (advice-add #'gnus :after #'gnus-demon-init)
;;   )


;;; mu4e
(use-package mu4e
  :load-path "~/.emacs.d/elpa/mu4e/"
  :commands mu4e
  :bind
  ("C-c e" . #'+mu4e-inbox)
  :config
  (setq mu4e-update-interval (* 5 60))
  ;; 默认邮件
  (setq mail-user-agent 'mu4e-user-agent)
  ;; 搜中文
  (setenv "XAPIAN_CJK_NGRAM" "1")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-update-interval (* 10 60))
  ;; 更新方式
  (setq mu4e-get-mail-command "mbsync -a")
  (when (eq system-type 'windows-nt)
    (setq mu4e-maildir "C:/Users/qinmo/mails/")
    )
  (setq mu4e-contexts
	(list
	 (make-mu4e-context
	  :name "IHEP"
	  :match-func (lambda (msg)
			(when msg
			  (string-prefix-p "/ihep.ac.cn" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "wliu@ihep.ac.cn")
		  (user-full-name . "lyuweii")
		  (mu4e-drafts-folder . "/wliu@ihep.ac.cn/Drafts")
		  (mu4e-sent-folder . "/wliu@ihep.ac.cn/Sent")
		  (mu4e-trash-folder . "/wliu@ihep.ac.cn/Trash")))
	 (make-mu4e-context
	  :name "UCAS"
	  :match-func (lambda (msg)
			(when msg
			  (string-prefix-p "/mails.ucas.ac.cn" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "liuwei234@mails.ucas.ac.cn")
		  (user-full-name . "lyuweii")
		  (mu4e-drafts-folder . "/liuwei234@mails.ucas.ac.cn/Drafts")
		  (mu4e-sent-folder . "/liuwei234@mails.ucas.ac.cn/Sent")
		  (mu4e-trash-folder . "/liuwei234@mails.ucas.ac.cn/Trash")))))
  ;; (setq browse-url-browser-function 'browse-url-generic)
  (setq mu4e-maildir-shortcuts
	'(("/wliu@ihep.ac.cn/Inbox" . ?i)
	  ("/wliu@ihep.ac.cn/Sent" . ?s)
	  ("/wliu@ihep.ac.cn/Trash" . ?t)
	  ("/liuwei234@mails.ucas.ac.cn/Sent" . ?m)
	  ("/liuwei234@mails.ucas.ac.cn/Trash" . ?n)
	  ("/liuwei234@mails.ucas.ac.cn/Inbox" . ?b)))
  (setq mu4e-bookmarks
	'(("flag:unread AND NOT flag:trashed" "Unread message" ?i)
	  ("date:today..now"                  "Today's messages" ?t)
	  ("date:7d..now"                     "Last 7 days" ?w)))
  ;; (add-to-list 'mu4e-view-actions
  ;;              '("XWidget View" . mu4e-action-view-with-xwidget) t)
  (setq mu4e-user-mail-address-list (list "wliu@ihep.ac.cn" "liuwei234@mails.ucas.ac.cn"))
  (setq message-sendmail-envelope-from 'header)
  ;;(setq mu4e-headers-fields '((:human-date . 25)  (:from . 35) (:subject . 45)))
  )

;; Mu4e Column faces ： mu4e 样式美化
(use-package mu4e-column-faces
  :ensure t
  :after mu4e
  :config (mu4e-column-faces-mode))



;; mu4e-mark-icons
(use-package mu4e-marker-icons
  :ensure t
  :after mu4e
  :config
  (setq mu4e-headers-precise-alignment t)
  )



;;; alert
(use-package mu4e-alert
  :ensure t
  :after mu4e
  :config
  (mu4e-alert-enable-notifications)
  (mu4e-alert-set-default-style 'toast)
  )


(use-package alert
  :after mu4e
  :commands (alert)
  :config (setq alert-default-style 'toast))


(use-package alert-toast
  :ensure t
  :after alert
  ;; :config
  ;; (defun my--notify-new-mail-arrived ()
  ;;   (alert-toast-notify `(:title "新邮件" :message ,(format "你有 %s 封未读邮件" 1))))
  ;; (add-hook 'mu4e-index-updated-hook #'my--notify-new-mail-arrived)
  ;;(add-hook 'mu4e-index-updated-hook #'mu4e~headers-maby-auto-update)
  )


;;; 发邮件
(use-package smtpmail
  :ensure nil
  :commands (compose-mail mail)
  :config
  (setq message-send-mail-function 'smtpmail-send-it
	message-directory "C:/Users/qinmo/mails/wliu@ihep.ac.cn/"	
	;;starttls-use-gnutls t
	;;smtpmail-starttls-credentials '(("mail.ihep.ac.cn" 465 nil nil))
	;;smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
	smtpmail-default-smtp-server "mail.ihep.ac.cn"
	smtpmail-stream-type 'ssl
	smtpmail-smtp-server "mail.ihep.ac.cn"
	smtpmail-smtp-service 465
	smtpmail-debug-info t)
  (setq mail-user-agent 'message-user-agent)
  )




;; (use-package gnus
;;   :ensure nil
;;   :commands gnus
;;   :config
;;   (setq gnus-select-method '(nnnil "")
;;       gnus-secondary-select-methods
;;       '((nnmaildir "ihep" (directory "C:/Users/qinmo/mails/wliu@ihep.ac.cn/"))))
;;   (setq mail-sources                                 ;邮件源设置
;;       '((maildir :path "C:/Users/qinmo/mails/wliu@ihep.ac.cn/"           ;本地邮件存储位置
;;                  :subdirs ("cur" "new" "tmp"))))   ;本地邮件子目录划分
;;   )

(provide 'init-mail)
;;; init-mail.el ends here
