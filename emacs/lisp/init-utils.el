;;; init-utils.el --- Emacs config utils             -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; 判断系统
(defvar +system-type 'gnu/linux)
(if (eq system-type 'windows-nt)
    (setq +system-type system-type))
(when (and (eq system-type 'gnu/linux)
           (string-match
            ".*microsoft.*"
            (shell-command-to-string "uname -r")))
  (setq +system-type 'wsl))

(defvar +linux-type 'nil)
(when (eq system-type 'gnu/linux)
  (cond ((executable-find "nixos-rebuild")
	 (setq +linux-type 'nixos))
	((executable-find "pacman")
	 (setq +linux-type 'arch))))



(provide 'init-utils)
;;; init-utils.el ends here

