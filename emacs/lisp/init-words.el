;;; init-words.el --- Initialize configurations.	-*- lexical-binding: t -*-



;;; advance-words-count
(add-to-list 'load-path (concat user-emacs-directory "elpa/advance-words-count"))

(require 'advance-words-count)

(defun +words-count--format-message (cons &optional arg)
  "Format a string to be shown for `words-count--message'.
Using the CONS passed form `advance-words-count'. See
`count-lines' & `count-words'. When ARG is specified, display
verbosely."
  (let ((start (car cons))
        (end (cdr cons))
        list)
    (setq list (advance-words-count start end))
    (format
     (if arg
	 "
-----------~*~ Words Count ~*~----------

 Characters (without Space) .... %d
 Characters (all) .............. %d
 Number of Lines ............... %d
 ANSCII Words .................. %d
%s
========================================
"
       "字符（除空格）：%d, 字符（带空格）：%d, 行数：%d, ANSCI 词数：%d, %s")
     (cadr list)
     (- end start)
     (count-lines start end)
     (car (last list))
     (if (= 0 (car list))
         (format (if arg
                     " Latin Words ................... %d\n"
                   "拉丁字母词数：%d")
                 (count-words start end))
       (format (if arg
                   " CJK Chars ..................... %d
 Word Count .................... %d\n"
                 "中文：%d, 中英文总字数：%d")
	       (car list)
	       (+ (car list) (car (last list))))))))

(advice-add 'words-count--format-message :override #'+words-count--format-message)

(provide 'init-words)
;;; init-words.el ends here
