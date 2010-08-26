;; from http://pyside.blogspot.com/2008/02/there-are-many-things-you-can-say-about.html
(global-set-key (kbd "M-#") 'comment-region)
(require 'sqlplus)
(add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))

; Directory of SQL*Plus command history (log) files, or nil (dont generate log files). History file name has format '<connect-string>-history.txt'.
(setq sqlplus-history-dir "~/sqlplus")

; Directory of SQL*Plus input buffer files, or nil (dont save user session). Session file name has format '<connect-string>.sqp'
(setq sqlplus-session-cache-dir "~/sqlplus")

; (setq sqlplus-pagesize 0)
(setq sqlplus-pagesize 50000)


; http://stackoverflow.com/questions/60367/the-single-most-useful-emacs-feature/64076#64076
