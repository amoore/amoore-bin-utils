(require 'mmm-auto)
;; http://www.masonhq.com/?MMMModeForEmacs
(setq mmm-global-mode 'maybe)
;; (html-mode "\\.html\\'" mason)         ;; Any .html file in html-mode
;; (nil "\\.\\(mason\\|html\\)\\'" mason) ;; All .mason and .html files
;; (nil "\\`/var/www/mason/" mason)       ;; Any file in the directory
;; (nil "\\`xmnetwork_htdocs/" mason)       ;; Any file in the directory

; make mason pages be in html-mode
(add-to-list 'auto-mode-alist '("\\handler\\'" . html-mode))
