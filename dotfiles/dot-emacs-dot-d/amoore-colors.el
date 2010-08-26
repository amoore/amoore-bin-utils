
;; These pages will help you fix the color scheme.
;; http://www.emacswiki.org/cgi-bin/wiki/ColorsAndKde
;; http://www.cs.cmu.edu/~maverick/GNUEmacsColorThemeTest/index.html
;; http://www.emacswiki.org/cgi-bin/wiki?ColorTheme
;; (require 'color-theme)
;; (setq color-theme-is-global t)
;; (color-theme-calm-forest)


; http://blog.nozav.org/post/2010/07/12/Updated-tangotango-emacs-color-theme
(require 'color-theme)
(setq color-theme-load-all-themes nil)

(require 'color-theme-tangotango)

;; select theme - first list element is for windowing system, second is for console/terminal
;; Source : http://www.emacswiki.org/emacs/ColorTheme#toc9
(setq color-theme-choices 
      '(color-theme-tangotango color-theme-tangotango))

;; default-start
(funcall (lambda (cols)
    	   (let ((color-theme-is-global nil))
    	     (eval 
    	      (append '(if (window-system))
    		      (mapcar (lambda (x) (cons x nil)) 
    			      cols)))))
    	 color-theme-choices)

;; test for each additional frame or console
(require 'cl)
(fset 'test-win-sys 
      (funcall (lambda (cols)
    		 (lexical-let ((cols cols))
    		   (lambda (frame)
    		     (let ((color-theme-is-global nil))
		       ;; must be current for local ctheme
		       (select-frame frame)
		       ;; test winsystem
		       (eval 
			(append '(if (window-system frame)) 
				(mapcar (lambda (x) (cons x nil)) 
					cols)))))))
    	       color-theme-choices ))
;; hook on after-make-frame-functions
(add-hook 'after-make-frame-functions 'test-win-sys)

(color-theme-tangotango)


