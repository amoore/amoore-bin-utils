;;; http://www.emacswiki.org/cgi-bin/wiki/CPerlMode
;;; cperl-mode is preferred to perl-mode
;;; "Brevity is the soul of wit" <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)
(setq cperl-invalid-face nil)
(setq cperl-electric-keywords t) ;; expands for keywords such as foreach, while
;(setq cperl-hairy t)
; (setq cperl-electric-parens t)

;; turn autoindenting on
(global-set-key "\r" 'newline-and-indent)

(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))

;; let hashes indent normally
'(cperl-indent-parens-as-block t)
'(cperl-close-paren-offset -4)


;; from Perl Best Practices
;; Set line width to 78 columns...
(setq fill-column 78)
(setq auto-fill-mode t)

;; ;; from Joshua ben Jore <twists@gmail.com> on the perlcritic mailing list
;; (autoload 'perlcritic        "perlcritic" "" t)
;; (autoload 'perlcritic-region "perlcritic" "" t)
;; (autoload 'perlcritic-mode   "perlcritic" "" t)
;; (eval-after-load "cperl-mode"
;;   '(add-hook 'cperl-mode-hook 'perlcritic-mode))
;; (eval-after-load "perl-mode"
;;   '(add-hook 'perl-mode-hook 'perlcritic-mode))


;; from Perl Best Practices
;; Use % to match various kinds of brackets...
;; See: http://www.lifl.fr/~hodique/uploads/Perso/patches.el
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))))

;; from Perl Best Practices
;; Expand the following abbreviations while typing in text files...
(abbrev-mode 1)
(define-abbrev-table 'global-abbrev-table '(
    ("pdbg"   "use Data::Dumper qw( Dumper );\nwarn Dumper[];"   nil 1)
    ("phbp"   "#! /usr/bin/perl -w"                              nil 1)
    ("pbmk"   "use Benchmark qw( cmpthese );\ncmpthese -10, {};" nil 1)
    ("pusc"   "use Smart::Comments;\n\n### "                     nil 1)
    ("putm"   "use Test::More 'no_plan';"                        nil 1)
    ))
(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))


;; I should include the perltidy mode in here:
;; http://perlcritic.tigris.org/servlets/ReadMsg?list=dev&msgNo=347

(font-lock-add-keywords 'cperl-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\)" 1 font-lock-warning-face prepend)))


;; http://www.emacswiki.org/emacs/CPerlMode
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))

; http://www.modernperlbooks.com/mt/2009/03/making-your-testing-life-easier.html
(eval-after-load "cperl-mode"
  '(add-hook 'cperl-mode-hook
             (lambda () (local-set-key "\C-ct" 'cperl-prove))))

(defun cperl-prove ()
  "Run the current test."
  (interactive)
  (shell-command (concat "prove -lv --merge -It/tests "
                         (shell-quote_argument (buffer-file-name)))))


