; mutt
; (server-start)
; (add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

(setq auto-mode-alist
      (cons '("/mutt" . mail-mode) auto-mode-alist))

; http://fsinfo.noone.org/~abe/mutt/
(defun axels-mail-mode-hook ()
  (turn-on-auto-fill) ;;; Auto-Fill is necessary for mails
  (turn-on-font-lock) ;;; Font-Lock is always cool *g*
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ;;; Kills quoted sigs.
  (not-modified) ;;; We haven't changed the buffer, haven't we? *g*
  (mail-text) ;;; Jumps to the beginning of the mail text
   ; (setq make-backup-files nil) ;;; No backups necessary.
  (local-set-key "\C-Xk" 'server-edit)
  (flyspell-mode) ;;; amoore
  ) 

(add-hook 'mail-mode-hook 'axels-mail-mode-hook)

; .muttrc
; (setq auto-mode-alist
;       (cons '("/.muttrc" . muttrc-mode) auto-mode-alist))
