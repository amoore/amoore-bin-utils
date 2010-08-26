(defun svndocdiff-current ()
  "Invoke svn docdiff for the current buffer,
and run mozilla-firefox -remote openfile(filename)
to preview the diff"
  (interactive)
  (let* ((currentname
          (buffer-file-name (current-buffer)))
         (svnorigname
          (concat default-directory
                  ".svn/text-base/"
                  (file-name-nondirectory
                   (buffer-file-name (current-buffer)))
                  ".svn-base"))
         (diff-command
          "/usr/bin/docdiff --html %s %s > %s")
         (mozilla-command-line
           "firefox %s")
         (moztmpfile-name
          (concat default-directory
                  ".svn/.docdifftmp.html")))
    (save-buffer)
    (shell-command
     (format diff-command
             svnorigname
             currentname
             moztmpfile-name)
     "*svndocdiff*" "*svndocdiff-err*")
    (shell-command
     (format mozilla-command-line
             moztmpfile-name)
     "*svndocdiff*" "*svndocdiff-err*")))

;; http://www.emacsblog.org/2007/05/17/package-faves-psvn/
(setq svn-status-hide-unmodified 't)

