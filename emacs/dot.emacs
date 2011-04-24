;;;; .emacs						-*- emacs-lisp -*-

(add-to-list 'load-path (expand-file-name "~/emacs/lisp"))

(cond ((eq window-system 'ns)		; Cocoa aka NextStep
       (load "MacOSX"))
      ((eq window-system 'mac)
       ;; These include fixed value screen sizes:
       (if (boundp 'aquamacs-version)
	   (load "MacOSX-Aquamacs")	;Aqua supersedes Carbon
	   (load "MacOSX-Carbon")))
      ((eq window-system 'x)
       (load "ubuntu"))
      ((eq window-system 'windows)
       (load "MS-Windows"))
      (t
       ;; http://common-lisp.net/project/slime/doc/html/slime.html
       (add-to-list 'load-path "/usr/local/lisp/slime")
       (add-to-list 'load-path "/usr/local/lisp/slime/contrib")
       (setq inferior-lisp-program "/usr/local/bin/sbcl"
	     hyperspec-prog "/usr/local/slime/hyperspec"
	     hyperspec-path "/usr/share/doc/HyperSpec/"
	     cltl2-url "file:///usr/share/doc/cltl/clm/node1.html"
	     w3m-command "/usr/local/bin/w3m")))

;; Divisions that made sense in 1989 may seem confusing in 2009:
(load "settings") ; (setq ...)
(load "utils")	  ; (defun ...)
(load "keyboard") ; (define-key ...)

;;(load "actionscript-mode")

;;(load (expand-file-name "/usr/local/lisp/quicklisp/slime-helper"))
(require 'slime)
(slime-setup '(slime-repl slime-editing-commands slime-scratch slime-fontifying-fu))
;; Then, when ready to work on your Lisp code, type: M-x slime

;; w3m is optional, used by SLIME for HyperSpec; may use Firefox instead.
(add-to-list 'load-path (expand-file-name "~/emacs/lisp/emacs-w3m"))
(require 'w3m-load)
;;You may need to manually install: /Applications/Emacs.app/Contents/Resources/site-lisp/w3m/
(load "w3m")

;;(setenv "PATH" (concat (getenv "PATH") ":/usr/local/git/bin"))
(require 'git)
;; (defun git-call-process (buffer &rest args)
;;   "Wrapper for call-process that sets environment strings."
;;   (apply #'call-process "/usr/local/git/bin/git" nil buffer nil args))

(display-time)

(defun Home () 
  "Upon starting emacs, upon login-- execute this function"
  (interactive)
  (find-file "~/etc/REMINDER")
  (diary 0)
  (calendar)
  (other-window 1)
  (unless (buffer-file-name)   ; because *Fancy Diary Entries* has no filename
    (split-window-with-another-buf "REMINDER"))
  (server-start)) ;then in .bashrc: export EDITOR=emacsclient

(Home) ;or upon start-up, add: -f Home
