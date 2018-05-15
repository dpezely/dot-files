;;;; .emacs						-*- emacs-lisp -*-

;; When debugging on new versions, remember to start here: (view-emacs-news)

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
       (setq inferior-lisp-program "/usr/local/bin/sbcl --noinform"
	     hyperspec-prog "/usr/local/slime/hyperspec"
	     hyperspec-path "/usr/share/doc/HyperSpec/"
	     cltl2-url "file:///usr/share/doc/cltl/clm/node1.html"
	     w3m-command "/usr/local/bin/w3m")))

;; Sample instructions for new package installer:
;; https://github.com/Fuco1/smartparens/wiki/Quick-tour
(require 'package)
(require 'cl)

(load "utils")

;; http://ergoemacs.org/emacs/emacs_package_system.html
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(custom-set-variables
 '(holiday-other-holidays
   (quote ((holiday-float 2 1 3 "Family Day - statutory") ; since 2013 in BC
           (holiday-float 5 1 1 "Victoria Day" (- 24 6)) ; on or BEFORE 24th
           (holiday-fixed 7 1 "Canada Day")
           (holiday-float 8 1 1 "BC Day")
           (holiday-float 9 1 1 "Labour Day")
           (holiday-float 10 1 2 "Thanksgiving in Canada")
           (holiday-fixed 11 11 "Rememberance Day"))))
 '(package-selected-packages
   (quote (pkg-info let-alist
           markdown-mode markdown-mode+
           smartparens seq
           rust-mode rust-playground cargo
           racer ;rust-racer ;for M-.
           company ;for TAB key word completion, used by racer
           ;; flycheck flycheck-rust flymake-rust ;; http://www.flycheck.org
           lsp-mode   ;Language Server protocol https://github.com/emacs-lsp/lsp-mode
           lsp-python ;Python support for Language Server
           lsp-rust            ;Rust support for Language Server
           ;;lsp-java   ;Java support for Language Server

           jedi-core  ;Common code of jedi.el and company-jedi.el
           jedi-direx ;Tree style source code viewer for Python buffer
           jedi	      ;Python auto-completion for Emacs
           ;;jdee	      ;Java Development Environment for Emacs
           ;;jtags      ;enhanced tags functionality for Java development
           ;;thread-dump;Java thread dump viewer
           w3m
           ;; React:
           web-mode js2-mode json-mode))))

;; FIXME: might not be needed, especially as of Emacs 25
(defun Install-Local-Packages ()
  "Typically only run once after OS installation or upgrading Emacs"
  (interactive)
  (package-initialize)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (package-install pkg)))

(load "settings")
(load "keyboard")
(load "markdown-to-html5")
(load "storyteller")

;;(require 'slime) ; use QuickLisp's slime instead
(load "/usr/local/lisp/quicklisp/slime-helper.el")
(when (boundp 'slime-mode)
  (slime-setup))
;; Then, when ready to work on your Lisp code, type: M-x slime
;; Periodically, run from REPL: (progn (ql:update-client) (ql:update-all-dists))

;;; eww replaces w3m as of Emacs 24.4.
;; w3m is optional, used by SLIME for HyperSpec; may use Firefox instead.
;; You may need to manually install emacs-w3m:
;; ./configure --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs 
;;(add-to-list 'load-path (expand-file-name "~/emacs/lisp/emacs-w3m"))
;;(require 'w3m-load)
;;You may need to manually install: /Applications/Emacs.app/Contents/Resources/site-lisp/w3m/
;; (package-install "w3m")
;;(load "w3m")

;;;2013-07-31: use 'magit instead of 'git-emacs or 'git 
;;(setenv "PATH" (concat (getenv "PATH") ":/usr/local/git/bin"))
;; git clone git://github.com/tsgates/git-emacs.git
;;(require 'git-emacs)
;; (require 'git)
;; (defun git-call-process (buffer &rest args)
;;   "Wrapper for call-process that sets environment strings."
;;   (apply #'call-process "/usr/local/bin/git" nil buffer nil args))


;;(require 'arc)				; arclanguage.org, extras/arc.el
;;(require 'rust-mode)
;;(require 'erlang)

;; Added for React but maybe benefits other languages too:
;;(require 'flycheck)
;;;;(add-hook 'after-init-hook #'global-flycheck-mode)

;;(add-to-list 'load-path "~/.emacs.d/org-present")
;;(require 'org-present)

(display-time)
(display-battery-mode)

(defun Home () 
  "Upon starting emacs, upon login-- execute this function.
Within .xinitrc, add: /usr/local/bin/emacs -f Home -f wide &"
  (interactive)
  (find-file "~/etc/REMINDER")
  (diary)
  (calendar)
  (other-window 1)
  (unless (buffer-file-name)   ; because *Fancy Diary Entries* has no filename
    (split-window-with-another-buf "REMINDER"))
  (server-start)) ;then in .bashrc: export EDITOR=emacsclient
