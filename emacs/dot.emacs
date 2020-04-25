;;;; .emacs						-*- emacs-lisp -*-

;; When debugging on new versions, remember to start here: (view-emacs-news)
;; and try: (setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "~/emacs/lisp"))
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file t)

(cond ((string-equal system-type "gnu/linux")
       (if (string-match "--prefix=/nix" system-configuration-options)
	   (load "nixos")
	 (load "ubuntu")))
      ((string-equal system-type "darwin")
       (cond ((eq window-system 'ns)		; Cocoa aka NeXTSTEP
              (load "macos"))
             ((eq window-system 'mac)
              (if (boundp 'aquamacs-version)
                  (load "MacOSX-Aquamacs")	;Aqua supersedes Carbon
                (load "MacOSX-Carbon")))))
      ((string-equal system-type "windows-nt")
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

;; http://ergoemacs.org/emacs/emacs_package_system.html
;; See settings.el for instances of USE-PACKAGE
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(setq package-archive-priorities '(("melpa" . 10) ("gnu" . 9))
      package-enable-at-startup nil
      use-package-always-ensure t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (use-package cl))

(load "utils")
(load "settings")
(load "keyboard")
(load "markdown-to-html5" t)

;; Use QuickLisp's slime instead of: (require 'slime)
;; Then, periodically eval this to get updates:
;; (slime-eval-with-transcript (progn (ql:update-client) (ql:update-all-dists)))
;; Allow this fail silently, because it's likely due to a fresh OS install:
(load (expand-file-name "~/quicklisp/slime-helper.el") t)
(when (boundp 'slime-mode)
  (slime-setup))

(display-time)
;;(display-battery-mode)

(defun Home () 
  "Upon starting emacs, upon login-- execute this function.
  Within ~/.xinitrc, add: /usr/local/bin/emacs -f Home -f wide &
  Add to ~/.bashrc: export EDITOR=emacsclient"
  (interactive)
  (find-file "~/etc/REMINDER")
  (diary)
  (calendar)
  (other-window 1)
  (unless (buffer-file-name)   ; because *Fancy Diary Entries* has no filename
    (split-window-with-another-buf "REMINDER"))
  (server-start))

;; ;; For macos:
;; (Home)
;; (use-laptop-display nil)
;; (wide)
