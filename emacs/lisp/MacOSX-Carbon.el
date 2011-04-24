;; MacOS X specific setting for Carbon Emacs

;; http://www.emacswiki.org/cgi-bin/wiki/CarbonEmacsPackage

;;(mac-hide-menu-bar)		 ;also disables MacOS Ctrl-F3 for Dock
(tool-bar-mode 0)
(transient-mark-mode 1)			; highlight selected region?

(defun tall ()
  (interactive)
  ;; 800x600 max height 40; 1024x786 max height 48; 1280x1024 max height 65
  (set-frame-position (selected-frame) 250 20)
  (set-frame-height (selected-frame) 44)
  (set-frame-width (selected-frame) 110))

(defun wide ()
  (interactive)
  ;; MacBookAir 11.6" using 185 columns allows edge of Mail list view to be visible underneath
  (set-frame-position (selected-frame) 10 20)
  (set-frame-height (selected-frame) 44)
  (set-frame-width (selected-frame) 185))

(tall)

(setq mac-option-modifier nil)

; reset this back to its normal setting:
(define-key esc-map " " 'just-one-space)
(global-unset-key "")	; was 'iconify-or-deiconify-frame (use Cmd-H on MacOSX)
(global-unset-key "") ; was 'iconify-or-deiconify-frame (use Cmd-H on MacOSX)

;; include 
(setq Man-switches (concat "-M " 
			   (let ((existing (getenv "MANPATH")))
			     (if existing
				 (concat existing ":")
				 "/usr/share/man:"))
			   "/usr/local/share/man:/usr/local/git/man:"
			   "/opt/local/share/man:/sw/share/man"))


;; http://common-lisp.net/project/slime/doc/html/slime.html
(add-to-list 'load-path "/usr/local/lisp/slime")
(add-to-list 'load-path "/usr/local/lisp/slime/contrib")

;(setq inferior-lisp-program "/opt/local/bin/ecl"
;(setq inferior-lisp-program "/opt/local/bin/clisp"
;(setq inferior-lisp-program "/usr/local/bin/ccl"
(setq inferior-lisp-program "/usr/local/bin/sbcl"
      ;; These really belong beneath /usr/share/doc/, but it's easier to keep all Lisp stuff together:
      cltl2-url "file:///usr/local/lisp/cltl/clm/node1.html"
      hyperspec-path "/usr/local/lisp/HyperSpec/"
      hyperspec-prog "/usr/local/lisp/slime/hyperspec"
      w3m-command "/opt/local/bin/w3m")

;(setenv "SBCL_HOME" "/usr/local/bin/sbcl")


;; Remote FreeBSD,Linux,Solaris versus local MacOSX:
(defvar remote-lisp-to-local-file-map '(("^/home/www/lib/" . "/usr/local/lisp/")
					("^/home/" . "/Users/"))
  "Order of definitions within map is preserved when applied.")

;; Local Emacs+SLIME connects to remote Lisp, and M-. views local source via different path:
;; Remote side should have been fed using REQUIRE or ASDF rather than merely LOAD.
(add-hook 'slime-edit-definition-hooks
	  (lambda (name &optional where)
	    ;; Based upon 2010-01-11 CVS edition of SLIME.
	    (let ((xrefs (slime-find-definitions-rpc name)))
	      ;; structure: (DEF ...) where DEF may contain (:location (:file "..."))
	      (mapcar (lambda (def)
			(mapcar (lambda (inner)
				  ;;older SLIME needs uppercase :LOCATION and :FILE here
				  (when (and (listp inner) (eq (car inner) :location)
					     (listp (cdr inner)) (eq (caadr inner) :file))
				    (mapcar (lambda (subst)
					      (setf (cadadr inner) (replace-regexp-in-string
								    (car subst) (cdr subst)
								    (cadadr inner))))
					    remote-lisp-to-local-file-map)))
				def))
		      xrefs)
	      (print xrefs)
	      (slime-edit-definition-cont xrefs name where))))

;;; Recommended for CCL:

;;(setq slime-net-coding-system 'utf-8-unix)
;;(slime-setup '(slime-fancy))


; Then, when ready to work on your Lisp code, type: M-x slime

;End.
