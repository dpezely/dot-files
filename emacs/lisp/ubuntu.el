;; Ubuntu Linux

(tool-bar-mode 0)
(menu-bar-mode 0)
(transient-mark-mode 1)			; highlight selected region?

(when window-system
  (setq scroll-bar-mode 'right)
  ;; Force reset of placing scroll-bars:
  (scroll-bar-mode 0)
  (scroll-bar-mode 1)

  ;; Fix clipboard problem: (no longer needed, probably long before Emacs-25)
  ;; (setq select-enable-clipboard t
  ;;       interprogram-paste-function 'gui-selection-value)

  ;; To determine best font, start with the default by uncommenting
  ;; any use of 'set-default-font, restart Emacs, and evaluate:
  ;; (assoc 'font (frame-parameters))
  ;; (/ (display-pixel-width) (display-mm-width) 1.0)
  ;;(set-default-font "-*-UbuntuMono-normal-normal-normal-*-17-*-*-*-m-*-iso10646-1")
  (set-default-font "-*-DejaVu Sans Mono-normal-normal-*-*-17-*-*-*-m-*-iso10646-1")

  (defun tall ()
    (interactive)
    ;; 800x600 max height 40; 1024x786 max height 48; 1280x1024 max height 65
    (set-frame-position (selected-frame) 470 0)
    (set-frame-height (selected-frame) 50)
    (set-frame-width (selected-frame) 100))

  (defun wide ()
    (interactive)
    ;; 800x600 max height 40; 1024x786 max height 48; 1280x1024 max height 175x65
    (set-frame-position (selected-frame) 70 10)
    (set-frame-height (selected-frame) 49)
    (set-frame-width (selected-frame) 176)))

;; The rationale for this config is for a Linux workstation to
;; essentially be functionally identical to a deployed server, with
;; the obvious addition of Desktop toolchain.


;;(setenv "SBCL_HOME" "/usr/local/lib/sbcl")

;; http://common-lisp.net/project/slime/doc/html/slime.html
(add-to-list 'load-path "/usr/local/lisp/slime")
(add-to-list 'load-path "/usr/local/lisp/slime/contrib")

(setq inferior-lisp-program "/usr/local/bin/sbcl"
      ;; These really belong beneath /usr/share/doc/, but it's easier to keep all Lisp stuff together:
      cltl2-url "file:///usr/local/lisp/cltl/clm/node1.html"
      hyperspec-path "/usr/local/lisp/HyperSpec/"
      hyperspec-prog "/usr/local/lisp/slime/hyperspec"
      w3m-command "/usr/bin/w3m")

;; ;; Remote FreeBSD,Linux,Solaris versus local MacOSX:
;; (defvar remote-lisp-to-local-file-map '(("^/home/www/lib/" . "/usr/local/lisp/")
;; 					("^/home/" . "/Users/"))
;;   "Order of definitions within map is preserved when applied.")

;; ;; Local Emacs+SLIME connects to remote Lisp, and M-. views local source via different path:
;; ;; Remote side should have been fed using REQUIRE or ASDF rather than merely LOAD.
;; (add-hook 'slime-edit-definition-hooks
;; 	  (lambda (name &optional where)
;; 	    ;; Based upon 2010-01-11 CVS edition of SLIME.
;; 	    (let ((xrefs (slime-find-definitions-rpc name)))
;; 	      ;; structure: (DEF ...) where DEF may contain (:location (:file "..."))
;; 	      (mapcar (lambda (def)
;; 			(mapcar (lambda (inner)
;; 				  ;;older SLIME needs uppercase :LOCATION and :FILE here
;; 				  (when (and (listp inner) (eq (car inner) :location)
;; 					     (listp (cdr inner)) (eq (caadr inner) :file))
;; 				    (mapcar (lambda (subst)
;; 					      (setf (cadadr inner) (replace-regexp-in-string
;; 								    (car subst) (cdr subst)
;; 								    (cadadr inner))))
;; 					    remote-lisp-to-local-file-map)))
;; 				def))
;; 		      xrefs)
;; 	      (print xrefs)
;; 	      (slime-edit-definition-cont xrefs name where))))

(setenv "RUST_SRC_PATH"
	(concat (expand-file-name "~/")
		".rustup/toolchains/stable-x86_64-unknown-linux-gnu/"
		"lib/rustlib/src/rust/src"))
