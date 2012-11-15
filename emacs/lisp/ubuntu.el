;; Ubuntu Linux

(tool-bar-mode 0)
(menu-bar-mode 0)
(transient-mark-mode 1)			; highlight selected region?

(when window-system
  (setq scroll-bar-mode 'right)
  ;; Force reset of placing scroll-bars:
  (scroll-bar-mode 0)
  (scroll-bar-mode 1)

  ;; fix clipboard problem:
  (setq x-select-enable-clipboard t
	interprogram-paste-function 'x-cut-buffer-or-selection-value)

  ;;(set-default-font "-*-LucidaTypewriter-Normal-R-*-Sans-17-*-*-*-M-*-ISO8859-1")
  ;;(set-default-font "-*-LucidaTypewriter-Medium-R-*-Sans-15-*-*-*-M-*-ISO8859-1")

  (defun tall ()
    (interactive)
    ;; 800x600 max height 40; 1024x786 max height 48; 1280x1024 max height 65
    (set-frame-position (selected-frame) 140 1)
    (set-frame-height (selected-frame) 49)
    (set-frame-width (selected-frame) 100))

  (defun wide ()
    (interactive)
    ;; 800x600 max height 40; 1024x786 max height 48; 1280x1024 max height 65
    (set-frame-position (selected-frame) 60 50)
    (set-frame-height (selected-frame) 49)
    (set-frame-width (selected-frame) 182)))

;;
;; The rationale for this config is for a Linux workstation to
;; essentially be functionally identical to a deployed server, with
;; the obvious addition of Desktop toolchain.
;;
;; (This is in contrast to a MacOSX laptop with VMware guest OS image
;; to mimic a server, whereby paths and other configuration details
;; will necessarily differ.)
;;
;; e.g., production web servers will likely have everything under
;; /home/www/, including slime and other Lisp libraries. 
;; 


(setenv "SBCL_HOME" "/usr/local/lib/sbcl")

;; http://common-lisp.net/project/slime/doc/html/slime.html
(add-to-list 'load-path "/home/www/lib/slime")
(add-to-list 'load-path "/home/www/lib/slime/contrib")
(setq inferior-lisp-program "/usr/local/bin/sbcl"
      ;; These really belong beneath /usr/share/doc/, but it's easier to keep all Lisp stuff together:
      cltl2-url "file:///usr/share/doc/cltl/clm/node1.html"
      hyperspec-path "/usr/share/doc/HyperSpec/"
      hyperspec-prog "/home/www/lib/slime/hyperspec"
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
