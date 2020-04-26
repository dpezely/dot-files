;; NixOS Linux

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
  ;; Dell XPS 13" laptop: Full HD:
  (set-default-font "-*-DejaVu Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1")
  ;; Samsung 13" laptop: Full HD, 18.04, 120dpi => 17pt font:
  ;;(set-default-font "-*-DejaVu Sans Mono-normal-normal-*-*-17-*-*-*-m-*-iso10646-1")
  ;; external 24" Full HD:
  ;;(set-default-font "-*-DejaVu Sans Mono-normal-normal-*-*-15-*-*-*-m-*-iso10646-1")
  ;;(set-default-font "-*-FreeMono-normal-normal-normal-*-17-*-*-*-m-*-iso10646-1")
  ;;(set-default-font "-*-LucidaTypewriter-Normal-R-*-Sans-17-*-*-*-M-*-ISO8859-1")
  ;;(set-default-font "-*-LucidaTypewriter-Medium-R-*-Sans-15-*-*-*-M-*-ISO8859-1")

  (defun tall ()
    (interactive)
    ;; 800x600 max height 40; 1024x786 max height 48; 1280x1024 max height 65
    (set-frame-position (selected-frame) 470 0)
    (set-frame-width (selected-frame) 100)
    (set-frame-height (selected-frame) 45))

  (defun wide ()
    (interactive)
    ;; 800x600 max height 40; 1024x786 max height 48; 1280x1024 max height 175x65
    (set-frame-position (selected-frame) 70 0)
    (set-frame-width (selected-frame) 160)
    (set-frame-height (selected-frame) 44)))

;;(setenv "SBCL_HOME" "/run/current-system/sw/bin/sbcl")

;; http://common-lisp.net/project/slime/doc/html/slime.html

;; FIXME: work-in-progress
;; See also /run/current-system/sw/lib/common-lisp-settings/*.sh

(add-to-list 'load-path "/run/current-system/sw/lib/common-lisp/swank/slime")
(add-to-list 'load-path "/run/current-system/sw/lib/common-lisp/swank/contrib")

(setq inferior-lisp-program "/run/current-system/sw/bin/sbcl"
      cltl2-url "file:///usr/local/lisp/cltl/clm/node1.html"
      hyperspec-path (car (file-expand-wildcards ;FIXME avoid wildcards
                           "/nix/store/*-hyperSpec-*/library/hyperSpec/"))
      hyperspec-prog "/run/current-system/sw/lib/common-lisp/swank/lib/hyperspec")
