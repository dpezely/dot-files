;; Arch Linux

(tool-bar-mode 0)
(menu-bar-mode 0)
(transient-mark-mode 1)			; highlight selected region

(when window-system
  (defun tall ()
    (interactive)
    (set-frame-position (selected-frame) 140 80)
    (set-frame-height (selected-frame) 49)
    (set-frame-width (selected-frame) 100))

  (defun wide ()
    (interactive)
    (set-frame-position (selected-frame) 60 80)
    (set-frame-height (selected-frame) 49)
    (set-frame-width (selected-frame) 182)))

;; The rationale for this config is for a Linux workstation to
;; essentially be functionally identical to a deployed server, with
;; the obvious addition of Desktop toolchain.

;; http://common-lisp.net/project/slime/doc/html/slime.html
(setq inferior-lisp-program "/usr/bin/sbcl"
      cltl2-url "file:///usr/local/lisp/cltl/clm/node1.html"
      hyperspec-path "/usr/local/lisp/HyperSpec/"
      w3m-command "/usr/bin/w3m")
