;;
;; MacOS X specific setting for Emacs
;; 
;; http://aquamacs.org/
;; http://www.emacswiki.org/cgi-bin/wiki/CustomizeAquamacs

;; See also ~/Library/Preferences/Aquamacs Emacs/customizations.el
;; (runs *after* ~/.emacs completes!)

;; Be sure to comment-out this line from customizations.el after saving:
;(custom-set-variables
;bad aquamacs!; '(aquamacs-auto-frame-parameters-flag nil)
;    '(w3m-pop-up-frames t)
;    '(w3m-pop-up-windows nil)



;;(if (boundp 'aquamacs-version) 
;;    (load "MacOSX-Aquamacs")
;;  (load "MacOSX"))

(setq aquamacs-auto-frame-parameters-flag nil)


;(setq initial-frame-alist '((top . 0) (left . 60) (width . 95) (height . 48)))
(set-default-font "-*-*-medium-r-normal-*-12-*-*-*-*-*-fontset-monaco12")

;; force Emacs kill-ring and MacOS/X clipboard to be one:
(setq x-select-enable-clipboard t)

(setq mac-command-modifier 'meta
      mac-option-modifier 'hyper)

(setq mac-pass-option-to-system nil)

;; See ~/.emacs
;; CUA supports C-z, C-x, C-c and C-v for undo, cut, copy, and paste
;;(cua-mode 0)
;;(menu-bar-mode 0)			; Can't disable menu on MacOSX
;;(tool-bar-mode 0)
;;(transient-mark-mode 0)		; 0 disables highlighting of region

(setq one-buffer-one-frame nil)		;fix Aquamacs Emacs silliness

(setq global-font-lock-mode 1)		; syntax highlighting
(global-font-lock-mode 1)


;For when you want a new Frame: (because Aquamacs can't have multiples running)
(defun override-new-frame-with-new-scratch ()
  "Spawn new frame when default behavior uses only one frame.
   Specific to Aquamacs Emacs: (setq one-buffer-one-frame nil)
   Bind this function to a key such as M-n or H-n"
  (interactive)
  (let ((one-buffer-one-frame t))
    (new-frame-with-new-scratch)))

(define-key osx-key-mode-map (kbd "M-n") 'override-new-frame-with-new-scratch)



;; CUA supports C-z, C-x, C-c and C-v for undo, cut, copy, and paste
(cua-mode 0)
(transient-mark-mode 1)	   ; 1 enables highlighting of selected region
;(menu-bar-mode 0)
(tool-bar-mode 0)

; 800x600 max height 40; 1024x786 max height 48; 1280x1024 max height 95
(defun force-window-resize ()
  "because Aquamacs Emacs won't reposition the frame correctly, force it!"
  (interactive)
  (set-frame-position (selected-frame) 60 0)
  (set-frame-height (selected-frame) 48)
  (set-frame-width (selected-frame) 95))

;End.