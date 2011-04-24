;;
;; XP specific setting for Emacs
;; 
;; http://www.gnu.org/software/emacs/windows/ntemacs.html
;;
(set-message-beep 'silent)		;quiet!

(menu-bar-mode -1)
(global-font-lock-mode)

; Most readable on a large screen for source code:
(set-default-font "-*-Fixedsys-normal-r-*-*-12-*-*-*-c-*-iso8859-*")
; common typewriter font for even columns of source code:
;(set-default-font "-*-Courier-normal-r-*-*-12-*-*-*-c-*-iso8859-*")
; 
; sans serif font more common for text than source code:
;(set-default-font "-*-Lucida Sans Unicode-normal-r-*-*-12-*-*-*-p-*-iso8859-15")
;(set-default-font "-*-Arial-normal-r-*-*-12-*-*-*-p-*-iso8859-15")




;;
;; ispell4:
;;
(setq local-ispell-path "c:/programs/emacs/ispell4/")
(autoload 'ispell-word (concat local-ispell-path "ispell4")
  "Check spelling of word at or before point" t)
(autoload 'ispell-complete-word (concat local-ispell-path "ispell4")
  "Complete word at or before point" t)
(autoload 'ispell-region (concat local-ispell-path "ispell4")
  "Check spelling of every word in the region" t)
(autoload 'ispell-buffer (concat local-ispell-path "ispell4")
  "Check spelling of every word in the buffer" t)
(setq ispell-command (concat local-ispell-path "exe/ispell.exe")
      ispell-look-dictionary (concat local-ispell-path "ispell.words")
      ispell-look-command (concat local-ispell-path "exe/look.exe")
      ispell-command-options (list "-d" 
				   (concat local-ispell-path "ispell.dict")))


;;
;; shell stuff
;;


;; http://sources.redhat.com/cygwin/faq/
;; mount -s --change-cygdrive-prefix /

(setq exec-path (cons "C:/cygwin/bin" exec-path))
(setenv "PATH" (concat "C:\\cygwin\\bin;" (getenv "PATH")))
;;
;; NT-emacs assumes a Windows command shell, which you change here.
(setq process-coding-system-alist '(("bash" . undecided-unix)))
(setq w32-quote-process-args ?\")
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name) 
(setq explicit-shell-file-name shell-file-name) 
;;
;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
;;
(add-hook 'comint-output-filter-functions
	  'comint-strip-ctrl-m)


;; 800x600
;(set-frame-position (selected-frame) 60 0)
;(set-frame-height (selected-frame) 40)
;(set-frame-width (selected-frame) 95)

;; 1024x786
;(set-frame-position (selected-frame) 60 0)
;(set-frame-height (selected-frame) 45)
;(set-frame-width (selected-frame) 95)

;; 1280x1024
(set-frame-position (selected-frame) 300 0)
(set-frame-height (selected-frame) 65)
(set-frame-width (selected-frame) 95)




;; open every file in binary mode except for batch and .txt files:
;(setq file-name-buffer-file-type-alist '(("\.bat$" . nil)
;					 ("\.txt$" . nil)
;					 ("\.script$" . nil)
;					 (".*" . t)))
(setq file-name-buffer-file-type-alist '(("\.py$" . t)
					 ("\.sh$" . t)
					 ("\.txt$" . t)
					 ("\.xhtml$" . t)
					 ("\.html$" . t)
					 (".emacs$" . t)
					 ("\.el$" . t)
					 (".*" . nil)))






; Make Emacs behave as an ordinary Windows based editor
; http://www.gnu.org/software/emacs/windows/faq3.html#windows-like

;(transient-mark-mode t)
;(delete-selection-mode t)
;;(pc-selection-mode)



; Standard Windows application keymaps (C-c, C-v, C-x, etc.)
; If you are used to standard Windows application keybindings, such as
; C-c for copy, C-v for paste, and C-x for cut, or selecting text with
; <shift>-movement, then you probably want to use Kim Storms
; <storm@cua.dk> cua.el available here:  http://www.cua.dk/cua.html

; Only when the region is currently active (and highlighted since
; transient-mark-mode is used), the C-x and C-c keys will work as CUA
; keys, i.e. cut and copy, but when the region is not active, C-x and
; C-c works as ordinary emacs prefix keys!
;;(require 'cua)
;;(CUA-mode t)
