;; You may think my key mappings are weird, and you're right!
;;
;; Emacs standard mapping convention used: 
;; Custom user stuff is ^C-whatever, exceptions are for cursor movement.

;; Unmapped 'cause they're just plain annoying: some fixed in emacs-19.
;;(global-unset-key "\C-x\C-u")    ; upcase-region
(global-unset-key "\C-xf")         ; set-fill-column
;;(global-unset-key "\M-g")        ; fill-region
;;(global-unset-key "\C-[\C-[")    ; was just obnoxious in emacs-18
(global-unset-key "\C-x\C-c")      ; kill-emacs; reset later for shell

(global-set-key "\C-h" 'backward-delete-char) ; was beginning of help sequence

;; Miscellaneous new key mappings:
(global-set-key "\C-c " 'set-mark-command)
(global-set-key "\C-ci" 'insert-buffer)		; complements C-x i
(global-set-key "\C-co" 'previous-other-window) ; Pez-ism: C-X o in reverse
(global-set-key "\C-cu" 'undo) 
(global-set-key "\C-c." 'bury-buffer) 
(global-set-key "\C-c;" 'comment-region)

;; reset font size by evaluating: (text-scale-adjust 0)
(define-key esc-map "+" 'text-scale-increase)
(define-key esc-map "-" 'text-scale-decrease)

;; Shell stuff:
;; (global-set-key "\C-xc" 'shell)      ; was relevent for emacs-19
;; (global-set-key "\C-x\C-c" 'shell)   ; instead of 'kill-emacs
(global-set-key "\C-x\C-c" 'shell)	; instead of 'kill-emacs
(global-set-key "\C-xc" 'shell)
(global-set-key "\C-ct" 'telnet)
(global-set-key "\C-cr" 'ssh)
;;(global-set-key "\C-cp" 'send-invisible) ;eg: for passwords

;; To fly around the screen: (fns mostly Pez-isms)
(define-key esc-map "P" 'scroll-half-window-down)
(define-key esc-map "N" 'scroll-half-window-up) 
(define-key esc-map "[" 'top-of-window) 
(define-key esc-map "]" 'bottom-of-window)
(define-key esc-map "m" 'move-to-window-line) ; Middle: or unintuitive M-r
(global-set-key "\C-cm" 'move-to-center-of-line)
(define-key esc-map "=" 'goto-line) ; was 'count-lines-region

;; Altered mappings:
(define-key esc-map "-" 'shrink-window)
(global-set-key "\C-x2" 'split-window-with-another-buf) ;was 'split-window-vertically
(global-set-key "\C-x3" 'split-window-horiz-with-another-buf) ;was 'split-window-horizontally

(add-hook 'shell-mode-hook '(lambda () 
			     (define-key shell-mode-map "\C-c." 'bury-buffer)))

(add-hook 'org-mode-hook '(lambda () 
			   (define-key org-mode-map "\C-c\C-a" 'show-all)
			   (define-key org-mode-map "\C-e" 'move-end-of-line)
			   (define-key org-mode-map "\C-c." 'bury-buffer)))

(add-hook 'c-mode-hook '(lambda ()
			 (define-key c-mode-map    "\C-m" 'newline-and-indent)
			 (define-key c-mode-map    "\n"   'newline-and-indent)
			 (define-key c-mode-map    "\C-c." 'bury-buffer)))

(add-hook 'c++-mode-hook '(lambda () 
			   (define-key c++-mode-map  "\C-m" 'newline-and-indent)
			   (define-key c++-mode-map  "\n"   'newline-and-indent)
			   (define-key c++-mode-map  "\C-c." 'bury-buffer)))

(add-hook 'java-mode-hook '(lambda ()
			    (define-key java-mode-map "\C-m" 'newline-and-indent)
			    (define-key java-mode-map "\n"   'newline-and-indent)
			    (define-key java-mode-map "\C-c." 'bury-buffer)))

(add-hook 'lisp-mode-hook '(lambda ()
			    (define-key lisp-mode-map "\C-c%" 'fi:extract-list)
			    (define-key lisp-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'html-mode-hook '(lambda ()
			    (define-key html-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'w3m-mode-hook '(lambda ()
			   (define-key esc-map "[" 'top-of-window) 
			   (define-key esc-map "]" 'bottom-of-window)))
