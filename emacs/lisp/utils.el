;; utils.el


(fset 'exit 'save-buffers-kill-emacs)

(fset 'make 'compile)
(fset 'run  'compile)

;; See keyboard.el for where these are used:

(defun stty-window-size ()
  "tell a shell from within emacs what the size of the window is --pez"
  (interactive)
  (insert "stty rows "  (int-to-string (/ (screen-height) 2)) 
	  " columns " (int-to-string (screen-width)) " "))

(defun scroll-half-window-up () 
  "scroll half the window; force a redraw of window --pez"
  (interactive)
  (scroll-up  (/ (window-height) 2)))

(defun scroll-half-window-down () 
  "scroll half the window; force a redraw of window --pez"
  (interactive)
  (scroll-down (/ (window-height) 2)))

(defun top-of-window ()
  "go to top of window --pez"
  (interactive)
  (move-to-window-line 0))

(defun bottom-of-window ()
  "go to bottom of window --pez"
  (interactive)
  (move-to-window-line -1))

(defun clear-buffer ()
  "Pez-ism: nuke the current buffer contents"
  (interactive) 
  (delete-region (point-min) (point-max)))

(defun zap ()
  "Pez-ism: nuke to end of buffer"
  (interactive) 
  (delete-region (point) (point-max)))

(defun previous-other-window ()
  "Pez-ism: (other-window -1)"
  (interactive)
  (other-window -1))

(defun split-window-with-another-buf (buf)
  "Pez-ism: split the current window, but change the other window 
  to another buffer, rather than defaulting to same buffer"
  (interactive "BBuffer: ")
  ;;#+emacs24 (split-window-below)
  ;;#-emacs24
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer buf))

(defun split-window-horiz-with-another-buf (buf)
  "Pez-ism: split the current window, but change the other window 
  to another buffer, rather than defaulting to same buffer"
  (interactive "BBuffer: ")
  ;;#+emacs24 (split-window-right)
  ;;#-emacs24
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer buf))

(defun move-to-center-of-line () 
  "Pez-ism: horizontal equiv to move-to-window-line"
  (interactive)
  (end-of-line)
  (move-to-column (/ (current-column) 2)))


;; Circa 1989, 'template' was an unused keyword in C++, and these
;; commands were an attempt to generate code in a parameterized way.
(defun expand-macros (sub) 
  "pez-ism: eval lisp code in non-lisp file such as .c file.
   fn should be of the form:   %%(elisp)
   and must be at the begining of a new line.
   see ~pez/emacs/lisp/coding.el for an example."
  (interactive "sPerform substitution? [Y]")
  (beginning-of-buffer)
  (while  (search-forward-regexp "^%%(.*)" nil t) 
    (if (not (string= sub ""))
	(forward-char))
    (eval-last-sexp nil)
    (if (string= sub "")
	(replace-match "")
      (insert 10))))

(defun expand-macros-c-code ()
  "pez-ism: eval lisp code imbedded in c style /*...*/ comments.
   fn should be of the form:   /*%%(elisp)*/ or /*@@(elisp)*/
   and may be located anywhere, on any line.
   where %% means do NOT print result in buffer and @@ means DO print.
   In both cases, the original comment block will be removed.
   See ~pez/emacs/lisp/coding.el for an example."
  (interactive)
  (beginning-of-buffer)
  (while  (search-forward-regexp "/\*%%(.*)" nil t) 
    (eval-last-sexp nil)
    (replace-match "")
    (search-forward-regexp ".*\*/" nil t)
    (replace-match ""))
  (beginning-of-buffer)
  (while  (search-forward-regexp "/\*@@(.*)" nil t) 
    (eval-last-sexp t)
    (replace-match "")
    (search-forward-regexp ".*\*/" nil t)
    (replace-match "")
    (insert temp)))


(defun rn ()
  "Defer loading GNUS... was useful on BSD VAXen when 4MB RAM was a lot"
  (interactive)
  (load "rn")
  (gnus))

(defun ssh (host)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*ssh-HOST*'.
Normally input is edited in Emacs and sent a line at a time."
  (interactive "sOpen SSH connection to host: ")
  (require 'telnet)
  (require 'shell)
  (let ((name (concat "ssh-"  host)))
;;    (pop-to-buffer (make-comint name "/usr/bin/ssh" nil host "-X"))
    (pop-to-buffer (make-comint name "/usr/bin/ssh" nil host))
;; old; circa 1990's
;;    (set-process-filter (get-process name) 'telnet-initial-filter)
;;    (telnet-mode)
;;    (setq telnet-count -16)
    (shell-mode)))



;; Remote SLIME
;; See also http://www.a1k0n.net/blah/archives/2005/11/04/T18_00_44/index.html
 
(defun remote-slime (host local-port remote-port)
  "Connect to remote lisp for SLIME via detachtty so you may continue
a session even if your network connection has been lost in the mean
time."
  (interactive (concat "sOpen SLIME connection to host: \n"
		       "sLocal port: [4005] \n"
		       "sRemote port: [4005] "))
  (require 'telnet)
  (require 'shell)
  (let* ((i (position (string-to-char "@") host))
	 (short-name (if i
			 (subseq host (1+ i))
			 host))
	 (buffer-name (concat "remote-slime-"  short-name)))
    (pop-to-buffer 
     (make-comint buffer-name "/usr/bin/ssh" nil
		  "-A" ;"-X"
		  "-L" (format "%d:localhost:%d" 
			       (or local-port 4005)
			       (or remote-port 4005))
		  host))
    (set-process-filter (get-process buffer-name) 'telnet-initial-filter)
    (telnet-mode)
    (setq telnet-count -16)
    (shell-mode)))
;;(remote-slime "danielp@wfc-sys-zip-001" 4005 4005)
;;(remote-slime "danielp@wfc-zit-dev-001" 4005 4005)


(defun fi:extract-list (arg)
  "Take the list after the point and remove the surrounding list.  With
argument ARG do it that many times."
  (interactive "p")
  (let ((string (progn
		  (mark-sexp 1)
		  (buffer-substring (point) (mark t)))))
    (backward-up-list (or arg 1))
    (mark-sexp 1)
    (delete-region (point) (mark t))
    (insert string)
    (backward-sexp 1)))

(defun insert-matched-pair (opening closing &optional backwards)
  "Insert matched pairs such as Begin End or { }, and place cursor in between"
  (interactive)
  (if (region-active-p)
      (let ((start (point))
	    (end (+ (mark t) (length opening) (length closing)))
	    (string (buffer-substring (region-beginning) (region-end))))
	(delete-region start (mark t))
	(insert opening string closing)
	(backward-char (or backwards 1))
	(indent-region start end))
      (let ((back-count (if backwards
			    (- backwards (count-if (lambda (x)
						     (= x 10)) ; \n
						   (string-to-list closing)))
			    1)))
	(insert (replace-regexp-in-string "\n" "" opening)
		(replace-regexp-in-string "\n" "" closing))
	(backward-char back-count))))

(defun mark-rust-statement ()
  (interactive)
  ;; FIXME: add parser; in presence of "{", skip to matching "}"
  (let ((sentence-end "[;}]"))
    (mark-end-of-sentence 1)))

(defun insert-rust-parens ()
  (interactive)
  (insert-matched-pair "(" ")"))

(defun insert-rust-curlies ()
  (interactive)
  (insert-matched-pair "{\n" "\n}\n" 3))

(defun insert-rust-square-brackets ()
  (interactive)
  (insert-matched-pair "[" "]"))

(defun insert-rust-angle-brackets ()
  (interactive)
  (insert-matched-pair "<" ">"))

(defun insert-rust-block ()
  (interactive)
  (insert-matched-pair "|| {\n" "\n}\n" 3))
