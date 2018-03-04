;; settings.el

(setq inhibit-startup-screen t
      initial-scratch-message nil
      ispell-dictionary "en_CA"
      scroll-step 2
      display-time-day-and-date t
      battery-mode-line-format "[%b%p%%]"
      sentence-end-double-space t
      ;;truncate-partial-width-windows t
      ;;truncate-lines t
      pop-up-windows nil
      pop-up-frames nil
      same-window-buffer-names '("*shell*" "*slime-scratch*" "*xref*")
      same-window-regexps '("^\*sldb sbcl/")
      version-control nil
      delete-old-versions t)

(setq calendar-today-visible-hook 'calendar-star-date
      calendar-view-diary-initially-flag t
      diary-display-function 'diary-fancy-display
      diary-list-entries-hook 'diary-include-other-diary-files
      diary-number-of-entries 5
      diary-file (expand-file-name "~/etc/SCHEDULE"))

;; Have diary mode notify me of any appointments:
(if (< emacs-major-version 24)
    (add-hook 'diary-hook 'appt-make-list)
    (appt-activate 1))

(calendar-set-date-style 'european)

(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)

;; (setq Man-switches (concat "-M " 
;; 			   (let ((existing (getenv "MANPATH")))
;; 			     (if existing
;; 				 (concat existing ":")))
;; 			   "/usr/man:/usr/local/share/man:/usr/local/git/man"))


;; Used for Shell and SSH: (or manually use M-x send-invisible)
(add-hook 'comint-mode-hook 
	  #'(lambda () 
	      (line-number-mode 1)
	      (setq comint-password-prompt-regexp "[Pp]ass\\(word\\|[ ]*[Pp]hrase\\).*[:]")
	      (setq comint-output-filter-functions '(comint-watch-for-password-prompt
						     comint-postoutput-scroll-to-bottom))))

;; set up unicode
(set-language-environment "UTF-8")	;see also 'slime-net-coding-system var
(unless (getenv "LANG")
  (setenv "LANG"     "en_CA.UTF-8"))	;for external programs; e.g., SBCL
(unless (getenv "LC_CTYPE")
  (setenv "LC_CTYPE" "en_CA.UTF-8"))

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;;; settings for Common Lisp development

(setq lisp-indent-function 'common-lisp-indent-function
      ;;slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-startup-animation nil
      ;;See OS-specific startup .el for values:
      ;; hyperspec-prog "/usr/local/slime/hyperspec"
      ;; hyperspec-path "/usr/share/doc/HyperSpec/"
      ;; w3m-command "/usr/local/bin/w3m"
      common-lisp-hyperspec-root (concat "file://" hyperspec-path)
      common-lisp-hyperspec-symbol-table (concat hyperspec-path "Data/Map_Sym.txt")

      ;;See http://www.emacswiki.org/emacs/emacs-w3m
      ;; browse-url-browser-function 'w3m
      w3m-home-page cltl2-url
      w3m-symbol 'w3m-default-symbol
      w3m-key-binding 'info
      w3m-coding-system 'utf-8
      w3m-default-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8

      slime-net-coding-system 'utf-8-unix
      paren-priority 'close
      paren-match-face 'bold
      paren-sexp-mode t)


;;http://www.emacswiki.org/cgi-bin/wiki/mic-paren.el
(require 'mic-paren)
(paren-activate)

;;(add-to-list 'slime-contribs 'slime-cl-indent)
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))

(add-hook 'lisp-mode-hook #'(lambda () 
			      (setq lisp-indent-function 'common-lisp-indent-function
				    ;;common-lisp-style "sbcl"
				    font-lock-maximum-decoration t)
			      (paren-toggle-matching-quoted-paren 1)
			      (show-paren-mode t)))

;;http://mumble.net/~campbell/emacs/paredit.html
;(add-hook 'lisp-mode-hook #'(lambda () (paredit-mode 1)))

;(font-lock-add-keywords 
; 'lisp-mode
; ;allegro macros:
; '(("\\<\\(if\\*\\|then\\|elseif\\|else\\)\\>" . font-lock-keyword-face)))



;;; for remote slime sessions: [watch "lisp movies"]

;; On local shell:
;; ssh -L4005:127.0.0.1:4005 daniel@shell.example.com

;; On local Emacs:
;; (let ((remote-path "/ssh:daniel@shell.example.com:"))
;;   (setf slime-translate-to-lisp-filename-function
;; 	(lambda (filename)
;; 	  (subseq filename (length remote-path)))
;; 	slime-translate-from-lisp-filename-function
;; 	(lambda (filename)
;; 	  (concat remote-path filename))))

;; Use single channel for slime sessions to remote hosts.
;; On remote lisp:
;(asdf:operate 'asdf:load-op :swank)
;(setf swank:*use-dedicated-output-stream* nil)
;(swank:create-server :port 4005 :dont-close t)


(add-hook 'org-mode-hook
	  (lambda ()
	    (setq org-export-author-info nil
		  ;;rg-export-with-toc nil ;;Instead, use: #+OPTIONS: toc:nil 
		  org-export-email-info nil
		  org-export-time-stamp-file nil
		  org-export-headline-levels 2
		  org-use-sub-superscripts nil
		  org-emphasis-alist (delete-if (lambda (x)
						  (equal (car x) "+"))
						org-emphasis-alist)
		  org-export-html-coding-system 'utf-8
		  org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
		  ;; org-export-html-style-extra
		  ;;    "<style type=\"text/css\"><!--/*--><![CDATA[/*><!--*/
		  ;; 	body {font-family:sans-serif}
		  ;; 	a {text-decoration:none}
		  ;; 	#table-of-contents {font-size:75%}
		  ;; 	/*]]>*/--></style>"
		  org-html-postamble nil
		  org-export-copy-to-kill-ring nil)))

(add-hook 'org-present-mode-hook
	  (lambda ()
	    (org-present-big)
	    (org-display-inline-images)))

(add-hook 'org-present-mode-quit-hook
	  (lambda ()
	    (org-present-small)
	    (org-remove-inline-images)))

(add-hook 'text-mode-hook
	  #'(lambda () 
	      (set-fill-column 76) 
	      (visual-line-mode nil)))

(add-hook 'c-mode-common-hook
	  #'(lambda ()
	      ;;(hide-ifdef-mode)
	      ;;(setq tab-width 4)
	      ;;(c-set-style "linux")
	      ;;(c-set-style "bsd")
	      (set-fill-column 79)
	      (auto-fill-mode 1)))

;; Use eslint instead for ECMAScript 2015 but flycheck for everything else
;; http://www.flycheck.org/en/manual/latest/index.html
;;(setq flycheck-disabled-checkers '(javascript-jshint json-jsonlist)
;;      flycheck-temp-prefix ".flycheck")

;; use eslint with web-mode for jsx files
;;(flycheck-add-mode 'javascript-eslint 'web-mode)

(add-hook 'web-mode-hook
	  #'(lambda ()
	      ;;(flycheck-mode)
	      ;; See web-mode.org
	      (setq web-mode-markup-indent-offset 2
		    web-mode-css-indent-offset 2
		    web-mode-code-indent-offset 2)
	      (defadvice web-mode-highlight-part (around tweak-jsx activate)
		(if (equal web-mode-content-type "jsx")
		    (let ((web-mode-enable-part-face nil))
		      ad-do-it)
		    ad-do-it))))

(add-hook 'javascript-mode-hook
	  #'(lambda ()
	      ;;(setq tab-width 4)
	      (set-fill-column 79)
	      (auto-fill-mode 1)))

(add-hook 'go-mode-hook
	  #'(lambda ()
	      ;;(setq tab-width 4)
	      (add-hook 'before-save-hook #'gofmt-before-save)))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;(autoload 'ruby-mode "ruby-mode" "Load ruby-mode")
;;(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;;(add-hook 'ruby-mode-hook 'turn-on-font-lock)
;;(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;;(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode)) ;ruby on rails
;;(add-to-list 'auto-mode-alist '("\\.xhtml$" . html-mode))
;;(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
;;(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
;;(add-to-list 'auto-mode-alist '("\\.mxml$" . xml-mode)) ;Flex/Flash
;;(add-to-list 'auto-mode-alist '("\\.php$" . perl-mode))
;;(add-to-list 'auto-mode-alist '("\\.inc$" . perl-mode))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.arc$" . arc-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))


(add-to-list 'completion-ignored-extensions ".swf") ; shockwave-flash
(add-to-list 'completion-ignored-extensions ".beam") ; Erlang VM

(add-hook 'scheme-mode-hook #'(lambda () 
				(setq font-lock-maximum-decoration t)
				(paren-toggle-matching-quoted-paren 1)
				(show-paren-mode t)))

(add-hook 'erlang-mode-hook
	  #'(lambda () 
	      (setq indent-tabs-mode nil ;Play nice with non-Emacs heathens
		    ;; Add paths to ../../*/deps/ebin for running & compiling:
		    ;; (Restart *erlang* shell when new dependancies are added)
		    inferior-erlang-machine-options
		    (append (remove nil
				    (mapcan (lambda (app-path)
					      (let ((ebin (concat app-path "/ebin")))
						(when (file-readable-p ebin)
						  (list "-pa" ebin))))
					    (directory-files "../.." t "[^.]$")))
			    (remove nil
				    (mapcan (lambda (dep)
					      (let ((dep-path (concat dep "/deps")))
						(when (file-readable-p dep-path)
						  (mapcan (lambda (dir)
							    (list "-pa" (concat dir "/ebin")))
							  (directory-files dep-path t "[^.]$")))))
					    (directory-files "../.." t "[^.]$"))))
		    erlang-compile-extra-opts
		    (append (remove nil
				    (mapcan (lambda (app-path)
					      (let ((ebin (concat app-path "/ebin")))
						(when (file-readable-p ebin)
						  (list "-pa" ebin))))
					    (directory-files "../.." t "[^.]$")))
			    (remove nil
				    (mapcan (lambda (dep)
					      (let ((dep-path (concat dep "/deps")))
						(when (file-readable-p dep-path)
						  (mapcar (lambda (dir)
							    (cons 'i (concat dir "/ebin")))
							  (directory-files dep-path t "[^.]$")))))
					    (directory-files "../.." t "[^.]$")))))))

(add-hook 'markdown-mode-hook (lambda ()
				;; Keep plain-text version readable
				;; using conventional 80 column tty:
				(visual-line-mode 0)
				;; Enforce hard wrap so plain text is
				;; readable in standard 80x24 terminal:
				(auto-fill-mode)
				;; Allow collapsing subtrees for easy nav:
				;;(outline-minor-mode)
				;; undo with M-x show-subtree or show-all:
				;;(hide-sublevels 1)
				))

(custom-set-faces
 ;; use: M-x list-colors-display
 '(markdown-header-face-1 ((t (:inherit markdown-header-face
					;;:height 1.5
					:underline t
					:foreground "brown"))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face
					;;:height 1.3
					:underline t
					:foreground "orange"))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face
					:foreground "salmon")))))

;; (add-to-list 'elixir-mode-hook
;; 	     #'(lambda ()
;; 	       (smartparens-mode)
;; 	       ;; https://github.com/elixir-lang/emacs-elixir
;; 	       ;; "Also, if you use smartparens you can piggyback on some of its
;; 	       ;; functionality for dealing with Ruby's do .. end blocks. A sample
;; 	       ;; configuration would be:"
;; 	       (sp-with-modes '(elixir-mode)
;; 		(sp-local-pair "fn" "end"
;; 		 :when '(("SPC" "RET"))
;; 		 :actions '(insert navigate))
;; 		(sp-local-pair "do" "end"
;; 		 :when '(("SPC" "RET"))
;; 		 :post-handlers '(sp-ruby-def-post-handler)
;; 		 :actions '(insert navigate))
;; 		(sp-local-pair "case" "end"
;; 		 :when '(("SPC" "RET"))
;; 		 :post-handlers '(sp-ruby-def-post-handler)
;; 		 :actions '(insert navigate)))))

;; (add-to-list 'elixir-mode-hook
;; 	     (defun auto-activate-ruby-end-mode-for-elixir-mode ()
;; 	       (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
;; 		    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
;; 	       (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
;; 	       ;;FIXME: requiring 'ruby-mode doesn't resolve warning: Can't Find...
;; 	       ;;(ruby-end-mode +1)
;; 	       ))

(add-hook 'rust-mode-hook #'(lambda ()
			      (setq company-tooltip-align-annotations t)
			      (cargo-minor-mode)
			      ;(racer-mode)
			      (eldoc-mode)
			      (electric-pair-local-mode)))

(add-hook 'cargo-process-mode-hook #'(lambda ()
				       (visual-line-mode)))
