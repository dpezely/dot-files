;;;; settings.el

(setq-default indent-tabs-mode nil)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      visible-bell t
      ispell-dictionary "en_CA"
      scroll-step 2
      display-time-day-and-date t
      battery-mode-line-format "[%b%p%%]"
      sentence-end-double-space t
      vc-handled-backends '()
      ;;truncate-partial-width-windows t
      ;;truncate-lines t
      pop-up-windows nil
      pop-up-frames nil
      same-window-buffer-names '("*compilation*" "*shell*"
                                 "*slime-scratch*" "*xref*")
      same-window-regexps '("^\\*sldb sbcl/" "^magit:")
      version-control nil
      delete-old-versions t)

;; (setq Man-switches (concat "-M " 
;; 			   (let ((existing (getenv "MANPATH")))
;; 			     (if existing
;; 				 (concat existing ":")))
;; 			   "/usr/man:/usr/local/share/man:/usr/local/git/man"))


;; Used for Shell and SSH: (or manually use M-x send-invisible)
(add-hook 'comint-mode-hook 
	  #'(lambda () 
	      (line-number-mode 1)
	      (setq comint-password-prompt-regexp
                    "[Pp]ass\\(word\\|[ ]*[Pp]hrase\\).*[:]")))

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
      common-lisp-hyperspec-symbol-table (concat hyperspec-path
						 "Data/Map_Sym.txt")

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

;;(add-to-list 'slime-contribs 'slime-cl-indent)
(add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode))

(add-hook 'lisp-mode-hook
          #'(lambda () 
              (setq-default indent-tabs-mode nil)
              (setq lisp-indent-function 'common-lisp-indent-function
                    ;;common-lisp-style "sbcl"
                    font-lock-maximum-decoration t)
              (paren-toggle-matching-quoted-paren 1)
              (show-paren-mode 'disable)))

;;; For remote slime sessions: [watch "lisp movies"]

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
;(require 'swank)
;;or (asdf:operate 'asdf:load-op :swank)
;;(setf swank:*use-dedicated-output-stream* nil)
;;(swank:create-server :port 4005 :dont-close t)


(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; https://github.com/jwiegley/use-package

;; https://github.com/remvee/android-mode.git
(use-package android-mode
    :disabled
  :custom
  ;; Ubuntu-specific, or just setenv ANDROID_HOME
  (android-mode-sdk-dir "/home/android-sdk"))

;;(add-to-list 'auto-mode-alist '("\\.arc\\'" . arc-mode))

(add-hook 'c-mode-common-hook
	  #'(lambda ()
	      ;;(hide-ifdef-mode)
	      ;;(c-set-style "linux")
	      ;;(c-set-style "bsd")
	      (set-fill-column 79)
	      (auto-fill-mode 'disable)))

(use-package calendar
  :init
  (setq calendar-today-visible-hook 'calendar-star-date
        calendar-view-diary-initially-flag t
        diary-display-function 'diary-fancy-display
        diary-list-entries-hook 'diary-include-other-diary-files
        diary-number-of-entries 5
        diary-file (expand-file-name "~/etc/SCHEDULE"))
  (appt-activate 1)            ; notify of appointments during the day
  :config
  (calendar-set-date-style 'european)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t)
  ;; Hack to fix local holidays in Emacs 25.2 and 26.3, possibly earlier;
  ;; Force append without modifying pointers to original head of list.
  ;; FIXME: remove when duplicates appear in *Fancy Diary Entries* buffer:
  (setf (nthcdr (length calendar-holidays) calendar-holidays) holiday-other-holidays)
  :custom
  (holiday-other-holidays
   ;; New Year's Day, Good Friday and Christmas are statutory
   ;; holidays in BC and already appear in Emacs default calendar
   '((holiday-float 2 1 3 "Family Day - statutory") ; since 2013 in BC
     (holiday-float 5 1 1 "Victoria Day" (- 24 6)) ; on or BEFORE 24th
     (holiday-fixed 7 1 "Canada Day")
     (holiday-float 8 1 1 "BC Day - statutory")
     (holiday-float 9 1 1 "Labour Day")
     (holiday-float 10 1 2 "Thanksgiving in Canada")
     (holiday-fixed 11 11 "Rememberance Day")
     (next-major-lunar-phase))))

(use-package cargo
  ;; Hook for rust language to access build manager
  :after rust-mode
  :hook ((rust-mode . cargo-minor-mode)
         (cargo-process-mode . visual-line-mode))
  :config
  ;; There's only a "beta" toml-mode since 2016 and as of end of 2019:
  ;; https://github.com/dryman/toml-mode.el
  (add-to-list 'auto-mode-alist '("Cargo.toml\\'" . conf-mode)))

(use-package company
    ;; for TAB key word completion, used by racer
    :hook ((racer-mode . company-mode)
           (rust-mode . company-mode)))

(use-package cython-mode
    ;; Cython programming language
    :after flycheck
    :hook (cython-mode . flycheck-mode))

(use-package elixir-mode
    :disabled
  ;; This was for a very early version of Elixir for treating various
  ;; code blocks that end with the keyword END to be handled similarly
  ;; as Lisp code blocks.  There are probably better solutions from
  ;; others now...
  :init
  (use-package ruby-mode)
  (use-package ruby-end-mode)
  (use-package smartparens-mode)
  :after (ruby-mode ruby-end-mode smartparens-mode)
  :config
  (smartparens-mode)
  ;; https://github.com/elixir-lang/emacs-elixir
  ;; "Also, if you use smartparens you can piggyback on some of its
  ;; functionality for dealing with Ruby's do .. end blocks. A sample
  ;; configuration would be:"
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "fn" "end"
                   :when '(("SPC" "RET"))
                   :actions '(insert navigate))
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(sp-ruby-def-post-handler)
                   :actions '(insert navigate))
    (sp-local-pair "case" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(sp-ruby-def-post-handler)
                   :actions '(insert navigate)))
  (defun auto-activate-ruby-end-mode-for-elixir-mode ()
    (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
         "\\(?:^\\|\\s-+\\)\\(?:do\\)")
    (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
    (ruby-end-mode)))

(use-package erlang-mode
    :disabled
  :config
  (add-to-list 'completion-ignored-extensions ".beam") ; Erlang VM
  (setq indent-tabs-mode nil
        ;; Add paths to ../../*/deps/ebin for running & compiling:
        ;; (Restart *erlang* shell when new dependancies are added)
        inferior-erlang-machine-options 'erlang-mapcan-paths
        erlang-compile-extra-opts       'erlang-mapcar-paths))

(use-package flycheck
    :disabled
  ;; Use eslint for ECMAScript 2015 (ES6) but flycheck for everything else
  ;; http://www.flycheck.org/en/manual/latest/index.html
  :custom
  (flycheck-disabled-checkers (append flycheck-disabled-checkers
                                      '(javascript-jshint json-jsonlist)))
  (flycheck-temp-prefix ".flycheck"))

(use-package gh-md
    :disabled
  ;;; Render markdown using the Github-flavoured markdown
  :after markdown-mode)

(use-package html-to-markdown
    :disabled
  ;;; HTML to Markdown converter written in Emacs-lisp
  :after markdown-mode)

(use-package html2org
    :disabled
  ;;; Convert html to org format text
  :after org)

;; FIXME: LSP for Android?
;; https://github.com/fwcd/KotlinLanguageServer
;; https://github.com/remvee/android-mode
;; https://github.com/fernando-jascovich/android-env.el
;; https://github.com/emacs-lsp/lsp-mode
;; This requires setting JAVA_HOME such as via helper script as
;; /usr/local/bin/kotlin-language-server or something in PATH.  Otherwise,
;; *lsp-log* complains `lsp-kotlin-language-server-path` not in path.
;; Beyond that, there are lots of broken references because it doesn't
;; resolve Android SDK jars, even with android-mode.
;; (use-package kotlin-mode
;;     :after lsp
;;     :hook
;;     (kotlin-mode . lsp-deferred)
;;     (kotlin-mode . android-mode))
(use-package kotlin-mode)

(use-package lsp-mode
  ;; Language Server Protocol https://github.com/emacs-lsp/lsp-mode
  ;; Beware that their changelog doesn't include all changes to variable
  ;; names, and not all changes seem to come with deprecation warnings.
  ;; See also eglot as a more lightweight LSP client.
  ;;:config             ; https://github.com/emacs-lsp/lsp-mode#settings
  ;;(with-eval-after-load 'lsp-mode (require 'lsp-ui-flycheck))
  :custom
  (lsp-enable-completion-at-point t)
  (lsp-eldoc-enable-hover t) ; non-NIL displays fn args or var doc-string
  (lsp-enable-indentation t)
  ;;(lsp-enable-snippet nil)
  (lsp-enable-xref t)
  (lsp-eldoc-enable-hover nil) ; non-NIL for gaudy, sluggish overlay of doc-string
  (lsp-enable-imenu t)
  (lsp-ui-sideline-enable t)  ; non-NIL overlays messages at right
  (lsp-ui-sideline-show-code-actions t) ; non-NIL for hint to fix code
  ;; non-NIL enables display of symbols information; NIL does
  ;; _not_ impact display of flycheck diagnostics or Code Actions:
  (lsp-ui-sideline-show-hover nil)
  :commands
  (lsp lsp-deferred))

(use-package eglot
    :disabled
    ;; Emacs-polyglot is a Language Server Protocol client.
    ;; https://github.com/joaotavora/eglot
    ;; M-x eglot-disconnect when background compilation consumes too
    ;; much CPU such as when revisiting old projects to borrow code.
    :hook (rust-mode . eglot-ensure))

(use-package lsp-ui
    :disabled
  ;; https://github.com/emacs-lsp/lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom-face                      ;see also: M-x list-colors-display
  (lsp-ui-sideline-current-symbol ((t :foreground "brown"
                                      :weight ultra-bold
                                      :box (:line-width -1 :color "brown")
                                      :height 0.99)))
  (lsp-ui-sideline-code-action ((t :foreground "orange"))))

(use-package magit                      ; Git
  :config
  (magit-auto-revert-mode 0))

(use-package markdown-mode
    ;;; https://jblevins.org/projects/markdown-mode/
    :pin "melpa-stable"
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode))
  :hook
  (markdown-mode . (lambda ()
                     (auto-fill-mode)
                     (electric-quote-local-mode 0)
                     ;; Effectively no-op when under :confg
                     (markdown-hide-sublevel 2)))
  :custom-face                      ;see also: M-x list-colors-display
  (markdown-header-face-1 ((t (:inherit markdown-header-face
                                        :underline t
                                        :foreground "brown"))))
  (markdown-header-face-2 ((t (:inherit markdown-header-face
                                        :underline t
                                        :foreground "orange"))))
  (markdown-header-face-3 ((t (:inherit markdown-header-face
                                        :foreground "salmon"))))
  :bind (("\C-c2" . markdown-hide-sublevel2)
         ("\C-c\C-a" . markdown-show-all)
         ("\C-c\C-h" . outline-hide-subtree)))

(use-package markdown-mode+
    :disabled
  :after markdown-mode)

(use-package mic-paren
  ;; http://www.emacswiki.org/cgi-bin/wiki/mic-paren.el
  :config
  (paren-activate))

(use-package org
  :config
  (setq org-emphasis-alist (delete-if (lambda (x) ;omit +strike-through+
                                        (equal (car x) "+"))
                                      org-emphasis-alist)
        ;; FIXME:
        ;; Use back-ticks (accent grave mark) for code, as with Markdown:
        org-emphasis-alist (cons '("`" org-code verbatim) org-emphasis-alist))
  :custom
  (org-export-author-info nil)
  (org-export-copy-to-kill-ring nil)
  (org-export-email-info nil)
  (org-export-headline-levels 2)
  (org-export-html-coding-system 'utf-8)
  ;; (org-export-html-style-extra
  ;;    "<style type=\"text/css\"><!--/*--><![CDATA[/*><!--*/
  ;; 	body {font-family:sans-serif}
  ;; 	a {text-decoration:none}
  ;; 	#table-of-contents {font-size:75%}
  ;; 	/*]]>*/--></style>")
  (org-export-time-stamp-file nil)
  ;;(org-export-with-toc nil) ;;Instead, use: #+OPTIONS: toc:nil
  (org-html-head (concat "<link rel=\"stylesheet\""
                         " type=\"text/css\" href=\"style.css\" />"))
  (org-html-postamble nil)
  (org-use-sub-superscripts nil))

(use-package org-present
    :disabled
  ;; Use org-mode for slide show presentation
  :after org-mode
  :hook ((org-present-mode . (lambda ()
                               (org-present-big)
                               (org-display-inline-images)))
         (org-present-mode-quit . (lambda ()
                                    (org-present-small)
                                    (org-remove-inline-images)))))

(use-package python
    ;; "The package is 'python, but the mode is 'python-mode"
    ;; https://github.com/palantir/python-language-server
    ;; pip3 install 'python-language-server[all]'
    :after lsp-mode
    :hook (python-mode . lsp-deferred))

(use-package racer
    ;; Provides M-. and M-, support for Rust language
    ;; Requires `racer` executable, which as of 2019-12-10 requires Rust Nightly:
    ;; cargo +nightly install racer
    ;; https://github.com/racer-rust/emacs-racer
    ;; https://github.com/racer-rust/racer
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

;; FIXME:
;; https://github.com/jsalzbergedu/etoile-emacs/blob/b72e67b27326e2ca48e46cd89e4c522de4dd612e/etoile-programming/etoile-programming.el#L679

;; (use-package rustic
;;   :demand t
;;   :after rust-mode
;;   :config
;;   (add-hook 'rustic-mode-hook 'prog-minor-modes-common)
;;   (add-hook 'rustic-mode-hook (lambda ()
;;                                 (add-to-list 'flycheck-checkers 'lsp-ui)))
;;   (add-hook 'rustic-mode-hook 'lsp)
;;   (sp-with-modes '(rustic-mode)
;;     (sp-local-pair "'" "'"
;;                    :unless '(sp-in-comment-p sp-in-string-quotes-p sp-in-rust-lifetime-context)
;;                    :post-handlers'(:rem sp-escape-quotes-after-insert))
;;     (sp-local-pair "<" ">"
;;                    :when '(sp-rust-filter-angle-brackets)
;;                    :skip-match 'sp-rust-skip-match-angle-bracket))
;;
;;   ;; Rust has no sexp suffices.  This fixes slurping
;;   ;; (|foo).bar -> (foo.bar)
;;   (add-to-list 'sp-sexp-suffix (list #'rustic-mode 'regexp "")))

(use-package rust-mode
  ;; Rust-stable includes `rls`.
  ;; https://github.com/rust-lang/rls
  ;; For LINT checks, run Rust's Clippy; invoke with: M-x cargo-process-clippy
  :after (company-mode lsp-mode racer-mode smartparens-mode)
  :hook ((rust-mode . lsp-deferred)
         (rust-mode . flycheck-mode)
         (rust-mode . smartparens-mode)
         (rust-mode . omit-version-control-mode-line))
  :config
  (sp-with-modes '(rust-mode)
    (sp-local-pair "<" ">"
                   :when '(sp-rust-filter-angle-brackets)
                   :skip-match 'sp-rust-skip-match-angle-bracket
                   :actions '(insert navigate)))
  (setq company-tooltip-align-annotations t
        ;;cargo-process--command-clippy "+nightly clippy"
        ;;(lsp-rust-rls-server-command '("rustup" "run" "stable" "rls"))
        c-syntactic-indentation t
        c-basic-offset 2)
  ;; FIXME: key-bindings here seem to be ignored
  :bind (("C-c C-c b" . cargo-process-build)
         ("C-c C-c n" . cargo-process-build-nightly)
         ("C-c C-c c" . cargo-process-clippy)
         ("C-c C-c r" . cargo-process-run)
         ("C-c C-c t" . cargo-process-test)
         ("C-M-;" . mark-rust-statement)))

(use-package rust-playground
  ;; Not quite a REPL but similar to *slime-scratch* as code sandbox
  ;; https://github.com/grafov/rust-playground
  ;; Start via: M-x rust-playground
  ;; when prompted for comment syntax, this is for .toml file, so use #
  ;; For sharing like gist: M-x rust-playpen-region or -buffer
  ;; Try: M-x rust-playground-download
  :after rust-mode
  :custom
  (rust-playground-basedir "/tmp/rust-playground"))

;; (add-hook 'scheme-mode-hook #'(lambda () 
;; 				(setq font-lock-maximum-decoration t)
;; 				(paren-toggle-matching-quoted-paren 1)
;; 				(show-paren-mode 'disable)))

(use-package smartparens
    ;; https://github.com/Fuco1/smartparens
    ;; Sample instructions for new package installer:
    ;; https://github.com/Fuco1/smartparens/wiki/Quick-tour
  :config
  (add-hook 'prog-minor-modes-common 'show-paren-mode)
  :init
  (add-hook 'prog-minor-modes-common 'smartparens-mode))

;; FIXME: sample config for smartparens
;; https://github.com/jsalzbergedu/etoile-emacs/blob/b72e67b27326e2ca48e46cd89e4c522de4dd612e/etoile-programming/etoile-programming.el#L135

(use-package smartparens-rust
    :disabled
  :after smartparens
  :demand t)

(add-hook 'text-mode-hook
	  #'(lambda () 
	      (set-fill-column 76) 
	      (visual-line-mode)))

;;(use-package yasnippet)                 ; for 'lsp-enable-snippet
