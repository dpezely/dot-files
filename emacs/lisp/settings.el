;;;; settings.el

(setq-default indent-tabs-mode nil
              indicate-buffer-boundaries '((bottom . right))
              show-trailing-whitespace t
              indicate-empty-lines t)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      visible-bell t
      ispell-dictionary "en_US"
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
      same-window-regexps '("^\\*sldb sbcl/" "^\\*Man " "^magit:")
      ;; Keep flycheck, delight, etc. from expanding minibuffer beyond this threshold:
      ;;max-mini-window-height 3 ;https://www.emacswiki.org/emacs/EchoArea
      buffer-quit-function 'ignore
      version-control nil
      delete-old-versions t)

;; (setq Man-switches (concat "-M "
;; 			   (let ((existing (getenv "MANPATH")))
;; 			     (if existing
;; 				 (concat existing ":")))
;; 			   "/usr/man:/usr/local/share/man:/usr/local/git/man"))


;; Used for Shell and SSH:
(add-hook 'comint-mode-hook
	  #'(lambda ()
	      (line-number-mode 1)
	      (setq comint-password-prompt-regexp ;or use M-x send-invisible
                    "[Pp]ass\\(word\\|[ ]*[Pp]hrase\\).*[:]"
                    indicate-buffer-boundaries nil
                    show-trailing-whitespace nil)))

;; set up unicode
(set-language-environment "UTF-8")	;see also 'slime-net-coding-system var
(unless (getenv "LANG")
  (setenv "LANG"     "en_US.UTF-8"))	;for external programs; e.g., SBCL
(unless (getenv "LC_CTYPE")
  (setenv "LC_CTYPE" "en_US.UTF-8"))

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
  :custom
  (calendar-today-visible-hook 'calendar-star-date)
  (calendar-view-diary-initially-flag t)
  (diary-display-function 'diary-fancy-display)
  (diary-list-entries-hook 'diary-include-other-diary-files)
  (diary-number-of-entries 5)
  (diary-file (expand-file-name "~/etc/SCHEDULE"))
  (appt-activate 1)            ; notify of appointments during the day
  (holiday-other-holidays '((next-major-lunar-phase)))
  :config
  (calendar-set-date-style 'european)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t)
  (add-hook 'calendar-mode-hook (lambda ()
                                  (setq indicate-buffer-boundaries nil
                                        show-trailing-whitespace nil
                                        indicate-empty-lines nil))))

;; for TAB key word completion used by clients of Language Server Protocol
(use-package company)

(use-package delight
  ;; utility to disable showing flycheck in mode line; use minibuf instead
  :ensure t)

(use-package eglot
  ;; Emacs-polyglot is a Language Server Protocol client.
  ;; https://github.com/joaotavora/eglot
  ;; M-x eglot-disconnect when background compilation consumes too
  ;; much CPU such as when revisiting old projects to borrow code.
  :ensure t
  :hook ((rust-mode . eglot-ensure))
  :config
  ;; FIXME: this belogs under `use-package rust-mode'
  (add-to-list 'eglot-server-programs
               ;; FIXME add :initializationOptions with JSON
               '(rust-mode . ("rustup" "run" "stable" "rust-analyzer"))))

(use-package lsp-mode
    :disabled
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
  :hook ((lsp-mode . (lambda ()
                       (lsp-headerline-breadcrumb-mode -1)
                       (vc-mode-line nil))))
  :init
  (lsp-headerline-breadcrumb-mode -1)
  (vc-mode-line nil))

(use-package lsp-ui
    :disabled
  ;; https://github.com/emacs-lsp/lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :custom-face                      ;see also: M-x list-colors-display
  (lsp-ui-sideline-current-symbol ((t :foreground "brown"
                                      :weight ultra-bold
                                      :box (:line-width -1 :color "brown")
                                      :height 0.99)))
  (lsp-ui-sideline-code-action ((t :foreground "orange"))))

(use-package magit                      ; Git
  ;; :pin melpa-stable
  :config
  (setq disable-magit-save-buffers nil)
  (magit-auto-revert-mode 0))

(use-package markdown-mode
    ;;; https://jblevins.org/projects/markdown-mode/
    :pin "melpa-stable"
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode))
  :hook ((markdown-mode . (lambda ()
                            (auto-fill-mode)
                            (electric-quote-local-mode 0)
                            ;; Effectively no-op when under :confg
                            (outline-hide-sublevels 2))))
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
         ("\C-c\C-a" . outline-show-all)
         ("\C-c\C-h" . outline-hide-subtree)))

(use-package markdown-mode+
    :disabled
  :after markdown-mode)

(use-package mic-paren
  ;; http://www.emacswiki.org/cgi-bin/wiki/mic-paren.el
  :config
  (paren-activate))

(use-package org
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
  (org-use-sub-superscripts nil)
  :bind (("C-c C-q" . outline-hide-sublevels))
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (outline-hide-sublevels 1))))

(use-package org-present
    :disabled
  ;; Use org-mode for slide show presentation
  ;; :after org-mode
  :hook ((org-present-mode . (lambda ()
                               (org-present-big)
                               (org-display-inline-images)))
         (org-present-mode-quit . (lambda ()
                                    (org-present-small)
                                    (org-remove-inline-images)))))

(use-package plantuml-mode)

(use-package python
    ;; "The package is 'python, but the mode is 'python-mode"
    ;; https://github.com/palantir/python-language-server
    ;; pip3 install 'python-language-server[all]'
    ;; :after lsp-mode
    :hook ((python-mode . lsp-deferred)))

(use-package rust-mode
  ;; Rust-stable includes `rust-analyzer` which supersedes `rls`.
  ;; https://rust-analyzer.github.io/
  ;; For LINT checks, run Rust's Clippy; invoke with: M-x cargo-process-clippy
  ;; Unresolved procedural macros? see:
  ;; https://www.reddit.com/r/emacs/comments/11hn42e/configuring_eglot_for_rustanalyzer_and_suppress/
  ;;:after (company-mode eglot-mode smartparens-mode)
  :after (eglot-mode smartparens-mode)
  ;;:hook ((rust-mode . eglot-ensure)
  ;;        (rust-mode . eldoc-mode)
  ;;        (rust-mode . smartparens-mode))
  ;; FIXME: see `use-package eglot'
  ;; :init
  ;; (add-to-list 'eglot-server-programs
  ;;              ;; FIXME add :initializationOptions with JSON
  ;;              '(rust-mode . ("rustup" "run" "stable" "rust-analyzer")))
  :config
  ;; (sp-with-modes '(rust-mode)
  ;;   (sp-local-pair "<" ">"
  ;;                  :when '(sp-rust-filter-angle-brackets)
  ;;                  :skip-match 'sp-rust-skip-match-angle-bracket
  ;;                  :actions '(insert navigate)))
  ;; (vc-mode-line nil)
  (setq company-tooltip-align-annotations t
        cargo-process--command-clippy "+nightly clippy -Zunstable-options"
        lsp-rust-rls-server-command '("rustup" "run" "stable" "rust-analyzer"))
  ;; ;; FIXME: key-bindings here seem to be ignored
  ;; :bind (("C-c C-c b" . cargo-process-build)
  ;;        ("C-c C-c n" . cargo-process-build-nightly)
  ;;        ("C-c C-c c" . cargo-process-clippy)
  ;;        ("C-c C-c r" . cargo-process-run)
  ;;        ("C-c C-c t" . cargo-process-test)
  ;;        ("C-M-;" . mark-rust-statement))
  )

(use-package rust-playground
  ;; Not quite a REPL but similar to *slime-scratch* as code sandbox
  ;; https://github.com/grafov/rust-playground
  ;; Start via: M-x rust-playground
  ;; when prompted for comment syntax, this is for .toml file, so use #
  ;; For sharing like gist: M-x rust-playpen-region or -buffer
  ;; Try: M-x rust-playground-download

  ;; :after rust-mode
  :custom
  (rust-playground-basedir "/tmp/rust-playground"))

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
