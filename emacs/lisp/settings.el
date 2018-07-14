;;;; settings.el

(setq inhibit-startup-screen t
      initial-scratch-message nil
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
      same-window-buffer-names '("*shell*" "*slime-scratch*" "*xref*")
      same-window-regexps '("^\*sldb sbcl/")
      version-control nil
      delete-old-versions t)

(setq-default indent-tabs-mode nil)

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

;;(add-to-list 'slime-contribs 'slime-cl-indent)
(add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode))

(add-hook 'lisp-mode-hook
          #'(lambda () 
              (setq-default indent-tabs-mode nil)
              (setq lisp-indent-function 'common-lisp-indent-function
                    ;;common-lisp-style "sbcl"
                    font-lock-maximum-decoration t)
              (paren-toggle-matching-quoted-paren 1)
              (show-paren-mode t)))

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


;;; https://github.com/jwiegley/use-package

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;(add-to-list 'auto-mode-alist '("\\.arc\\'" . arc-mode))

(add-hook 'c-mode-common-hook
	  #'(lambda ()
	      ;;(hide-ifdef-mode)
	      ;;(setq tab-width 4)
	      ;;(c-set-style "linux")
	      ;;(c-set-style "bsd")
	      (set-fill-column 79)
	      (auto-fill-mode 1)))

(use-package cargo
  ;; Hook for rust language to access build manager
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'cargo-process-mode-hook #'visual-line-mode))
;; Ironically, there is no file mode for rust's Cargo.toml files
(add-to-list 'auto-mode-alist '("Cargo.toml\\'" . conf-mode))

(use-package company
  ;; for TAB key word completion, used by racer
  :config
  (add-hook 'racer-mode-hook #'company-mode))

(use-package company-lsp
  ;; facilitates completion-at-point (C-M-i)
  :after company
  :config
  (setq company-lsp-enable-recompletion t)
  (lsp-define-stdio-client lsp-rust "rust" #'lsp-rust--get-root nil
                           :command-fn #'lsp-rust--rls-command
                           :initialize #'lsp-rust--initialize-client)
  (push 'company-lsp company-backends))

(use-package eglot
    :disabled
  ;; Requires Emacs-26.  Use this OR lsp-mode, but not both.
  ;; Emacs polyglot, a lightweight LSP https://github.com/joaotavora/eglot
  ;; See also lsp-mode.
  ;; (Works with project.el)
  ;; M-x eglot-shutdown says bye-bye to the server
  :bind
  (define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions))

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
    (ruby-end-mode +1)))

(use-package erlang-mode
    :disabled
  :config
  (add-to-list 'completion-ignored-extensions ".beam") ; Erlang VM
  (setq indent-tabs-mode nil
        ;; Add paths to ../../*/deps/ebin for running & compiling:
        ;; (Restart *erlang* shell when new dependancies are added)
        inferior-erlang-machine-options 'erlang-mapcan-paths
        erlang-compile-extra-opts       'erlang-mapcar-paths))

(use-package gh-md
    :disabled
  ;;; Render markdown using the Github-flavoured markdown
  :after markdown-mode)

;; (add-hook 'go-mode-hook
;; 	  #'(lambda ()
;; 	      ;;(setq tab-width 4)
;; 	      (add-hook 'before-save-hook #'gofmt-before-save)))

;; Use eslint instead for ECMAScript 2015 but flycheck for everything else
;; http://www.flycheck.org/en/manual/latest/index.html
;;(setq flycheck-disabled-checkers '(javascript-jshint json-jsonlist)
;;      flycheck-temp-prefix ".flycheck")
;; use eslint with web-mode for jsx files
;;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;;(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (add-hook 'javascript-mode-hook
;; 	  #'(lambda ()
;; 	      ;;(setq tab-width 4)
;; 	      (set-fill-column 79)
;; 	      (auto-fill-mode 1)))
;; (add-hook 'web-mode-hook
;; 	  #'(lambda ()
;; 	      ;;(flycheck-mode)
;; 	      ;; See web-mode.org
;; 	      (setq web-mode-markup-indent-offset 2
;; 		    web-mode-css-indent-offset 2
;; 		    web-mode-code-indent-offset 2)
;; 	      (defadvice web-mode-highlight-part (around tweak-jsx activate)
;; 		(if (equal web-mode-content-type "jsx")
;; 		    (let ((web-mode-enable-part-face nil))
;; 		      ad-do-it)
;; 		    ad-do-it))))

(use-package html-to-markdown
    :disabled
  ;;; HTML to Markdown converter written in Emacs-lisp
  :after markdown-mode)

(use-package html2org
    :disabled
  ;;; Convert html to org format text
  :after org)

(use-package lsp-mode
  ;; Language Server Protocol https://github.com/emacs-lsp/lsp-mode
  ;; See also eglot, an alternative and lighter LSP implementation
  :config
  ;;(with-eval-after-load 'lsp-mode (require 'lsp-ui-flycheck))
  (setq lsp-enable-codeaction t
        lsp-enable-completion-at-point t
        lsp-enable-eldoc t ; non-NIL to display fn args or var doc-string
        lsp-enable-flycheck t ; non-NIL give overlay of doc-string
        lsp-enable-indentation t
        lsp-enable-xref t
        lsp-ui-doc-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t
        lsp-ui-peek-enable t
        lsp-ui-sideline-enable t  ; non-NIL overlays messages at right
        lsp-ui-sideline-show-code-actions t ; non-NIL for hint to fix code
        ;; non-NIL enables display of symbols information; NIL does
        ;; _not_ impact display of flycheck diagnostics or Code Actions:
        lsp-ui-sideline-show-hover nil))

;;FIXME: enable highlight of fn param like SLIME does
(use-package lsp-rust
  ;; Language Server Protocol for Rust language
  ;; https://github.com/emacs-lsp/lsp-rust
  ;; See also: https://github.com/rust-lang-nursery/rls
  ;; and set 'lsp-rust-rls-command or environment variable RLS_ROOT
  :after lsp-mode
  :config
  (add-hook 'rust-mode-hook #'lsp-rust-enable)
  (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls")))

(use-package lsp-ui
  ;; https://github.com/emacs-lsp/lsp-ui
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  :custom-face                      ;see also: M-x list-colors-display
  (lsp-ui-sideline-current-symbol ((t :foreground "brown"
                                      :weight ultra-bold
                                      :box (:line-width -1 :color "brown")
                                      :height 0.99)))
  (lsp-ui-sideline-code-action ((t :foreground "orange"))))

(use-package markdown-mode
    ;;; https://jblevins.org/projects/markdown-mode/
    :pin "melpa-stable"
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode))
  :config
  ;; Allow collapsing subtrees for easy nav:
  ;;(outline-minor-mode)
  ;; undo with M-x show-subtree or show-all:
  ;;(hide-sublevels 1)
  (auto-fill-mode)
  :custom-face                      ;see also: M-x list-colors-display
  (markdown-header-face-1 ((t (:inherit markdown-header-face
                                        :underline t
                                        :foreground "brown"))))
  (markdown-header-face-2 ((t (:inherit markdown-header-face
                                        :underline t
                                        :foreground "orange"))))
  (markdown-header-face-3 ((t (:inherit markdown-header-face
                                        :foreground "salmon")))))

(use-package markdown-mode+
    :disabled
  :after markdown-mode)

(use-package mic-paren
  ;; http://www.emacswiki.org/cgi-bin/wiki/mic-paren.el
  :config
  (paren-activate))

(use-package org
  :config
  (setq org-export-author-info nil
        ;;org-export-with-toc nil ;;Instead, use: #+OPTIONS: toc:nil 
        org-export-email-info nil
        org-export-time-stamp-file nil
        org-export-headline-levels 2
        org-use-sub-superscripts nil
        org-emphasis-alist (delete-if (lambda (x)
                                        (equal (car x) "+"))
                                      org-emphasis-alist)
        org-export-html-coding-system 'utf-8
        org-html-head (concat "<link rel=\"stylesheet\""
                              " type=\"text/css\" href=\"style.css\" />")
        ;; org-export-html-style-extra
        ;;    "<style type=\"text/css\"><!--/*--><![CDATA[/*><!--*/
        ;; 	body {font-family:sans-serif}
        ;; 	a {text-decoration:none}
        ;; 	#table-of-contents {font-size:75%}
        ;; 	/*]]>*/--></style>"
        org-html-postamble nil
        org-export-copy-to-kill-ring nil))

(use-package org-present
    :disabled
  ;; Use org-mode for slide show presentation
  :after org-mode
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images))))

(use-package racer
  ;; Provides M-. for Rust language
  :after 'rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package rust-mode
  ;; For LINT checking, consider installing Rust's Clippy.
  ;; Run: rustup install nightly && cargo +nightly install clippy
  ;; then invoke with: M-x cargo-process-clippy
  :after smartparens
  :config
  ;; Requires the Language Server Protocol (LSP) be enabled,
  ;; but sometimes in emacs-25.2 fails with
  ;; "Suspicious state from syntax checker rust-cargo..."
  ;; When viewing rust code, use: M-x flycheck-buffer
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'smartparens-mode)
  (add-hook 'rust-mode-hook #'(lambda () (vc-mode-line nil)))
  ;; FIXME: insert matching pairs of < and >
  ;; (sp-with-modes '(rust-mode)
  ;;   (sp-local-pair "<" ">" :actions '(insert navigate)))
  (setq cargo-process--command-clippy "+nightly clippy"
        company-tooltip-align-annotations t
        c-syntactic-indentation t
        c-basic-offset 2))

(use-package rust-playground
  ;; Not quite a REPL, similar to *slime-scratch* as code sandbox
  ;; https://github.com/grafov/rust-playground
  ;; Start via: M-x rust-playground
  ;; when prompted for comment syntax, this is for .toml file, so use #
  ;; For sharing like gist: M-x rust-playpen-region or -buffer
  ;; Try: M-x rust-playground-download
  :after rust-mode
  :config
  (setq rust-playground-basedir "/tmp/rust-playground"))

;; (add-hook 'scheme-mode-hook #'(lambda () 
;; 				(setq font-lock-maximum-decoration t)
;; 				(paren-toggle-matching-quoted-paren 1)
;; 				(show-paren-mode t)))

;; Sample instructions for new package installer:
;; https://github.com/Fuco1/smartparens/wiki/Quick-tour
(use-package smartparens)

(add-hook 'text-mode-hook
	  #'(lambda () 
	      (set-fill-column 76) 
              (electric-quote-local-mode)
	      (visual-line-mode nil)))
