;; macOS specific settings for Emacs-26.3 using prebuilt executable
;; https://emacsformacosx.com/

(transient-mark-mode 1)		; highlight selected region?

(when window-system
  (tool-bar-mode 0)

  ;; Emacs-26.1 default font is Menlo 12pt
  ;; (set-default-font "-*-Menlo-normal-*-*-*-12-*-*-*-*-*-iso10646-1")
  ;; (set-default-font "-*-Menlo-normal-*-*-*-14-*-*-*-*-*-iso10646-1")
  (defun use-laptop-display (retina-display-p)
    "Use MacBook Pro Retina display vs an external monitor"
    (interactive "xUse built-in Laptop display? (Y)")
    (cond
      (retina-display-p
       (set-default-font "-*-Menlo-normal-*-*-*-14-*-*-*-*-*-iso10646-1")
       (defun tall ()
         (interactive)
         (set-frame-position (selected-frame) 250 20)
         (set-frame-height (selected-frame) 60)
         (set-frame-width (selected-frame) 110))
       (defun wide ()
         (interactive)
         (set-frame-position (selected-frame) 45 20)
         (set-frame-height (selected-frame) 59)
         (set-frame-width (selected-frame) 185)))
      (t
       (set-default-font "-*-Menlo-normal-*-*-*-14-*-*-*-*-*-iso10646-1")
       (defun tall ()
         (interactive)
         (set-frame-position (selected-frame) 250 20)
         (set-frame-height (selected-frame) 60)
         (set-frame-width (selected-frame) 110))
       (defun wide ()
         "For 1920x1080 scaled external display"
         (interactive)
         (set-frame-position (selected-frame) 120 20)
         (set-frame-height (selected-frame) 70)
         (set-frame-width (selected-frame) 200))))))

(setq ns-command-modifier 'meta	     ;use Command (not Option) as Meta
      ns-option-modifier 'none)

; reset this back to its normal setting:
(define-key esc-map " " 'just-one-space)
(global-unset-key "")	; was 'iconify-or-deiconify-frame (use Cmd-H on MacOSX)
(global-unset-key "") ; was 'iconify-or-deiconify-frame (use Cmd-H on MacOSX)

(setenv "PATH"
        (concat (getenv "PATH")
                ":/usr/local/bin"
                ":" (expand-file-name "~/") ".cargo/bin"
                ":" (expand-file-name "~/")
		".rustup/toolchains/stable-x86_64-apple-darwin/bin"))

(setenv "RUST_SRC_PATH"
	(concat (expand-file-name "~/")
		".rustup/toolchains/stable-x86_64-apple-darwin/"
		"lib/rustlib/src/rust/src"))

(setq ring-bell-function #'(lambda () nil) ; mute gaudy "<!>" icon on macOS
      python-shell-exec-path '("/usr/local/bin")
      python-shell-interpreter "python3"
      python-shell-completion-native-enable nil)

;; http://common-lisp.net/project/slime/doc/html/slime.html
(add-to-list 'load-path "/usr/local/lisp/slime")
(add-to-list 'load-path "/usr/local/lisp/slime/contrib")

(setq inferior-lisp-program "/usr/local/bin/sbcl"
      ;; These really belong beneath /usr/share/doc/, but it's easier to keep all Lisp stuff together:
      cltl2-url "file:///usr/local/lisp/cltl/clm/node1.html"
      hyperspec-path "/usr/local/lisp/HyperSpec/"
      hyperspec-prog "/usr/local/lisp/slime/hyperspec"
      ispell-program-name "/usr/local/bin/aspell"
      w3m-command "/usr/local/bin/w3m")

;(setenv "SBCL_HOME" "/usr/local/bin/sbcl")
