dot files
=========

In an earlier era of shared hosts, you could simply look at my home
directory.  Any file or subdirectory that was readable to the world or a
group of which we were both members, was available for you to browse freely.

Without that commonality, this repo is the next best thing...

## Emacs

My Emacs configuration begins with `~/[.emacs](./.emacs)`, but newer
documentation since I started might put this in `~/.emacs.d/init.el`
instead.  Name of this file is less significant that the rest.

All my other elisp files are kept in `~/[emacs/lisp/](emacs/lisp/)`, but if
you change this location, the corresponding path within `~/.emacs` must also
be changed.

The second most significant file here is
`~/[emacs/lisp/settings.el](emacs/lisp/settings.el)` for working with Common
Lisp, Rust and other programming languages.

Adopted here in 2018, [use-package](https://github.com/jwiegley/use-package)
configures each mode.  Apparently, Emacs loads faster by using this method,
yet speed was never a concern.

### Lisp family of programming languages

While developing Common Lisp or any other dialect such as Scheme, Racket,
Arc, etc., you should **never** type or remove a single literal paren!

Very quickly, you'll begin thinking in terms of Lisp forms or whole
expressions, and that's a beautiful place to be.

Here's how:

For [Common Lisp programming language](https://lisp-lang.org/), the
*Superior Lisp Interaction Mode for Emacs*
([slime](http://quickdocs.org/slime/)) gets loaded via
[Quicklisp library manager](https://quicklisp.org/).

However, Quicklisp setup is handled via [lisp/Makefile](lisp/Makefile)
outside of Emacs.  For that, run:

- First time: `cd lisp/ && make setup`
- For periodic Quicklisp updates, use: `cd lisp/ && make update`

Then within Emacs, simply type:

- `Meta-X slime` to invoke a command-line REPL (do this first)
- `Meta-X slime-scratch` to invoke a special buffer called `*slime-scratch*`
  from which you can evaluate arbitrary values and expressions

(*Meta* key is `ALT` on Linux and Windows or ⌘ `Command Key` on macOS,
unless remapped by your Emacs config or OS setup.)

When viewing Lisp source code, use these key sequences:

- Ctrl-Meta-SPACE -- *mark* next *form*
- Ctrl-Meta `(` -- insert a matching pair of parens around marked form
- Ctrl-W -- cut marked form
- Ctrl-Y -- paste previous cut
- Meta-Y -- cycle through *kill-ring* of all previous cuts
- Ctrl-X Ctrl-E -- evaluate preceding form
- Ctrl-Meta-T -- transpose s-expressions; i.e., swap X, Y

where *form* may be a value or parenthesis-ed expression; *cut* really means
remove from current buffer and save in practically infinite "kill ring" from
which you can then yank (`Ctrl-Y`) and cycle through the kill ring
(`Meta-Y`).

### Rust programming language

For the [Rust programming language](https://rust-lang.org/), the 
[Language Server Protocol](https://github.com/emacs-lsp/lsp-mode) overlays
on-the-fly hints from the compiler into Emacs.  This indicates function
arguments or doc-string messages, where applicable-- as you type.

There is a companion Cargo mode for Rust, such that while viewing your Rust
source file, simply type:

- Ctrl-C Ctrl-C b -- equivalent to `cargo build`
- Ctrl-C Ctrl-C t -- run tests
- Ctrl-C Ctrl-C c -- equivalent to `cargo +nightly clippy`
- Meta-X rust-playground -- similar to Lisp's `*slime-scratch*` buffer

Assuming that you've installed Rust Clippy separately via
`rustup component add clippy-preview --toolchain=nightly` (current as of
rustc 1.28.0 in August 2018), the key sequence of `C-c C-c c` will work.

Since typing `Meta-X rust-playground` is a bit long, simply type enough of
each word to be unique; e.g., `ru-playg` then tap TAB or SPACE key to
complete.

### Other languages

Some support via `use-package` for these programming languages and editor
modes are accommodated:

- Contemporary JavaScript (ECMAScript 2015, ES6) as JSX
- Erlang (minimally used since converted from older config)
- Elixir (converted from older config, but not tested)
