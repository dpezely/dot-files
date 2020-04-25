dot files
=========

In an earlier era of shared hosts, you could simply look at my home
directory.  Any file or subdirectory that was readable to the world or a
group in which we were both members was available for you to browse freely.

Without that commonality, this repo is the next best thing...

## Emacs

My Emacs configuration begins with `~/.emacs`, but newer
documentation since I started might put this in `~/.emacs.d/init.el`
instead.  Name of this file is less significant than the rest.

All my other elisp files are kept in [~/emacs/lisp/](emacs/lisp/), but if
you change this location, the corresponding path within
[~/.emacs](emacs/dot.emacs) must also be changed.

The second most significant file here is
[~/emacs/lisp/settings.el](emacs/lisp/settings.el) for working with Common
Lisp, Rust and other programming languages.

Adopted here in 2018, [use-package](https://github.com/jwiegley/use-package)
configures most editing modes.  Apparently, Emacs loads faster by using this
method, yet speed was never a concern.  It's used here for keeping
configuration tidy.  However, this is a work in progress.

File paths and environment variables live in OS-specific files such as
[~/emacs/lisp/ubuntu.el](emacs/lisp/ubuntu.el) for current LTS edition or
[~/emacs/lisp/macos.el](emacs/lisp/macos.el), updated for Mojave/10.14.

### Compiling Emacs Locally

Browse [official repo](http://git.savannah.gnu.org/cgit/emacs.git) for
relevant tag such as `emacs-26.3` because you probably don't want HEAD.

Install dependencies:

    sudo apt-get install build-essential
	sudo apt-get build-dep emacs

Or use individual packages from the `build-dep` if on a different distro:

	sudo apt-get install build-essential texinfo \
      libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev \
      libgtk2.0-dev libncurses5-dev gnutls-dev libgtk-3-dev

After either of those above, run:

    git clone -b emacs-26.3 git://git.savannah.gnu.org/emacs.git
    cd emacs/
	./autogen.sh && ./configure && make

    sudo make install

### Lisp family of programming languages

While developing in Common Lisp or other dialects such as Scheme, Racket,
Arc, etc., you should **never** type or remove a single literal paren!

Very quickly, you'll begin thinking in terms of Lisp forms or whole
expressions, and that's a beautiful place to be.

Here's how:

See `#'lisp-mode-hook` within
[~/emacs/lisp/settings.el](emacs/lisp/settings.el) and various `paren-*`
variables near top of that file within `setq`.

For [Common Lisp programming language](http://lisp-lang.org/), the
*Superior Lisp Interaction Mode for Emacs*
([slime](http://quickdocs.org/slime/)) gets loaded via
[Quicklisp library manager](https://www.quicklisp.org/).

However, Quicklisp setup is handled via [lisp/Makefile](lisp/Makefile)
outside of Emacs.  For that, run:

- First time: `cd lisp/ && make setup references`
- For periodic Quicklisp updates, use: `cd lisp/ && make update`

Then within Emacs, simply type:

- `Meta-X slime` to invoke a command-line REPL (do this first)
- `Meta-X slime-scratch` to invoke a special buffer called `*slime-scratch*`
  from which you can evaluate arbitrary values and expressions

(*Meta* key is `ALT` on BSD Unix, Linux and Windows or ⌘ `Command Key` on
macOS, unless remapped by your Emacs config or OS setup.)

When viewing Lisp source code, use these key sequences:

- Ctrl-Meta-SPACE -- *mark* next *form*
- Ctrl-Meta `(` -- insert a matching pair of parens around marked form
- Ctrl-W -- cut marked form
- Ctrl-Y -- paste previous cut
- Meta-Y -- after Ctrl-Y -- cycle through *kill-ring* of all previous cuts
- Ctrl-X Ctrl-E -- evaluate preceding form
- Ctrl-Meta-T -- transpose s-expressions; i.e., swap X, Y
- Ctrl-C Ctrl-D h -- lookup symbol under cursor within language docs (HyperSpec)
- Ctrl-C % -- raise a form by removing its immediate parent scope

where *form* may be a value or parenthesized expression; *cut* really means
remove from current buffer and save in practically infinite "kill ring" from
which you can then yank (`Ctrl-Y`) and cycle through the kill ring
(`Meta-Y`).

The key sequence that will *raise a form by removing its immediate parent scope*
gives you much power by maintaining structure while editing.

e.g., place cursor over opening paren of an expression within `PROGN`, use
the key sequence `C-c %`, and everything within that `progn` including the
`progn`'s parens go away except for the one with which you started the key
sequence.  This leaves only the form (or value) of where the cursor was when
you invoked that key sequence.

That's very useful for thinking in terms of *expressions*, rather than code
or syntax!

For language documentation, I prefer maintaining local copies on my laptop
for being able to work anywhere without a network connection most of the
time.  (See `references` target within [lisp/Makefile](lisp/Makefile).)

### Rust programming language

For the [Rust programming language](https://rust-lang.org/), the 
[Language Server Protocol](https://github.com/emacs-lsp/lsp-mode) overlays
on-the-fly hints from the compiler into Emacs.  This indicates function
arguments or doc-string messages, where applicable-- as you type.

There is a companion Cargo mode for Rust, such that while viewing your Rust
source file, simply type:

- Ctrl-C Ctrl-C b -- equivalent to `cargo build`
- Ctrl-C Ctrl-C t -- run tests
- Ctrl-C Ctrl-C c -- equivalent to `cargo clippy`
- Meta-X rust-playground -- similar to Lisp's `*slime-scratch*` buffer

Since typing `Meta-X rust-playground` is a bit long, simply type enough of
each word to be unique; e.g., type `ru-playg` then tap TAB or SPACE key to
complete.

### Other languages

Some support via `use-package` for these programming languages and editor
modes are accommodated, but ignore any marked `:disabled`.

## Further Reading

While learning Common Lisp many years ago, books and other resources used
are listed on [play.org/links/lisp-intro](https://play.org/links/lisp-intro)
and [play.org/links/lisp](https://play.org/links/lisp).  Today, however, you
can start with [Lisp-Lang.org](http://lisp-lang.org/).

Similarly for Rust programming language: [play.org/links/rust](https://play.org/links/rust)

Beyond [electric-pair-mode](http://www.emacswiki.org/emacs/ElectricPair) and
[smartparens](https://github.com/Fuco1/smartparens) used here, other Emacs
packages for structured manipulation of parenthesis include:

- [AutoPairs](https://www.emacswiki.org/emacs/AutoPairs)
- [Par Edit](http://emacswiki.org/emacs/ParEdit)
