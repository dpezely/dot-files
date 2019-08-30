## .bashrc

export LANG=en_CA.UTF-8
export LC_CTYPE=$LANG
export GDM_LANG=en_CA
export LANGUAGE=en_CA

umask 022

PS1='\u@\h \W> '

one_agent() {
    status=""
    if [ -r ~/.ssh/env ]; then
	eval `cat ~/.ssh/env`
	if [ -r $SSH_AUTH_SOCK ]; then
	    status="running"
	fi
    fi
    if [ "$status" != "running" ]; then
	ssh-agent > ~/.ssh/env
	eval `cat ~/.ssh/env`
	ssh-add
    fi
}

archive() {
    URL=https://web.archive.org/save/$1
    echo "$URL"
    wget --method=HEAD $URL
}

play() {
    for x in $*
    do
	mpc -q load $x
    done
    mpc -q random on
    mpc -q repeat on
    song_count=$(grep -c -v '^#' ~/Music/playlists/$1.m3u)
    mpc play $(( $(date +%s) % $song_count ))
}

alias ls='ls -FC'


case $OSTYPE in
    darwin*)
	# Prevent 'tar' from creating or using those annoying MacOSX metadata files:
	# e.g., ._foo
	export COPYFILE_DISABLE=1
	export COPY_EXTENDED_ATTRIBUTES_DISABLE=1
        # for HomeBrew to supersede macOS built-ins:
        PATH=/usr/local/bin:$PATH
        # for `aws` command from: pip3 install awscli
        PATH=~/Library/Python/3.7/bin:$PATH
        # Prevent homebrew from overruling Rust, because this will be be latest/official:
        PATH=~/.cargo/bin:$PATH
	;;
    *bsd)
	;;
    linux*)
	PATH="${PATH}:~/.cargo/bin"
	# https://doc.rust-lang.org/book/second-edition/ch09-01-unrecoverable-errors-with-panic.html
	;;
esac

if [ ! -z "$(which rustc)" ]; then
    export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
    export RUST_BACKTRACE=1
fi

if [ -z "$EMACS" ]; then
	export EDITOR=vi
else
	export EDITOR=emacsclient
fi

alias ssh_nohosts='ssh -o UserKnownHostsFile=/dev/null'

# RipGrep:
# reg-ex pattern follows, then subdirectory of where to begin search:
alias rg_cython='rg --files-with-matches --glob "*.{py,pxd,pyx,h,c}" -e'
