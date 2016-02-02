## .bashrc

export LANG=en_CA.UTF-8
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


alias ls='ls -FC'


case $OSTYPE in
darwin*)
	#prevent 'tar' from creating or using those annoying MacOSX metadata files: e.g., ._foo
	export COPYFILE_DISABLE=1
	export COPY_EXTENDED_ATTRIBUTES_DISABLE=1
	PATH=${PATH}:/sw/bin:/Local/bin
	;;
*bsd)
	;;
linux*)
	;;
esac

if [ -z "$EMACS" ]; then
	export EDITOR=vi
else
	export EDITOR=emacsclient
fi

alias ssh_nohosts='ssh -o UserKnownHostsFile=/dev/null'
