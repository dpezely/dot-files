
PS1='\u@\h \W> '

export EDITOR=emacsclient
export PATH=/opt/local/bin:/opt/local/sbin:/sw/bin:/Local/bin:/usr/local/bin:/usr/local/git/bin:$PATH

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

#alias clisp='clisp -q -ansi'

#export GIT_DIR=~/git

#prevent 'tar' from creating or using those annoying MacOSX metadata files: e.g., ._foo
export COPYFILE_DISABLE=1
export COPY_EXTENDED_ATTRIBUTES_DISABLE=1
