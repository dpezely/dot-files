if [ "$SSH_AGENT_PID" != "" -a -r $SSH_AUTH_SOCK ]; then
    count=`who | grep -v console | grep -c $USER`
    if [ $count -lt 2 ]; then
	eval `ssh-agent -k`
    fi
fi
