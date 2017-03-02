# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# Functions
function hexview () { hexdump -C $1 | less; }
function pidinfo {
    ps -eo pid,ppid,euser,comm,etime,cputime,%cpu,%mem,rss | grep -i "$1" | grep -v grep;
}

# Environment customizations
if [[ ! $PATH == *"/usr/local/cuda/bin"* ]]; then 
    export PATH=/usr/local/cuda/bin:$PATH
fi
if [[ ! $PATH == *".local/bin"* ]]; then 
    export PATH=${HOME}/.local/bin:$PATH
fi
