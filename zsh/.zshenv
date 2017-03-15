# Environment customizations

# Customize our path.
typeset -U path
path=(${HOME}/.local/bin "$path[@]")
path+=(/usr/local/cuda/bin)

# Make sure language is set properly.
export LANG=en_US.UTF-8

# Set up our editor.
export EDITOR=vim
export ALTERNATE_EDITOR=""
# export GIT_EDITOR='emacsclient -t'
# export VISUAL=emacs

# Set up for Python and virtual environments.
export WORKON_HOME=${HOME}/.virtualenvs
export PROJECT_HOME=${HOME}/Development
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3.5m

# Useful functions
function hexview () { hexdump -C $1 | less; }

function pidinfo {
    ps -eo pid,ppid,euser,comm,etime,cputime,%cpu,%mem,rss | grep -i "$1" | grep -v grep;
}
