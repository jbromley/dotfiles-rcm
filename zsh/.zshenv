# Environment customizations

# Customize our path. 
typeset -U path
# Local bin directory goes first.
path=(${HOME}/.local/bin "$path[@]")
# Add CUDA to the end of the path.
path+=(/usr/local/cuda/bin)
    
# Make sure language is set properly.
export LANG=en_US.UTF-8

# Set up our editor.
export EDITOR=emacs
export ALTERNATE_EDITOR=""

# Set up for Python and virtual environments.
export WORKON_HOME=${HOME}/.virtualenvs
export PROJECT_HOME=${HOME}/Code
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3.5m

# Choose a light theme.
export SOLARIZED_THEME=dark

# Todo.txt
alias t='todo.sh'
export TODOTXT_DEFAULT_ACTION=list

# Useful functions
function hexview () { hexdump -C $1 | less; }

function pidinfo {
    ps -eo pid,ppid,euser,comm,etime,cputime,%cpu,%mem,rss | grep -i "$1" | grep -v grep;
}

function dockerid {
    docker ps | grep "$1" | gawk -F' ' '{print $1;}';
}

function clrd {
    while [[ "$?" == 0 ]]; do
	popd > /dev/null 2>&1
    done
    cd
}

function o {
    xdg-open "$*" > /dev/null 2>&1
}
