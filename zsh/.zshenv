# Environment customizations

# Customize our path.
typeset -U path
path=(${HOME}/.local/bin "$path[@]")
path+=(/usr/local/cuda/bin)

# Make sure language is set properly.
export LANG=en_US.UTF-8

# Set up our editor.
export EDITOR=emacs
export GIT_EDITOR='emacsclient -t'
export VISUAL=emacs

# Set up for Python and virtual environments.
export WORKON_HOME=${HOME}/.virtualenvs
export PROJECT_HOME=${HOME}/Development
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3.5m

# Useful functions
function hexview () { hexdump -C $1 | less; }

function pidinfo {
    ps -eo pid,ppid,euser,comm,etime,cputime,%cpu,%mem,rss | grep -i "$1" | grep -v grep;
}

function man() {
    env LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        man "$@"
}
