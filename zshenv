# Zsh environment customizations

typeset -U path
path=(${HOME}/.local/bin "$path[@]")
path+=(/usr/local/cuda/bin)

# Make sure language is set properly.
export LANG=en_US.UTF-8

# Set up our editor.
export EDITOR=emacs
export GIT_EDITOR="emacsclient -nw"
export ALTERNATE_EDITOR=""

# Set up MONITOR for use with polybar
case "$(hostname)" in
    llama)
	MONITOR="DVI-I-1"
	;;
    dromedary)
	MONITOR="eDP1"
	;;
    *)
	MONITOR=
esac
export MONITOR

# Set up theme for bat pager.
BAT_THEME='Buenos Aires'
export BAT_THEME

# Set up for Python and virtual environments.
export WORKON_HOME=${HOME}/.virtualenvs
export PROJECT_HOME=${HOME}/Code
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3.5m

