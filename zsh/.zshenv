# Zsh environment customizations

typeset -U path
path=(${HOME}/.local/bin "$path[@]")
path+=(${HOME}/.cargo/bin)
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

