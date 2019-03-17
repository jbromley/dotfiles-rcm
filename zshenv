# Zsh environment customizations

typeset -U path
path=(${HOME}/.local/bin "$path[@]")
if [ $(uname) = "Linux" ]; then
    path+=(/usr/local/cuda/bin)
else
    path=(/opt/local/bin "$path[@]")
fi

# Make sure language is set properly.
export LANG=en_US.UTF-8

# Set up our editor.
export EDITOR=emacs
export GIT_EDITOR="emacsclient -q -nw"
export ALTERNATE_EDITOR=""

# Java settings
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'

# Set up stuff for snap applications.
[ -x /etc/profile.d/apps-bin-path.sh ] && emulate sh -c  'source /etc/profile.d/apps-bin-path.sh'

