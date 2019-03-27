# Zsh environment customizations

# Configure the path. On MacOS user path_helper.
typeset -TUx PATH path
# [ -x /usr/libexec/path_helper ] && eval $(/usr/libexec/path_helper -s)
path=(${HOME}/.local/bin $path)
if [ $(uname) = "Linux" ]; then
    path+=(/usr/local/cuda/bin)
fi

# Make sure language is set properly.
export LANG=en_US.UTF-8

# Set up our editor.
export EDITOR=emacs
export GIT_EDITOR="nvim"
export ALTERNATE_EDITOR=""

# Java settings
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'

# Set up stuff for snap applications.
[ -x /etc/profile.d/apps-bin-path.sh ] && emulate sh -c  'source /etc/profile.d/apps-bin-path.sh'

# Don't let Python venv add anything to the prompt.
export VIRTUAL_ENV_DISABLE_PROMPT=yes

# Powerlevel10k prompt configuration
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(root_indicator dir_writable dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status command_execution_time background_jobs)
