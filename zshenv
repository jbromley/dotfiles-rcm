# Zsh environment customizations

# Configure the path. On MacOS user path_helper.
typeset -Ux PATH path
path=(${HOME}/.local/bin $path)
path+=${HOME}/.fzf/bin
if [ $(uname) = "Linux" ]; then
    path+=(/usr/local/cuda/bin)
fi

# Make sure language is set properly.
export LANG=en_US.UTF-8

# Set up our editor.
export EDITOR=nvim
export GIT_EDITOR=nvim
export ALTERNATE_EDITOR=""

if [ $(uname) = "Linux" ]; then
    # Java settings (Linux only)
    # export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
    # Set up stuff for snap applications.
    [ -x /etc/profile.d/apps-bin-path.sh ] && emulate sh -c  'source /etc/profile.d/apps-bin-path.sh'
fi


# Don't let Python venv add anything to the prompt.
export VIRTUAL_ENV_DISABLE_PROMPT=yes
