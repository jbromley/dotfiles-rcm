# Zsh environment customizations

# Make sure language is set properly.
export LANG=en_US.UTF-8

# Editor
export EDITOR="nvim"
export VISUAL="neovide"
export GIT_EDITOR="nvim"

# Zsh environment variables
export HISTFILE="${HOME}/.zhistory"
export HISTSIZE=8192
export SAVEHIST=8192

# Fzf prompt customizations
export FZF_CTRL_T_OPTS="--prompt 'file> '"
export FZF_ALT_C_OPTS="--prompt 'cd> '"
export FZF_CTRL_R_OPTS="--prompt 'history> '"

# Linux-specific configuration
if [ $(uname) = "Linux" ]; then
    # Set up stuff for snap applications.
    [ -f /etc/profile.d/apps-bin-path.sh ] && emulate sh -c  'source /etc/profile.d/apps-bin-path.sh'
fi

# Don't let Python venv add anything to the prompt.
export VIRTUAL_ENV_DISABLE_PROMPT=yes


# Golang
if [ -d "${HOME}/go" ]; then
    export GOPATH="${HOME}/go"
    export GOBIN="${GOPATH}/bin"
fi

# Configure the path. On MacOS user path_helper.
typeset -Ux PATH path
path=(${HOME}/.local/bin ${HOME}/.fzf/bin $path)

[ -n "${GOBIN}" ] && path+=("${GOBIN}")
[ -d /usr/local/cuda/bin ] && path+=(/usr/local/cuda/bin)
[ -d /opt/local/bin ] && path+=(/opt/local/bin)

# ROS 2
if [ -f /opt/ros/galactic/setup.zsh ]; then
    source /opt/ros/galactic/setup.zsh
    export ROS_DOMAIN_ID=17
fi
