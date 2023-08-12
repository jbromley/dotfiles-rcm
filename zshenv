# Zsh environment customizations

# Make sure language is set properly.
export LANG=en_US.UTF-8

# Editor
export EDITOR="nvim"
export VISUAL="neovide"
export GIT_EDITOR="nvim --clean"

# Zsh environment variables
export HISTFILE="${HOME}/.zhistory"
export HISTSIZE=16384
export SAVEHIST=16384

# Fzf prompt customizations
export FZF_CTRL_T_OPTS="--prompt 'file> '"
export FZF_ALT_C_OPTS="--prompt 'cd> '"
export FZF_CTRL_R_OPTS="--prompt 'history> '"

# Don't let Python venv add anything to the prompt.
export VIRTUAL_ENV_DISABLE_PROMPT=yes

# Custom zsh-abbr temp directory
export ABBR_TMPDIR=/tmp/zsh-abbr-${USER}/

# Golang
if [ -d "${HOME}/go" ]; then
    export GOPATH="${HOME}/go"
    export GOBIN="${GOPATH}/bin"
fi

# Rust
[ -f "${HOME}/.cargo/env" ] && source "${HOME}/.cargo/env"

# ROS 2
ROS_DIR=/opt/ros/iron
if [ -f ${ROS_DIR}/setup.zsh ]; then
    source ${ROS_DIR}/setup.zsh
    export ROS_DOMAIN_ID=17
    export RMW_IMPLEMENTATION=rmw_cyclonedds_cpp
fi

# Configure the path.
typeset -Ux PATH path
path=(${HOME}/.local/bin $path)
[ -d ${HOME}/.cargo/bin ] && path=(${HOME}/.cargo/bin $path)
[ -n "${GOBIN}" ] && path+=("${GOBIN}")
[ -d /usr/local/cuda/bin ] && path+=(/usr/local/cuda/bin)
