# Zsh environment customizations

# Make sure language is set properly.
export LANG=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8

# Editor
export EDITOR="hx"
export VISUAL="neovide"
export GIT_EDITOR=hx

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

# If these paths exist, add them to PATH.
extra_paths=(~/.cargo/bin /usr/lib/llvm-15/bin /usr/local/cuda/bin)
[ -n "${GOBIN}" ] && extra_paths+=("${GOBIN}")

for extra_path in $extra_paths; do
    [ -d "$extra_path" ] && path+=("$extra_path")
done
