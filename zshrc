#======================================================================
# Zsh configuration file
# Inspired by the blog post "Understanding and Configuring Zsh"
# (https://thevaluable.dev/zsh-install-configure/) and the from-scratch
# configuration on GitHub at https://github.com/Phantas0s/.dotfiles/tree/master/zsh
#======================================================================

# Enable Powerlevel10k instant prompt. 
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Emacs key bindings
bindkey -e

# Changing directories
setopt AUTO_CD
setopt PUSHD_IGNORE_DUPS
setopt CORRECT
setopt EXTENDED_GLOB

# History
setopt EXTENDED_HISTORY
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt HIST_VERIFY

# Aliases
[ -f ${HOME}/.aliases ] && source ${HOME}/.aliases

# Mise en place
mise_executable=${HOME}/.local/bin/mise
if [ -x ${mise_executable} ]; then
  eval "$(${mise_executable} activate zsh)"
fi

# Fzf
if [ "$(uname)" = "Darwin" ]; then
    fzf_dir=/usr/local/opt/fzf/shell
else
    fzf_dir=${HOME}/.fzf/shell
fi
source ${fzf_dir}/completion.zsh
source ${fzf_dir}/key-bindings.zsh
bindkey -s '^V' 'nvim $(fzf --preview "bat --color always {}");^M'
bindkey -s '^W' 'fzf --preview="bat --color always {}" --bind shift-up:preview-page-up,shift-down:preview-page-down;^M'

#
# Plugins
#
plugin_dir=${HOME}/.zsh
fpath=(${plugin_dir} $fpath)

# bd
source ${plugin_dir}/zsh-bd/bd.zsh

# Syntax highlighting
source ${plugin_dir}/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Fish-like auto-suggestions
source ${plugin_dir}/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#606060"

# Change directories with z
# source ${plugin_dir}/zsh-z/zsh-z.plugin.zsh

# Powerlevel10k prompt
source ${plugin_dir}/powerlevel10k/powerlevel10k.zsh-theme

#
# Completion
#
# ROS 2 colcon
colcon_comp=/usr/share/colcon_argcomplete/hook/colcon-argcomplete.zsh
[ -f ${colcon_comp} ] && source ${colcon_comp}

ros2_arg_comp=${ROS_DIR}/share/ros2cli/environment/ros2-argcomplete.zsh
[ -f ${ros2_arg_comp} ] && source ${ros2_arg_comp}

fpath=(${plugin_dir}/zsh-completions/src $fpath)
autoload -Uz compinit; compinit
_comp_options+=(globdots)
source ${plugin_dir}/completion.zsh

# Allow zsh to use bash completions
autoload bashcompinit; bashcompinit

# GHC configuration
[ -f "${HOME}/.ghcup/env" ] && source "${HOME}/.ghcup/env" # ghcup-env

# Check ps for a process
function psinfo() {
    if [ -z "$1" ]; then
        echo "Usage: psinfo <regex>"
    else
        ps -ef | grep -E "$1" | grep -v grep
    fi
}

# Use z to jump and push the directory.
function zp() {
    if [ -z "$1" ]; then
        echo "Usage: zp DIRECTORY"
    else
        pushd $(zoxide query "$1")
    fi
}

# Push or pop just using `p` and context.
function p() {
    case "$#" in
    0)
        popd
        ;;
    1)
        pushd "$1"
        ;;
    *)
        echo "p [DIR]"
        false
        ;;
    esac
}

# Prompt
function set_term_title() {
    title=$(pwd | sed -e "s|${HOME}|~|")
    echo -ne "\e]0;${title}\a"
}
precmd_functions+=(set_term_title)

# Set up helper programs
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -x /usr/bin/zoxide ] && eval "$(zoxide init zsh)" 

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

