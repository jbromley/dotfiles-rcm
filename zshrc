# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi

#======================================================================
# Zsh configuration file
# Inspired by the blog post "Understanding and Configuring Zsh"
# (https://thevaluable.dev/zsh-install-configure/) and the from-scratch
# configuration on GitHub at https://github.com/Phantas0s/.dotfiles/tree/master/zsh
#======================================================================

# Emacs key bindings
bindkey -e

# Add plugins directory to path.
fpath=(${HOME}/.zsh/plugins $fpath)

# Navigation
setopt AUTO_CD

# setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
# setopt PUSHD_SILENT

setopt CORRECT
# setopt CDABLE_VARS
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
source ${HOME}/.aliases

# ASDF
ASDF_INIT=${HOME}/.asdf/asdf.sh
if [ -f ${ASDF_INIT} ]; then
    source ${ASDF_INIT}
    fpath=(${HOME}/.asdf/completions $fpath)
fi

# Fzf
source ${HOME}/.fzf/shell/completion.zsh
source ${HOME}/.fzf/shell/key-bindings.zsh
bindkey -s '^V' 'emacsclient -tty $(fzf);^M'

#
# Plugins
#
plugin_dir=${HOME}/.zsh/plugins
fpath=(${HOME}/.zsh/plugins $fpath)

# bd
autoload -Uz bd; bd

# Syntax highlighting
source ${plugin_dir}/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Fish-like auto-suggestions
source ${plugin_dir}/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#606060"

# Change directories with z
source ${plugin_dir}/zsh-z/zsh-z.plugin.zsh

#
# Completion
#
fpath=(${plugin_dir}/zsh-completions/src $fpath)
autoload -Uz compinit; compinit
_comp_options+=(globdots)
# source ${plugin_dir}/completion.zsh

# ROS 2 colcon
colcon_comp=/usr/share/colcon_argcomplete/hook/colcon-argcomplete.zsh
[ -f ${colcon_comp} ] && source ${colcon_comp}

# Allow zsh to use bash completions
autoload bashcompinit; bashcompinit

# ROS 2
if [ -f /opt/ros/foxy/setup.zsh ]; then
    source /opt/ros/foxy/setup.zsh
    export ROS_DOMAIN_ID=17
fi

# Prompt
# source ~/.zsh/plugins/powerlevel10k/powerlevel10k.zsh-theme
# [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
function set_term_title() {
    title=$(pwd | sed -e "s|${HOME}|~|")
    echo -ne "\e]0;${title}\a"
}
precmd_functions+=(set_term_title)
eval "$(starship init zsh)"
# source ${plugin_dir}/git.zsh
# autoload -U promptinit; promptinit
# prompt purity
