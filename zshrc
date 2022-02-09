# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#======================================================================
# Zsh configuration file
# Inspired by the blog post "Understanding and Configuring Zsh"
# (https://thevaluable.dev/zsh-install-configure/) and the from-scratch
# configuration on GitHub at https://github.com/Phantas0s/.dotfiles/tree/master/zsh
#======================================================================

# Emacs key bindings
bindkey -e

# Navigation
setopt AUTO_CD

# setopt auto_pushd
setopt PUSHD_IGNORE_DUPS
# setopt pushd_silent

setopt CORRECT
# setopt CDABLE_VARS
setopt EXTENDED_GLOB

autoload -Uz bd; bd

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
[ -f ${ASDF_INIT} ] && source ${ASDF_INIT}

# Fzf
source ${HOME}/.fzf/shell/completion.zsh
source ${HOME}/.fzf/shell/key-bindings.zsh
bindkey -s '' 'nvim $(fzf);'

#
# Plugins
#
plugin_dir=${HOME}/.zsh/plugins
fpath=(${HOME}/.zsh/plugins $fpath)

# Syntax highlighting
source ${plugin_dir}/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Fish-like auto-suggestions
source ${plugin_dir}/zsh-autosuggestions/zsh-autosuggestions.zsh

# Change directories with z
source ${plugin_dir}/zsh-z/zsh-z.plugin.zsh

#
# Completion
#
fpath=(${HOME}/.asdf/completions ${plugin_dir}/zsh-completions/src $fpath)
autoload -Uz compinit; compinit
_comp_options+=(globdots)
source ${plugin_dir}/completion.zsh

# Allow zsh to use bash completions
autoload bashcompinit
bashcompinit

# Prompt
source ~/.zsh/plugins/powerlevel10k/powerlevel10k.zsh-theme
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
