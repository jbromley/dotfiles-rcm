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

# Add plugins directory to path.
fpath=(${HOME}/.zsh/plugins $fpath)

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
source ${HOME}/.asdf/asdf.sh

# Fzf
source ${HOME}/.fzf/shell/completion.zsh
source ${HOME}/.fzf/shell/key-bindings.zsh
bindkey -s '' 'nvim $(fzf);'

# Syntax highlighting
source ${HOME}/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Completion
fpath=(${HOME}/.asdf/completions ${HOME}/.zsh/plugins/zsh-completions/src $fpath)
autoload -Uz compinit; compinit
_comp_options+=(globdots)
source ${HOME}/.zsh/plugins/completion.zsh

# Allow zsh to use bash completions
autoload bashcompinit
bashcompinit

# z
source ~/.zsh/plugins/zsh-z/zsh-z.plugin.zsh

# Prompt
source ~/.zsh/plugins/powerlevel10k/powerlevel10k.zsh-theme
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
