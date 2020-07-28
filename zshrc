#======================================================================
# Zsh configuration file
# Inspired by the blog post "Understanding and Configuring Zsh" 
# (https://thevaluable.dev/zsh-install-configure/) and the from-scratch 
# configuration on GitHub at https://github.com/Phantas0s/.dotfiles/tree/master/zsh
#======================================================================

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

# Completion
fpath=(${HOME}/.zsh/plugins/zsh-completions/src $fpath)
autoload -U compinit; compinit
_comp_options+=(globdots)
source ${HOME}/.zsh/plugins/completion.zsh

# Fzf
source /usr/share/doc/fzf/examples/completion.zsh
source /usr/share/doc/fzf/examples/key-bindings.zsh
bindkey -s '' 'vim $(fzf);'

# Syntax highlighting
source ${HOME}/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Prompt 
eval "$(starship init zsh)"
