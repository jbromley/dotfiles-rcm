# zshrc

source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then
    echo "Creating a zgen save..."
    
    zgen oh-my-zsh

    # Plugins
    zgen oh-my-zsh plugins/colored-man-pages
    zgen oh-my-zsh plugins/sudo
    zgen oh-my-zsh plugins/web-search
    zgen load zsh-users/zsh-history-substring-search

    # Completions
    zgen load zsh-users/zsh-completions src

    # Theme
    zgen oh-my-zsh custom/themes/muse
    
    # Regenerate the init script.
    zgen save
fi


# Set up aliases.
[ -f ${HOME}/.aliases ] && source ${HOME}/.aliases

# Install the fuzzy finder (fzf).
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
