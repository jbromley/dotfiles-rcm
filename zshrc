# zshrc

source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then
    echo "Creating a zgen save..."
    
    zgen oh-my-zsh

    # Plugins
    zgen oh-my-zsh plugins/aws
    zgen oh-my-zsh plugins/extract
    zgen oh-my-zsh plugins/colored-man-pages
    zgen oh-my-zsh plugins/sudo
    zgen oh-my-zsh plugins/web-search
    zgen oh-my-zsh plugins/history-substring-search
    zgen oh-my-zsh plugins/fzf
    zgen oh-my-zsh plugins/z

    # Completions
    zgen load zsh-users/zsh-completions src

    # Theme
    # zgen oh-my-zsh custom/themes/powerlevel10k/powerlevel10k

    # Regenerate the init script.
    zgen save
fi

source ~/.zgen/robbyrussell/oh-my-zsh-master/custom/themes/powerless/powerless.zsh true

# Set up aliases.
[ -f ${HOME}/.aliases ] && source ${HOME}/.aliases

# Install the fuzzy finder (fzf).
# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
