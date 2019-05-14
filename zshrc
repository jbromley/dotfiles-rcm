# zshrc

source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then
    echo "Creating a zgen save..."
    
    zgen oh-my-zsh

    # Plugins
    zgen oh-my-zsh custom/plugins/aws
    # zgen oh-my-zsh plugins/emacs
    zgen oh-my-zsh plugins/extract
    zgen oh-my-zsh plugins/colored-man-pages
    zgen oh-my-zsh plugins/mvn
    zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/sudo
    zgen oh-my-zsh plugins/web-search
    zgen oh-my-zsh plugins/history-substring-search
    zgen oh-my-zsh plugins/fzf
    zgen oh-my-zsh plugins/z

    # Completions
    zgen load zsh-users/zsh-completions src

    # Theme
    # zgen load romkatv/powerlevel10k powerlevel10k

    # Regenerate the init script.
    zgen save
fi

# Turn off auto pushd.
setopt noautopushd

source "${HOME}/.zsh/powerless/powerless.zsh" true

# Set up aliases.
[ -f ${HOME}/.aliases ] && source ${HOME}/.aliases || true
