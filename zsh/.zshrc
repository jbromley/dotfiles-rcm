# Oh my zsh customizations.
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"

# load zgen
source "${HOME}/.zgen/zgen.zsh"

# if the init scipt doesn't exist
if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh

    # plugins
    # zgen oh-my-zsh plugins/emacs
    # zgen oh-my-zsh plugins/tmux
    # zgen oh-my-zsh plugins/git
    # zgen oh-my-zsh plugins/sudo
    # zgen oh-my-zsh plugins/python
    zgen oh-my-zsh plugins/virtualenv
    zgen oh-my-zsh plugins/virtualenvwrapper
    zgen oh-my-zsh plugins/colorize
    zgen oh-my-zsh plugins/colored-man-pages
    zgen oh-my-zsh plugins/command-not-found
    # zgen oh-my-zsh plugins/docker
    # zgen oh-my-zsh plugins/docker-compose
    zgen oh-my-zsh plugins/web-search
    zgen oh-my-zsh plugins/themes
    #zgen load zsh-users/zsh-syntax-highlighting

    # bulk load
    zgen loadall <<EOPLUGINS
        zsh-users/zsh-history-substring-search
EOPLUGINS
    # ^ can't indent this EOPLUGINS

    # completions
    zgen load zsh-users/zsh-completions src

    # theme
    zgen oh-my-zsh themes/blinks

    # save all to init script
    zgen save
fi
