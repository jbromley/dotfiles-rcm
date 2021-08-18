# Set up asdf.
source ~/.asdf/asdf.fish

# Set up default editor.
set --global --export ALTERNATE_EDITOR ""
set --global --export EDITOR "emacsclient -t"

# Set up Go directories.
set --global --export GOPATH ~/go
set --global --export GOBIN $GOPATH/bin

# Aliases
alias b=batcat
alias et='emacsclient --tty'
alias e='emacsclient --create-frame --no-wait'
alias xo=xdg-open

# Configure fzf.
set --export FZF_DEFAULT_OPTS '--cycle --layout=reverse --border --height=75% --preview-window=right:60%:wrap --marker="*"'
# fzf_configure_bindings --directory=\co
