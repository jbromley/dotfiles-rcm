# Show virtualenv name.
set -g tide_virtual_env_display_mode 'venvName'

# Set up asdf.
source ~/.asdf/asdf.fish
# source ~/.config/fish/completions/asdf.fish

# Set up default editor.
set --global --export EDITOR nvim

# Set up Go directories.
set --global --export GOPATH ~/go
set --global --export GOBIN $GOPATH/bin

# Aliases
alias xo=xdg-open

# Fix fzf.fish C-f binding.
bind --erase \cf
bind \co __fzf_search_current_dir

# Adjust fzf preview options
set --export FZF_DEFAULT_OPTS '--cycle --layout=reverse --border --height=75% --preview-window=right:60%:wrap --marker="*"'
