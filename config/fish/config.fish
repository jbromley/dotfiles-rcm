# Show virtualenv name.
set -g tide_virtual_env_display_mode 'venvName'

# Set up asdf.
source ~/.asdf/asdf.fish
source ~/.config/fish/completions/asdf.fish

# Set up Go directories.
set --global --export GOPATH ~/go
set --global --export GOBIN $GOPATH/bin

