# Add our local bin directory to the path.
set -g --prepend PATH $HOME/.local/bin

# Show virtualenv name.
set -g tide_virtual_env_display_mode 'venvName'

# Set up asdf.
source ~/.asdf/asdf.fish
source ~/.config/fish/completions/asdf.fish
