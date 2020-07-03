# Add our local bin directory to the path.
set PATH "/home/jay/.local/bin:$PATH"
# Set the default editor.
set -U EDITOR vim

# Use new fzf key bindings.
set -U FZF_LEGACY_KEYBINDINGS 0

starship init fish | source

