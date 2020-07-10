# Add our local bin directory to the path.
set PATH "/home/jay/.local/bin:$PATH"
# Set the default editor.
set -U EDITOR vim

# Use new fzf key bindings.
set -U FZF_LEGACY_KEYBINDINGS 0

# Prompt configuration
# starship init fish | source
set -g pure_symbol_prompt ">"
set -g pure_symbol_reverse_prompt "<"
set -g pure_symbol_unpulled_commits "v"
set -g pure_symbol_unpushed_commits "^"

