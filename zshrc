config_files=(exports \
		  setopt \
		  colors \
		  completion \
		  aliases \
		  bindkeys \
		  functions \
		  history \
		  zsh_hooks \
		  plugins \
		  prompt)

for config in $config_files; do
    [ -f "${HOME}/.zsh/${config}.zsh" ] && source "${HOME}/.zsh/${config}.zsh"
done

# Set up fzf.
[ -f "${HOME}/.fzf.zsh" ] && source "${HOME}/.fzf.zsh"

# tmux customization
precmd() {
  if [[ -n "$TMUX" ]]; then
    tmux setenv "$(tmux display -p 'TMUX_PWD_#D')" "$PWD"
  fi
}

