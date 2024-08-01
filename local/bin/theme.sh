#! /usr/bin/env bash
set -o errexit
set -o errtrace
set -o nounset
set -o pipefail

usage() {
  echo "Usage: $(basename $0) THEME"
  echo "   THEME is light or dark"
}

if [ "$#" != "1" ]; then
  echo "Error: must specify theme"
  usage "$0"
  exit 1
fi

light_theme() {
  kitty +kitten themes --config-file-name=themes.conf "Modus Operandi"
  ln -sf ~/.config/alacritty/themes/modus_operandi.toml ~/.config/alacritty/theme.toml
  ln -sf ~/.dotfiles/config/bat/config-light ~/.config/bat/config
  ln -sf ~/.dotfiles/config/nvim/lua/plugins/light-colorscheme.lua ~/.config/nvim/lua/plugins/current-colorscheme.lua
}

dark_theme() {
  kitty +kitten themes --config-file-name=themes.conf Dracula
  ln -sf ~/.config/alacritty/themes/dracula.toml ~/.config/alacritty/theme.toml
  ln -sf ~/.dotfiles/config/bat/config-dark ~/.config/bat/config
  ln -sf ~/.dotfiles/config/nvim/lua/plugins/dark-colorscheme.lua ~/.config/nvim/lua/plugins/current-colorscheme.lua
}

case "$1" in
light)
  light_theme
  ;;
dark)
  dark_theme
  ;;
*)
  echo "Unknown theme: $1"
  usage "$0"
  exit 2
  ;;
esac
