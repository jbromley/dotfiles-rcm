#! /usr/bin/env bash
set -o errexit
set -o errtrace
set -o nounset
set -o pipefail

usage() {
  echo "Usage: $(basename $0) FONT"
}

check_font() {
  local test_font="${1}"
  fc-list -q "${test_font}"
}

get_current_font() {
  local xresources="${HOME}/.dotfiles/Xresources"
  local regex='URxvt\.font: xft:(.*):style'
  if [[ $(grep URxvt.font "${xresources}") =~ $regex ]]; then
    current_font="${BASH_REMATCH[1]}"
  else
    echo "Can't find the current font name!"
    exit 1
  fi
  echo "${current_font}"
}

# Main entry point
if (("$#" < 1)); then
  echo "Error: must specify font"
  usage "$0"
  exit 1
fi

new_font="$@"
echo "Setting font to ${new_font}"

if ! check_font "${new_font}"; then
  echo "No such font: ${new_font}"
  exit 1
fi

current_font=$(get_current_font)
echo "Current font is ${current_font}"
old_dir=$(pwd)
cd ~/.dotfiles/
for f in $(ag -l "${current_font}"); do
  sed -i -e "s/${current_font}/${new_font}/g" "$f"
done
cd "${old_dir}"
