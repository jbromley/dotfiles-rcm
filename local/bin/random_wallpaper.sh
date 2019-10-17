#! /usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

readonly WALLPAPER_DIR="${HOME}/Pictures/Wallpaper"

get_x_display() {
    ps -u $(id -u) -o pid= | \
	while read pid; do
	    cat /proc/$pid/environ 2>/dev/null | tr '\0' '\n' | grep '^DISPLAY=:'
	done | grep -o ':[0-9]*' | sort -u
}

export DISPLAY=$(get_x_display)
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id -u)/bus

wallpaper=$(find "${WALLPAPER_DIR}" -type f | shuf -n 1)

feh --bg-fill "${wallpaper}"
${HOME}/.local/bin/make_lockscreen.sh ${wallpaper} &
