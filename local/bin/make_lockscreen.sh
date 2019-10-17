#! /usr/bin/env bash
#
# Usage: make_lockscreen.sh
#
# Create a lock screen image from the current wallpaper. We assume the
# wallpaper is being set with nitrogen.

#{{{ Bash settings
set -o errexit   # abort on nonzero exitstatus
set -o nounset   # abort on unbound variable
set -o pipefail  # don't hide errors within pipes
#}}}

#{{{ Constants
readonly FEHBG_FILE="${HOME}/.fehbg"
readonly WALLPAPER_DIR="${HOME}/Pictures/Wallpaper"
readonly LOCK_SCREEN="${HOME}/.cache/lockscreen.png"
#}}}

#{{{ Main entry point
main () {
    local wallpaper

    if [[ $# -gt 0 ]]
    then
	# The wallpaper name was passed in.
	wallpaper="$1"
    elif [[ -f ~/.fehbg ]]
    then
	    # Get the wallpaper from the last one set with feh.
	    wallpaper=$(read_wallpaper_name ${FEHBG_FILE})
    else
        # There is no wallpaper information, select a random wallpaper and use that.
        wallpaper=$(select_random_file "${WALLPAPER_DIR}")
	feh --bg-fill "${wallpaper}"
    fi
    make_lock_screen "${wallpaper}"
}
#}}}

#{{{ Helper functions

# Read the wallpaper name from feh.
read_wallpaper_name () {
    local cfg_file="$1"
    tail -n1 "${cfg_file}" | cut -d' ' -f3 | tr -d "'"
}

# Given a directory, select a random file.
select_random_file () {
    local dir="$1"
    f=$(find "${dir}" -type f | shuf | head -n1)
    echo "${f}"
}

# Get the screen resolution.
get_screen_resolution () {
    echo $(xrandr --current | grep '*' | uniq | mawk '{print $1}')
}

# Create a new lock screen.
make_lock_screen () {
    screen_resolution=$(get_screen_resolution)
    convert -resize "${screen_resolution}"^ -gravity center -extent "${screen_resolution}" \
	    -blur 11x11 "${wallpaper}" "${LOCK_SCREEN}"
    # convert -resize "${screen_resolution}"^ -gravity center -extent "${screen_resolution}" \
    #       -scale 6.25% -scale 1600% "${wallpaper}" "${LOCK_SCREEN}"
}

main "${@}"   
