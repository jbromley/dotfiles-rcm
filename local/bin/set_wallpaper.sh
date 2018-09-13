#! /usr/bin/env bash
# Usage: set_wallpaper.sh
#
# Call nitrogen to set the wall paper and then create a lock screen
# image from the current wallpaper.

#{{{ Bash settings
set -o errexit   # abort on nonzero exitstatus
set -o nounset   # abort on unbound variable
set -o pipefail  # don't hide errors within pipes
#}}}

#{{{ Constants
readonly STORE_FILE="${HOME}/.config/nitrogen/bg-saved.cfg"
readonly WALLPAPER_DIR="${HOME}/Pictures/Wallpaper"
readonly LOCK_SCREEN="/tmp/lockscreen.png"
#}}}

#{{{ Main entry point
main () {
    local wallpaper

    nitrogen
    
    # Check for a nitrogen restore file.
    if [[ -f "${STORE_FILE}" ]]
    then
	# Found it. Read the wallpaper name.
	wallpaper=$(read_wallpaper_name "${STORE_FILE}")
	# Check if the lock screen matches the wallpaper. If it doesn't then
	# generate a new lock screen.
	if lockscreen_needs_update "${wallpaper}"
	then
	    convert -gaussian 13x13 "${wallpaper}" "${LOCK_SCREEN}"
	    # convert -scale 6.25% -scale 1600% "${wallpaper}" "${LOCK_SCREEN}"
	fi
    fi
}
#}}}

#{{{ Helper functions
# Read the wallpaper name from the Nitrogen store file.
read_wallpaper_name () {
    local cfg_file="$1"
    grep ^file "${cfg_file}" | cut -d= -f2
}

# Check if the lock screen image needs to be updated.
lockscreen_needs_update () {
    local wallpaper="$1"

    set +o errexit
    puzzle-diff -e "${wallpaper}" "${LOCK_SCREEN}" > /dev/null 2>&1
    local result="$?"
    set -o errexit

    [[ "${result}" != "10" ]]
}
#}}}

main "${@}"   
