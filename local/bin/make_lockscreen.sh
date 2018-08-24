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
readonly STORE_FILE="${HOME}/.config/nitrogen/bg-saved.cfg"
readonly WALLPAPER_DIR="${HOME}/Pictures/Wallpaper"
readonly LOCK_SCREEN="/tmp/lockscreen.png"
#}}}

#{{{ Main entry point
main () {
    local wallpaper
    
    # Check for a nitrogen restore file.
    if [[ -f "${STORE_FILE}" ]]
    then
	# Found it. Read the wallpaper name.
	wallpaper=$(read_wallpaper_name "${STORE_FILE}")
	# Check if the lock screen matches the wallpaper. If it doesn't then
	# generate a new lock screen.
	if lockscreen_needs_update "${wallpaper}"
	then
	    #convert -gaussian 13x13 "${wallpaper}" "${LOCK_SCREEN}"
	    convert -scale 6.25% -scale 1600% "${wallpaper}" "${LOCK_SCREEN}"
	    xdg-open "${LOCK_SCREEN}"
	fi
    else
	# There is no wallpaper information, select a random wallpaper and use that.
	wallpaper=$(select_random_file "${WALLPAPER_DIR}")
	convert -gaussian 11x11 "${wallpaper}" "${LOCK_SCREEN}"
	write_store_file "${STORE_FILE}" "${wallpaper}"
	cat ${STORE_FILE}
	xdg-open "${LOCK_SCREEN}"
    fi
}
#}}}

#{{{ Helper functions
# Given a directory, select a random file.
select_random_file () {
    local dir="$1"
    f=$(find "${dir}" -type f | shuf | head -n1)
    echo "${f}"
}

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

# Write the new wallpaper to the Nitrogen store file.
write_store_file () {
    local output_file="$1"
    local wallpaper="$2"
    cat <<STORE_FILE > "${output_file}"
[${DISPLAY}.0]
file=${wallpaper}
mode=0
bgcolor=#000000
STORE_FILE
}
#}}}


main "${@}"   
