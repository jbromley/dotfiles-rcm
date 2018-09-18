#! /usr/bin/env bash
set -o nounset
set -o errexit

get_display () {
    xrandr | grep "connected primary" | cut -d' ' -f1
}

get_default_resolution () {
    xrandr | grep '*' | mawk -F' ' '{print $1;}'
}

get_screen_resolution () {
    resolution=$(xdpyinfo | grep -oP 'dimensions:\s+\K\S+' | sed -e 's/x/ /')
    echo "${resolution}"
}

usage () {
    echo "Usage: $(basename $1) [-h|--help] [-d|--dry-run] [-r|--reset] scale_factor"
}

declare -a positional
declare -a resolution
declare -i dryrun

progname="$(basename $0)"
positional=()
dryrun=0
resetscreen=0
while [[ $# -gt 0 ]]
do
    key="$1"
    case "$key" in
	-d|--dry-run)
	    dryrun=1
	    shift
	    ;;
	-h|--help)
	    usage "${progname}"
	    exit
	    ;;
	-r|--reset)
	    resetscreen=1
	    shift
	    ;;
	*)
	    positional+=("$1")
	    shift
	    ;;
    esac
done
set -- "${positional[@]}" # restore positional parameters

if (( "$#" < 1 )) && (( ${resetscreen} != 1 )); then
    usage "${progname}"
    exit -1
fi

if (( ${resetscreen} == 1 )); then
    scale_factor="reset"
else
    scale_factor="$1"
fi
output=$(xrandr | grep "connected primary" | cut -d' ' -f1)
resolution=($(get_screen_resolution))
width="${resolution[0]}"
height="${resolution[1]}"
echo "Primary display is ${output}."
echo "Detected resolution: ${width}x${height}."
echo "Scale factor: ${scale_factor}."
scaled_width=$(echo "${width} * ${scale_factor}" | bc | sed -e '/\./ s/\.\{0,1\}0\{1,\}$//')
scaled_height=$(echo "${height} * ${scale_factor}" | bc -l | sed -e '/\./ s/\.\{0,1\}0\{1,\}$//')
echo "Scaled resolution: ${scaled_width}x${scaled_height}."
if (( ${dryrun} == 0 )); then
    if (( ${resetscreen} == 0 )); then
	xrandr --output "${output}" --scale "${scale_factor}"x"${scale_factor}" --panning "${scaled_width}"x"${scaled_height}"
    else
	xrandr --output "${output}" -s 0 --panning "$(get_default_resolution)"
    fi	
else
    if (( ${resetscreen} == 0 )); then
	echo "Dry run: xrandr --output ${output} --scale ${scale_factor}x${scale_factor} --panning ${scaled_width}x${scaled_height}"
    else
	echo "Dry run: xrandr --output ${output} -s 0 --panning $(get_default_resolution)"
    fi
fi


