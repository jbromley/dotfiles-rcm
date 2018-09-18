#! /usr/bin/env bash
if (( $# < 1 ))
then
    echo $(basename $0) monitor
    exit 1
fi

pkill polybar
sleep 1
MONITOR="$1" polybar topbar > /dev/null 2> /tmp/polybar.log &


