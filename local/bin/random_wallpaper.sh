#! /usr/bin/env bash
wallpaper=$(find ~/Pictures/Wallpaper -type f | shuf -n 1)
DISPLAY=:0 DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus feh --bg-fill "${wallpaper}"
~/.local/bin/make_lockscreen.sh ${wallpaper} &
