xrdb -merge "${HOME}/.Xresources"
nitrogen --restore
dbus-daemon --print-address 4 --session
urxvtd -q -o -f
xautolock -detectsleep -time 5 -locker "i3lock -e -f -i ${HOME}/.cache/lockscreen.png" &
dunst &
# compton --no-dock-shadow --fading --inactive-opacity=0.9 --daemon
xinput set-prop 'Logitech M510' 'libinput Natural Scrolling Enabled' 1 &
setxkbmap -layout us,es -option 'grp:rctrl_rshift_toggle'
udiskie --automount --notify --no-tray &
export XDG_CURRENT_DESKTOP=kde dolphin
exec ssh-agent /usr/bin/xmonad
