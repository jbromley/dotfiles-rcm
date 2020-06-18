xrdb -merge "${HOME}/.Xresources"
dbus-daemon --print-address 4 --session
urxvtd --quiet --opendisplay --fork
xautolock -detectsleep -time 5 -locker "i3lock -e -f -i ${HOME}/.cache/lockscreen.png" &
# dunst &
xinput set-prop 'Logitech M510' 'libinput Natural Scrolling Enabled' 1 &
setxkbmap -layout us,es -option 'grp:rctrl_rshift_toggle'
udiskie --automount --notify --no-tray &
[ -f ${HOME}/.fehbg ] && source ${HOME}/.fehbg
exec ssh-agent /usr/bin/i3
