xrdb -merge "${HOME}/.Xresources"
dbus-daemon --print-address 4 --session
urxvtd --quiet --opendisplay --fork
xautolock -detectsleep -time 5 -locker "i3lock -e -f -i ${HOME}/.cache/lockscreen.png" &
# dunst &
setxkbmap -layout us,es -option 'grp:rctrl_rshift_toggle'
udiskie --automount --notify --no-tray &
[ -f ${HOME}/.fehbg ] && ${HOME}/.fehbg
xinput set-prop 'Logitech M510' 'libinput Natural Scrolling Enabled Default' 1
xinput set-prop 'Logitech M510' 'libinput Natural Scrolling Enabled' 1 
xinput set-prop 'Synaptics TM3053-003' 'Synaptics Scrolling Distance' -44 -44 
exec ssh-agent /usr/bin/i3
