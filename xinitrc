# Xinitrc for window manager environment

# Set the wallpaper 
[ -x ${HOME}/.fehbg ] && ${HOME}/.fehbg || xsetroot -solid black

# Read in X resource configuration
xrdb -merge "${HOME}/.Xresources"
xrdb -merge "${HOME}/.config/i3/styles/root"
xrdb -merge "${HOME}/.config/i3/styles/Xresources"

dbus-daemon --print-address 4 --session
# urxvtd --quiet --opendisplay --fork
xautolock -detectsleep -time 5 -locker "i3lock -e -f -i ${HOME}/.cache/lockscreen.png" &
# dunst &
udiskie --automount --notify --no-tray &

# Configure natural scrolling on pointing devices.
xinput set-prop 'Logitech M510' 'libinput Natural Scrolling Enabled Default' 1
xinput set-prop 'Logitech M510' 'libinput Natural Scrolling Enabled' 1 
xinput set-prop 'Synaptics TM3053-003' 'Synaptics Scrolling Distance' -44 -44 

# Set up keyboard maps and switching.
setxkbmap -layout us,es -option 'grp:rctrl_rshift_toggle' -option 'caps:escape'

exec ssh-agent /usr/bin/i3
