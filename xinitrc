nitrogen --restore
dbus-daemon --print-address 4 --session
urxvtd -q -o -f
xautolock -detectsleep -time 5 -locker "i3lock -e -f -i /tmp/lockscreen.png" &
dunst &
# compton --no-dock-shadow --fading --inactive-opacity=0.9 --daemon
udiskie --automount --notify --no-tray &
exec ssh-agent /home/jay/.local/bin/xmonad
