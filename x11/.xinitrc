[ -f ~/.fehbg ] && sh ~/.fehbg
eval "gnome-keyring-daemon --components=pkcs11,secrets,ssh --daemonize --login"
dbus-daemon --print-address 4 --session
# gnome-settings-daemon
# /usr/lib/gnome-settings-daemon/gsd-printer
nm-applet &
blueman-applet &
pasystray &
# /usr/lib/gvfs/gvfs-udisks2-volume-monitor &
# /usr/lib/gvfs/gvfs-mtp-volume-monitor &
# /usr/lib/gvfs/gvfs-afc-volume-monitor &
urxvtd -q -o -f
# slstatus &
exec /usr/bin/i3
