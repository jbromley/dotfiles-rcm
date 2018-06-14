[ -f ~/.fehbg ] && sh ~/.fehbg
eval $(gnome-keyring-daemon --components=gpg,pkcs11,secrets,ssh --daemonize --login)
export GNOME_KEYRING_CONTROL
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_PID

dbus-daemon --print-address 4 --session
gnome-settings-daemon &
# /usr/lib/gnome-settings-daemon/gsd-printer
# nm-applet &
# blueman-applet &
# pasystray &
# /usr/lib/gvfs/gvfs-udisks2-volume-monitor &
# /usr/lib/gvfs/gvfs-mtp-volume-monitor &
# /usr/lib/gvfs/gvfs-afc-volume-monitor &
urxvtd -q -o -f
exec ssh-agent /usr/local/bin/stumpwm
