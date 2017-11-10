#! /usr/bin/env bash
eval $(gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
export GNOME_KEYRING_CONTROL
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_PID

xsettingsd &
compton -b
nitrogen --restore
urxvtd -q -f -o 
#/usr/local/bin/st -e tmux &
#emacs &
exec ssh-agent /usr/local/bin/stumpwm
