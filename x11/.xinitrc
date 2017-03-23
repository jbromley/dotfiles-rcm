#! /usr/bin/env bash
eval $(gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
export GNOME_KEYRING_CONTROL
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_PID

/usr/bin/compton -b
nitrogen --restore
#/usr/local/bin/st -e tmux &
#emacs &
exec ssh-agent /usr/bin/stumpwm
