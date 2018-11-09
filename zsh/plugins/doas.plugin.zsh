# ------------------------------------------------------------------------------
# Description
# -----------
#
# doas will be inserted before the command
#
# ------------------------------------------------------------------------------
# Authors
# -------
#
# * Dongweiming <ciici123@gmail.com>
#
# ------------------------------------------------------------------------------

doas-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    if [[ $BUFFER == doas\ * ]]; then
        LBUFFER="${LBUFFER#doas }"
    elif [[ $BUFFER == $EDITOR\ * ]]; then
        LBUFFER="${LBUFFER#$EDITOR }"
        LBUFFER="doasedit $LBUFFER"
    else
        LBUFFER="doas $LBUFFER"
    fi
}
zle -N doas-command-line
# Defined shortcut keys: [Esc] [Esc]
bindkey "\e\e" doas-command-line
