# Documentation: https://github.com/romkatv/zsh4humans/blob/v2/README.md.

# Export XDG environment variables. Other environment variables are exported later.
export XDG_CACHE_HOME="$HOME/.cache"

# URL of zsh4humans repository. Used during initial installation and updates.
Z4H_URL="https://raw.githubusercontent.com/romkatv/zsh4humans/v2"

# Cache directory. Gets recreated if deleted. If already set, must not be changed.
: "${Z4H:=${XDG_CACHE_HOME:-$HOME/.cache}/zsh4humans}"

# Do not create world-writable files by default.
umask o-w

# Fetch z4h.zsh if it doesn't exist yet.
if [ ! -e "$Z4H"/z4h.zsh ]; then
  mkdir -p -- "$Z4H" || return
  >&2 printf '\033[33mz4h\033[0m: fetching \033[4mz4h.zsh\033[0m\n'
  if command -v curl >/dev/null 2>&1; then
    curl -fsSL -- "$Z4H_URL"/z4h.zsh >"$Z4H"/z4h.zsh.$$ || return
  else
    wget -O-   -- "$Z4H_URL"/z4h.zsh >"$Z4H"/z4h.zsh.$$ || return
  fi
  mv -- "$Z4H"/z4h.zsh.$$ "$Z4H"/z4h.zsh || return
fi

# Code prior to this line should not assume the current shell is Zsh.
# Afterwards we are in Zsh.
. "$Z4H"/z4h.zsh || return

# 'ask': ask to update; 'no': disable auto-update.
zstyle ':z4h:' auto-update                     ask
# Auto-update this often; has no effect if auto-update is 'no'.
zstyle ':z4h:'                auto-update-days 28
# Stability vs freshness of plugins: stable, testing or dev.
zstyle ':z4h:*'               channel          stable
# Bind alt-arrows or ctrl-arrows to change current directory?
# The other key modifier will be bound to cursor movement by words.
zstyle ':z4h:'                cd-key           alt
# Right-arrow key accepts one character ('partial-accept') from
# command autosuggestions or the whole thing ('accept')?
zstyle ':z4h:autosuggestions' forward-char     partial-accept

if (( UID && UID == EUID && ! Z4H_SSH )); then
  # When logged in as a regular user and not via `z4h ssh`, check that
  # login shell is zsh and offer to change it if it isn't.
  z4h chsh
fi

# Clone additional Git repositories from GitHub. This doesn't do anything
# apart from cloning the repository and keeping it up-to-date. Cloned
# files can be used after `z4h init`.
#
# This is just an example. If you don't plan to use Oh My Zsh, delete this.
# z4h install ohmyzsh/ohmyzsh || return

# Install or update core components (fzf, zsh-autosuggestions, etc.) and
# initialize Zsh. After this point console I/O is unavailable. Everything
# that requires user interaction or can perform network I/O must be done
# above. Everything else is best done below.
z4h init || return

# Enable emacs (-e) or vi (-v) keymap.
bindkey -e


# Extend PATH.
# path=(~/bin $path)

# Use additional Git repositories pulled in with `z4h install`.
#
# This is just an example that you should delete. It doesn't do anything useful.
# z4h source $Z4H/ohmyzsh/ohmyzsh/lib/diagnostics.zsh
# z4h source $Z4H/ohmyzsh/ohmyzsh/plugins/emoji-clock/emoji-clock.plugin.zsh
# fpath+=($Z4H/ohmyzsh/ohmyzsh/plugins/supervisor)

# Source additional local files.
if [[ $LC_TERMINAL == iTerm2 ]]; then
  # Enable iTerm2 shell integration (if installed).
  z4h source ~/.iterm2_shell_integration.zsh
fi

# Define key bindings.
bindkey -M emacs '^H' backward-kill-word # Ctrl-H and Ctrl-Backspace: Delete previous word.

# Sort completion candidates when pressing Tab?
zstyle ':completion:*'                           sort               false
# Should cursor go to the end when Up/Down/Ctrl-Up/Ctrl-Down fetches a command from history?
zstyle ':zle:(up|down)-line-or-beginning-search' leave-cursor       no
# When presented with the list of choices upon hitting Tab, accept selection and
# trigger another completion with this key binding. Great for completing file paths.
zstyle ':fzf-tab:*'                              continuous-trigger tab

# Autoload functions.
autoload -Uz zmv

# Define functions and completions.
function md() { [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1" }
compdef _directories md

# Set shell options: http://zsh.sourceforge.net/Doc/Release/Options.html.
# setopt glob_dots  # glob matches files starting with dot; `ls *` becomes equivalent to `ls *(D)`
setopt correct_all

#
# Zsh environment customizations
#

# Configure the path. On MacOS user path_helper.
typeset -Ux PATH path
path=(${HOME}/.local/bin $path)
if [ -d /usr/local/cuda/bin ]; then
    path+=(/usr/local/cuda/bin)
fi
if [ -d /opt/local/bin ]; then
    path+=(/opt/local/bin)
fi

export LANG=en_US.UTF-8
export EDITOR=emacs
export GPG_TTY=$TTY

# Set up our editor.
export EDITOR="emacsclient -cn"
export GIT_EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=""

if [ $(uname) = "Linux" ]; then
    # Java settings (Linux only)
    # export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'

    # Set up stuff for snap applications.
    [ -f /etc/profile.d/apps-bin-path.sh ] && emulate sh -c  'source /etc/profile.d/apps-bin-path.sh'
fi

if [ $(uname) = "Darwin" ]; then
    export FZF_BASE=/opt/local/share/fzf/
fi

# Set up the Node Version Manager.
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Don't let Python venv add anything to the prompt.
export VIRTUAL_ENV_DISABLE_PROMPT=yes

#
# Command aliases
#

alias tree='tree -a -I .git'

# Colorize output, add file type indicator, and put sizes in human readable format
[ $(uname) = "Darwin" ] && alias ls='ls -F -G' || alias ls='ls --color=auto --classify'
alias lsa='ls -la'
alias l='ls -lah'
alias ll='ls -lh'
alias la='ls -lAh'
# alias ls="${aliases[ls]:-ls} -A"

# On Linux use some color options for grep
[ "$(uname -s)" = "Linux" ] && alias grep='grep --color=auto'

# Don't correct some things.
alias man='nocorrect man'
alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'
alias ssh='nocorrect ssh'
alias sudo='nocorrect sudo'
alias tmux='nocorrect tmux'
alias z4h='nocorrect z4h'

# Directory aliases
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias -- -='cd -'
alias 1='cd -'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

alias d='dirs -v | head -10'

# Emacs, terminal emacsclient, and windowed emacsclient
alias e=emacs
alias et='emacsclient -t'
alias ew='emacsclient -cn'

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Open files easily with desktop tools
alias o='xdg-open'


#
# Functions
#

function venv() { source "${HOME}/.venv/$1/bin/activate" }
