# ~/.bashrc: executed by bash(1) for non-login shells.
# See /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples.

# If not running interactively, don't do anything.
case $- in
    *i*) ;;
      *) return;;
esac

# Append to the history file, don't overwrite it.
shopt -s histappend

# Save multiline commands as one command.
shopt -s cmdhist

# Record each command as it is entered.
PROMPT_COMMAND='history -a'

# Set history length. See HISTSIZE and HISTFILESIZE in bash(1).
HISTSIZE=32768
HISTFILESIZE=65536

# Don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options.
HISTCONTROL="ignoreboth:erasedups"
HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history"
HISTTIMEFORMAT='%F-%T'

# # Log all commands.
# if [[ $- = *I* ]] && [[ $EUID != 0 ]]; then
#     PROMPT_COMMAND='echo $(date +"%Y%m%d-%H%M%s") $(pwd) $(history 1)" >> ~/.logs/bash-history-$(date +"%Y%m%d")'
# fi

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# Make less more friendly for non-text input files, see lesspipe(1).
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Set variable identifying the chroot you work in (used in the prompt below).
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# Set a fancy prompt (non-color, unless we know we "want" color).
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# Uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt.
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    # PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    PS1='\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\e[01;32m\]\u@\h\[\e[0m\] \[\e[01;34m\]\w\[\e[0m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
[ -f /usr/share/doc/tmux/examples/bash_completion_tmux.sh ] && source /usr/share/doc/tmux/examples/bash_completion_tmux.sh

# User customizations
export EDITOR='emacs -q -nw'
export VISUAL=emacs
export CSCOPE_EDITOR=emacsclient
export GIT_EDITOR='emacsclient -t'

# Python virtual environment setup
export WORKON_HOME=${HOME}/.virtualenvs
export PROJECT_HOME=${HOME}/Development
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3.4m
export VIRTUALENVWRAPPER_SCRIPT=$HOME/.local/bin/virtualenvwrapper.sh
source $HOME/.local/bin/virtualenvwrapper_lazy.sh

