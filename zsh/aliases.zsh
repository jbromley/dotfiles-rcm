# Colorize output, add file type indicator, and put sizes in human readable format
[ $(uname) = "Darwin" ] && alias ls='ls -F' || alias ls='ls --color -F'
alias lsa='ls -lahF'
alias l='ls -lahF'
alias ll='ls -lhF'
alias la='ls -lAhF'

# On Linux use some color options.
if [ "$(uname -s)" = "Linux" ]; then
    alias ls='ls --color=auto -F'
    alias grep='grep --color=auto'
fi

# Don't correct some things.
alias ebuild='nocorrect ebuild'
alias gist='nocorrect gist'
alias heroku='nocorrect heroku'
alias hpodder='nocorrect hpodder'
alias man='nocorrect man'
alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'
alias mysql='nocorrect mysql'
alias sudo='nocorrect sudo'
alias doas='nocorrect doas'

setopt correct_all

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

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Preview with fzf and bash.
alias preview="fzf --preview 'bat --color \"always\" {}'"
export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(emacsclient {})+abort'"
