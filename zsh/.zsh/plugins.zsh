plugins=(sudo colored-man-pages virtualenv virtualenvwrapper web-search zsh-history-substring-search)

for plugin in $plugins; do
    if [ -f ${HOME}/.zsh/plugins/${plugin}.plugin.zsh ]; then
	source ${HOME}/.zsh/plugins/${plugin}.plugin.zsh
    else
	source ${HOME}/.zsh/plugins/${plugin}.zsh
    fi
done
