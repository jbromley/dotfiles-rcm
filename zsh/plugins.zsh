plugins=(sudo \
	     colored-man-pages \
	     web-search \
	     zsh-history-substring-search \
	     kubectl \
	     lein)

for plugin in $plugins; do
    if [ -f ${HOME}/.zsh/plugins/${plugin}.plugin.zsh ]; then
	source ${HOME}/.zsh/plugins/${plugin}.plugin.zsh
    else
	source ${HOME}/.zsh/plugins/${plugin}.zsh
    fi
done
