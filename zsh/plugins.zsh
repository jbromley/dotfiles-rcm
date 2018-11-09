if [ "$(uname -s)" = "OpenBSD" ]; then
    plugins=(doas \
		 web-search \
		 zsh-history-substring-search)
else
    plugins=(sudo \
		 colored-man-pages \
		 web-search \
		 zsh-history-substring-search \
		 kubectl \
		 lein)
fi

for plugin in $plugins; do
    if [ -f ${HOME}/.zsh/plugins/${plugin}.plugin.zsh ]; then
	source ${HOME}/.zsh/plugins/${plugin}.plugin.zsh
    else
	source ${HOME}/.zsh/plugins/${plugin}.zsh
    fi
done
