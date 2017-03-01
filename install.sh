#!/bin/bash

STOW=/usr/bin/stow

if [ ! -x ${STOW} ]; then
    echo "Need to apt install stow"
    exit 1
fi

echo $(dirname $0)
exit 0

STOWARGS="-v -t ${HOME}"

OH_MY_ZSH_DIR=${HOME}/.oh-my-zsh
DOTFILES_DIR=${HOME}/.dotfiles

# easy_packages="bash gdb git readline tmux executables pylint x"
easy_packages="bash emacs git readline tmux executables pylint zsh"

install_easy() {
	${STOW} ${STOWARGS} ${1}
}

install_oh_my_zsh() {
    pushd ${HOME}
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    ln -sf ${DOTFILES_DIR}/oh-my-zsh-custom ${OH_MY_ZSH_DIR}/custom
}

install_tpm() {
    mkdir ${HOME}/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    tmux run-shell /home/jay/.tmux/plugins/tpm/bindings/install_plugins
}

for pkg in ${easy_packages}; do
    install_easy $pkg
done

install_oh_my_zsh
install_tpm

pushd git && cp .gitconfig.template .gitconfig ; popd


