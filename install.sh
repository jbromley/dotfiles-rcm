#!/bin/bash
STOW=/usr/bin/stow
if [ ! -x ${STOW} ]; then
    echo "Need to apt install stow"
    exit 1
fi
STOWARGS="-v -t ${HOME}"

ZGEN_DIR=${HOME}/.zgen
DOTFILES_DIR=${HOME}/.dotfiles

# easy_packages="bash gdb git readline tmux executables pylint x"
easy_packages="emacs git tmux x11 zsh misc"

install_easy() {
        ${STOW} ${STOWARGS} ${1}
}

install_zgen() {
    git clone https://github.com/tarjoilija/zgen.git "${ZGEN_DIR}"
}

install_tpm() {
    mkdir -p ${HOME}/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    tmux run-shell /home/jay/.tmux/plugins/tpm/bindings/install_plugins
}

for pkg in ${easy_packages}; do
    install_easy $pkg
done

install_zgen
install_tpm

# pushd git && cp .gitconfig.template .gitconfig ; popd

