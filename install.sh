#!/bin/bash
STOW=/usr/bin/stow
if [ ! -x ${STOW} ]; then
    echo "Need to apt install stow"
    exit 1
fi
STOWARGS="-v -t ${HOME}"

VIM=/usr/bin/vim
VIM_DIR=${HOME}/.vim
VUNDLE_DIR=${VIM_DIR}/bundle/Vundle.vim 

ZGEN_DIR=${HOME}/.zgen
DOTFILES_DIR=${HOME}/.dotfiles

# easy_packages="bash gdb git readline tmux executables pylint x"
easy_packages="bash git tmux x11 zsh"

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

install_vim() {
    install_easy vim_full
    if [ -d ${VUNDLE_DIR} ]; then
        pushd ${VUNDLE_DIR}
        git pull
        popd
    else
        git clone https://github.com/VundleVim/Vundle.vim.git ${VUNDLE_DIR}
    fi  
    ${VIM} +PluginInstall +qall
    pushd ${VIM_DIR}/bundle/YouCompleteMe
    ./install.py --clang-completer
    popd
}

for pkg in ${easy_packages}; do
    install_easy $pkg
done

install_zgen
install_tpm

# pushd git && cp .gitconfig.template .gitconfig ; popd

if [ -x "$(which vim)" -a -x "$(which git)" ] && vim --version | grep -q +python; then
    echo "Installing complete vim configuration."
    install_vim
else
    echo "Installing simple vim configuration."
    install_easy vim
fi


