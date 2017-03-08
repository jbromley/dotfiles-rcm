set nocompatible
filetype off

" Set the runtime path to include Vundle and initialize.
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/syntastic'
Plugin 'Valloric/YouCompleteMe'
Plugin 'tpope/vim-sensible'

" Color schemes
Plugin 'tyrannicaltoucan/vim-quantum'
Plugin 'ciaranm/inkpot'

call vundle#end()
filetype plugin indent on

"" Set up a status line.
set statusline=%f%m%r%h%w\ [%l/%L\ %c\ (%p%%)]
set laststatus=2

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_checkers = ['flake8']

" YouCompleteMe
let g:ycm_global_ycm_extra_conf = '~/.vim/.ycm_extra_conf.py'
" let g:ycm_python_binary_path = '/usr/bin/python3.5'

" Turn on smart indent.
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

" Turn on incremental search with ignore case (except for explicit caps.
set ignorecase
set smartcase

" Set color scheme
set t_Co=256
let g:quantum_black=1
let g:quantum_italic=1
set background=dark
colorscheme quantum
syntax enable

" Put a margin on screen.
highlight OverLength ctermbg=DarkMagenta ctermfg=white guibg=#592929
match OverLength /\%>80v.\+/
set colorcolumn=+1
highlight ColorColumn ctermbg=DarkMagenta

" Configure folding. Space toggles a fold.
set foldenable
set foldmethod=syntax
nnoremap <space> za

" XML folding
let g:xml_syntax_folding=1
autocmd FileType xml setlocal foldmethod=syntax 

" Autowrap text for Markdown documents.
autocmd BufRead,BufNewFile *.md setlocal textwidth=80

