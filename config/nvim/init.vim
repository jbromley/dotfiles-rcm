""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set encoding=utf-8
set fileencoding=utf-8
syntax on
set ttyfast
set lazyredraw
" set cursorline

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins list
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin()

" NERDTree
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'majutsushi/tagbar'

" Java development
Plug 'sbdchd/neoformat'
Plug 'artur-shaik/vim-javacomplete2'
Plug 'neomake/neomake'

" TypeScript development
Plug 'HerringtonDarkholme/yats.vim'
Plug 'mhartington/nvim-typescript', {'do': './install.sh'}

" Autocomplete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'Shougo/denite.vim', {'do': 'UpdateRemotePlugins'}

" FuZzy Find
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" TComment
Plug 'tomtom/tcomment_vim'

" Color schemes
Plug 'treycucco/vim-monotonic'
Plug 'wolverian/minimal'

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Neomake async hooks
call neomake#configure#automake('w')

" NERDTree
let NERDTreeShowHidden=1
map <silent> <C-n> :NERDTreeToggle<CR>
let g:NERDTreeQuitOnOpen=1

" Make fzf respect ag
if (executable('ag'))
    let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -g ""'
endif

" Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#omni_patterns = {}
let g:deoplete#omni_patterns.java = '[^. *\t]\.\w*'
let g:deoplete#sources = {}
let g:deoplete#sources._ = []
let g:deoplete#file#enable_buffer_path = 1

" Java completion
autocmd FileType java setlocal omnifunc=javacomplete#Complete

" neomake
autocmd! BufWritePost,BufEnter * Neomake

" neoformat
augroup astyle
    autocmd!
    autocmd BufWritePre * Neoformat
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Visual configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set t_Co=256
" colorscheme
set nowrap
" set relativenumber
set number
set laststatus=2
set invlist
set list
set listchars=tab:¦\ ,eol:¬,trail:⋅,extends:❯,precedes:❮

" Disable scroll bars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Don't use arrow keys

nnoremap <C-P> :Files<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Indentation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set ai
set si

