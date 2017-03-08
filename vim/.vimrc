set rtp+=/usr/share/vim/addons
filetype plugin indent on

"" Set up a status line.
set statusline=%f%m%r%h%w\ [%l/%L\ %c\ (%p%%)]
set laststatus=2

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

" Turn on smart indent.
set expandtab
set autoindent
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

