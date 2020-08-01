" Neovim configuration

" Options {{{
set splitbelow splitright
set clipboard+=unnamedplus

" Tab settings
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoindent

" Ignore case when searching unless a capital letter is present.
set ignorecase smartcase

" File and script encoding settings.
set fileencoding=utf-8
set fileencodings=ucs-bom,utf-8,latin1
scriptencoding utf-8

" Ignore certain files when globbing.
set wildignore+=*.o,*.obj
set wildignore+=*/.git/*,*/.svn/*,*/__pycache__/,*/build/**
set wildignore+=*.pyc
set wildignore+=.DS_Store
set wildignore+=*.aux,*.bbl,*.blg,*.brf,*.fls

" Searching 
set path+=**
set ignorecase          " ignore case when searching
set incsearch           " search as characters are entered
set hlsearch            " highlight all matches

" Folding
" set foldlevel=0
set foldmethod=indent   " fold based on indent level
set foldnestmax=10      " max 10 depth
set foldenable          " don't fold files by default on open
set foldlevelstart=10   " start with fold level of 1

"Size of command history
set history=500

" Use list mode and customized listchars
if has('multi_byte') && &encoding ==# 'utf-8'
    let &listchars='tab:▸ ,extends:❯,precedes:❮,nbsp:±'
else
    let &listchars='tab:> ,extends:>,precedes:<,nbsp:.'
endif

" Backups
set backup
set backupdir=~/.cache/nvim/tmp,/var/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.cache/nvim/tmp,/var/tmp
set writebackup

filetype plugin indent on
syntax on

" If using the fish shell, spawn things with bash instead.
if &shell =~# 'fish$'
    set shell=/usr/bin/bash
endif
" }}}

" Plugins {{{
call plug#begin()

" FZF 
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" NERDTree - file tree navigation
Plug 'preservim/nerdtree'

" Git plugin
Plug 'tpope/vim-fugitive'

" Database interaction
Plug 'tpope/vim-dadbod'

" VimWiki
Plug 'vimwiki/vimwiki'

" Unicode
Plug 'chrisbra/unicode.vim'

" Commentary
Plug 'tpope/vim-commentary'

" coc.nvim
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" coc-elixir
Plug 'elixir-lsp/coc-elixir', {'do': 'yarn install && yarn prepack'}

" ALE
" Plug 'dense-analysis/ale'

" Elixir
" Plug 'elixir-editors/vim-elixir'
call plug#end()
"}}}

" Plugin configuration and key bindings {{{

" Key bindings for fzf
nmap ; :Buffers<CR>
nmap <Leader>t :Tags<CR>
nmap <Leader>f :Files<CR>
nmap <Leader>a :Ag<CR>

" NerdTree configuration
map <F2> :NERDTreeToggle<CR>
let g:NERDTreeIgnore=['\~$', '__pycache__', '.git']

" VimWiki
let g:vimwiki_list = [{'path': '~/Documents/VimWiki/'}]

" Coc
" Use Tab to trigger completion.
inoremap <silent><expr> <Tab> 
    \ pumvisible() ? "\<C-n>" :
    \ <SID>check_back_space() ? "\<Tab>" :
    \ coc#refresh()
inoremap <expr><S-Tab> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1] =~# '\s'
endfunction

" Use <C-Space> to trigger completion.
inoremap <silent><expr> <C-Space> coc#refresh()

" Use <CR> to confirm completion.
if exists('*complete_info')
    inoremap <expr> <CR> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
    inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use [g and ]g to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
" xmap <leader>f  <Plug>(coc-format-selected)
" nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
" xmap <leader>a  <Plug>(coc-codeaction-selected)
" nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
" nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
" nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
" xmap if <Plug>(coc-funcobj-i)
" omap if <Plug>(coc-funcobj-i)
" xmap af <Plug>(coc-funcobj-a)
" omap af <Plug>(coc-funcobj-a)
" xmap ic <Plug>(coc-classobj-i)
" omap ic <Plug>(coc-classobj-i)
" xmap ac <Plug>(coc-classobj-a)
" omap ac <Plug>(coc-classobj-a)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
" nmap <silent> <C-s> <Plug>(coc-range-select)
" xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
" command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
" command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
" command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

" ALE
" let g:ale_linters= {'elixir': ['elixir-ls'], \ 'python': ['pylint', 'flake8', 'mypy']}
" let g:ale_fixers = {'elixir': ['mix_format'], }
" let g:ale_elixir_elixir_ls_relase='/opt/elixir-ls'
" let g:ale_completion_enabled = 1
" let g:ale_sign_error = '✘'
" let g:ale_sign_warning = '⚠'
" let g:ale_lint_on_enter = 0
" let g:ale_lint_on_text_changed = 'never'
" highlight ALEErrorSign ctermbg=NONE ctermfg=red
" highlight ALEWarningSign ctermbg=NONE ctermfg=yellow
" let g:ale_linters_explicit = 1
" let g:ale_lint_on_save = 1
" let g:ale_fix_on_save = 1

" noremap <Leader>ad :ALEGoToDefinition<CR>
" nnoremap <leader>af :ALEFix<CR>
" noremap <Leader>ar :ALEFindReferences<CR>

"Move between linting errors
" nnoremap ]r :ALENextWrap<CR>
" nnoremap [r :ALEPreviousWrap<CR>
" }}}

" Functions{{{
function! StatuslineMode()
  let l:mode=mode()
  if l:mode==#"n"
    return "NORMAL"
  elseif l:mode==?"v"
    return "VISUAL"
  elseif l:mode==#"i"
    return "INSERT"
  elseif l:mode==#"R"
    return "REPLACE"
  elseif l:mode==?"s"
    return "SELECT"
  elseif l:mode==#"t"
    return "TERMINAL"
  elseif l:mode==#"c"
    return "COMMAND"
  elseif l:mode==#"!"
    return "SHELL"
  endif
endfunction

" function! LinterStatus() abort
"     let l:counts = ale#statusline#Count(bufnr(''))
"     let l:all_errors = l:counts.error + l:counts.style_error
"     let l:all_non_errors = l:counts.total - l:all_errors
"     return l:counts.total == 0 ? 'OK' : printf('%dW %dE', all_non_errors, all_errors)
" endfunction
" }}}

" Autocommands {{{ 
augroup dynamic_smartcase
    autocmd!
    autocmd CmdLineEnter : set nosmartcase
    autocmd CmdLineLeave : set smartcase
augroup END

augroup cursorline_active_window
    autocmd!
    autocmd WinEnter * set cursorline
    autocmd WinLeave * set nocursorline
augroup END

" Set up Python buffers
augroup python_buffer 
    autocmd!
    " autocmd BufRead,BufNewFile *.py setlocal tabstop=2 shiftwidth=2 softtabstop=2 signcolumn=yes
    " autocmd BufRead,BufNewFile *.py setlocal signcolumn=yes
    autocmd FileType python setlocal signcolumn=yes
    " autocmd BufRead,BufNewFile *.py nnoremap <Leader>b V:s/[,)]/&\r/g <cr>='<
    autocmd FileType python nnoremap <Leader>b V:s/[,)]/&\r/g <cr>='<
    autocmd BufWritePre *.py %s/\s\+$//e
augroup END
"}}}

" Key bindings {{{
" Turn word under cursor to upper case.
inoremap <silent> <C-u> <Esc>viwUea

" Turn current word into title case.
inoremap <silent> <C-t> <Esc>b~lea

" Location and quickfix list navigation
nnoremap [l :lprevious<CR>zv
nnoremap ]l :lnext<CR>zv
nnoremap [L :lfirst<CR>zv
nnoremap ]L :llast<CR>zv
nnoremap [q :cprevious<CR>zv
nnoremap ]q :cnext<CR>zv
nnoremap [Q :cfirst<CR>zv
nnoremap ]Q :clast<CR>zv

" Close location or quickfix list if present.
nnoremap <silent> <Leader>x :windo lclose <bar> cclose<CR>

" Close a buffer and switch to other buffer.
nnoremap <silent> <Leader>d :bprevious <bar> bdelete #<CR>

" Toggle search highlight.
nnoremap <silent><expr> <Leader>hl (&hls && v:hlsearch ? ':nohls' : ':set hls')."\n"

" Insert a space after current character.
nnoremap <silent> <Space><Space> a<Space><Esc>h

" Decrease indent level in insert mode.
inoremap <S-Tab> <Esc><<i
"}}}

" UI {{{
" Modeline
set modeline
set modelines=1

set number
set showcmd
set wildmenu
set lazyredraw
set showmatch

" Show marker at text width
set textwidth=80
set colorcolumn=+1

" Status line configuration
set statusline=\ %n\ 
set statusline+=%1*\ %f\ %m\ %r\ 
set statusline+=%2*\ %{FugitiveStatusline()}\  
set statusline+=%9*\ "%{StatuslineMode()}
set statusline+=%=
" set statusline+=%{coc#status()}%{get(b:,'coc_current_function','')}
set statusline+=%{coc#status()}
set statusline+=%#warningmsg#
set statusline+=%*
set statusline+=%y\ %{&fileencoding?&fileencoding:&encoding}\ [%{&fileformat}]\ 
set statusline+=%2*\ Column\ %c\ 
set statusline+=%1*\ Line\ %l/%L\ (%P)
set laststatus=2
hi User1 ctermbg=darkblue ctermfg=white guibg=darkblue guifg=white
hi User2 ctermbg=magenta ctermfg=white guibg=magenta guifg=white
hi User9 ctermbg=black ctermfg=white guibg=black guifg=white
"}}}


" vim: foldmethod=marker:foldlevel=0
