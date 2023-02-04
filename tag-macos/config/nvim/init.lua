-- Plugin installation {{{
vim = vim
local g = vim.g
local o = vim.o
local bo = vim.bo
local wo = vim.wo
local cmd = vim.cmd
local keymap = vim.keymap

cmd 'packadd paq-nvim'

require 'paq' {
    {'savq/paq-nvim', opt = true};
    -- {'junegunn/fzf', run = fn['fzf#install']};
    -- 'junegunn/fzf.vim';
    {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'};

    {'VonHeikemen/lsp-zero.nvim'};

    -- LSP Support
    {'williamboman/mason.nvim'};
    {'williamboman/mason-lspconfig.nvim'};
    {'neovim/nvim-lspconfig'};

    -- Autocompletion
    {'hrsh7th/nvim-cmp'};
    {'hrsh7th/cmp-buffer'};
    {'hrsh7th/cmp-path'};
    -- {'saadparwaiz1/cmp_luasnip'};
    {'hrsh7th/cmp-nvim-lsp'};
    {'hrsh7th/cmp-nvim-lua'};

    -- Snippets
    {'L3MON4D3/LuaSnip'};
    -- {'rafamadriz/friendly-snippets'};

    -- Telescope
    {'nvim-lua/plenary.nvim'};
    {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'};
    {'nvim-telescope/telescope.nvim'};

    -- Terminal
    {"akinsho/toggleterm.nvim", tag = '*'};
    'elixir-editors/vim-elixir';
    'tpope/vim-commentary';
    'tpope/vim-fugitive';
    'vimwiki/vimwiki';
    'hoob3rt/lualine.nvim';

    -- Themes
    'lifepillar/vim-solarized8';
}

cmd 'packadd! dracula_pro'

require('mason').setup()
require('mason-lspconfig').setup()
require('toggleterm').setup{
    open_mapping = [[<c-\>]],
    direction = 'float',
    float_opts = {
        width = 100,
        height = 50
    },
}
require('telescope').load_extension('fzf')

-- }}}

-- Options {{{

-- Theme and background
cmd [[silent! colorscheme dracula_pro]]

cmd([[highlight Pmenu ctermbg=Black guibg=Black ctermfg=LightGray guifg=LightGray]])
cmd([[highlight PmenuSel ctermbg=Gray guibg=Gray ctermfg=White guifg=White]])

-- Global options
g.mapleader = '\\'
g.maplocalleader = ','

-- Global options
o.guifont = 'JetBrains Mono NL:h11'
o.hidden = true
o.joinspaces = false
o.scrolloff = 4
o.sidescrolloff = 8
o.splitbelow = true
o.splitright = true
o.termguicolors =  true
o.wildmode = 'longest:full'
o.wildmenu = true

o.clipboard = o.clipboard .. 'unnamedplus'
o.autowriteall = true
o.history = 1024
o.listchars = 'tab:▸ ,extends:❯,precedes:❮,nbsp:±'

o.modelines = 10
o.lazyredraw = true
o.showmatch = true

-- Buffer options
local indent = 4
bo.tabstop = indent
bo.shiftwidth = indent
bo.expandtab = true
bo.autoindent = true
bo.smartindent = true
bo.textwidth = 80

-- Window options
wo.number = true
wo.wrap = false
wo.colorcolumn = '+1'

-- Folding
o.foldlevelstart = 10
wo.foldmethod = 'indent'
wo.foldnestmax = 10
-- }}}

-- Key mappings {{{

-- telescope
local builtin = require('telescope.builtin')
keymap.set('n', '<leader>ff', builtin.find_files, {})
keymap.set('n', '<leader>fg', builtin.live_grep, {})
keymap.set('n', '<leader>fb', builtin.buffers, {})
keymap.set('n', '<leader>fh', builtin.help_tags, {})

-- }}}

-- Plugin configuration {{{

-- Treesitter {{{
require 'nvim-treesitter.configs'.setup {
        ensure_installed = 'all',
        highlight = {
                enable = true,
        }
}
-- }}}

-- VimWiki {{{
g['vimwiki_list'] = {{path = '~/Documents/Wiki'}}
-- }}}

-- LSP {{{
local lsp = require('lsp-zero')
lsp.preset('recommended')
lsp.setup()
-- }}}

-- lualine {{{
require('lualine').setup { options = { icons_enabled = false, theme = 'dracula', }, }
-- }}}

-- }}}

-- Commands {{{
-- Set comment strings.
cmd 'autocmd FileType c,cpp,cs,java setlocal commentstring=//\\ %s'
cmd 'autocmd FileType rkt,scm setlocal commentstring=;\\ %s'
-- }}}

-- vim: foldmethod=marker:foldlevel=10
