-- Aliases and helpers {{{
local cmd = vim.cmd
local fn = vim.fn
local g = vim.g

-- Set up keymaps
local function map(mode, lhs, rhs, opts)
    local options = {noremap = true}
    if opts then
        options = vim.tbl_extend('force', options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end
-- }}}

-- Plugin installation {{{
cmd 'packadd paq-nvim'

local paq = require("paq")

paq {
	{'savq/paq-nvim', opt = true};
	{'junegunn/fzf', run = fn['fzf#install']};
	'junegunn/fzf.vim';
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
	{'saadparwaiz1/cmp_luasnip'};
	{'hrsh7th/cmp-nvim-lsp'};
	{'hrsh7th/cmp-nvim-lua'};

	-- Snippets
	{'L3MON4D3/LuaSnip'};
	{'rafamadriz/friendly-snippets'};

	'elixir-editors/vim-elixir';
	'tpope/vim-commentary';
	'jreybert/vimagit';
	'vimwiki/vimwiki';
	'hoob3rt/lualine.nvim';

    'lifepillar/vim-solarized8';
}

-- cmd 'packadd! dracula_pro'

require('mason').setup()
require('mason-lspconfig').setup()

-- }}}

-- Options {{{

-- cmd 'colorscheme dracula_pro'

-- Global options
g.mapleader = '\\'
g.maplocalleader = ','

-- Global options
vim.o.guifont = 'JetBrains Mono:h11'
vim.o.hidden = true
vim.o.joinspaces = false
vim.o.scrolloff = 4
vim.o.sidescrolloff = 8
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.termguicolors =  true
vim.o.wildmode = 'longest:full'
vim.o.wildmenu = true

vim.o.clipboard = vim.o.clipboard .. 'unnamedplus'
vim.o.autowriteall = true
vim.o.history = 1024
vim.o.listchars = 'tab:▸ ,extends:❯,precedes:❮,nbsp:±'

vim.o.modelines = 10
vim.o.lazyredraw = true
vim.o.showmatch = true

-- Buffer options
local indent = 4
vim.bo.tabstop = indent
vim.bo.shiftwidth = indent
vim.bo.expandtab = true
vim.bo.autoindent = true
vim.bo.smartindent = true
vim.bo.textwidth = 80

-- Window options
vim.wo.number = true
vim.wo.wrap = false
vim.wo.colorcolumn = '+1'

-- Folding
vim.o.foldlevelstart = 10
vim.wo.foldmethod = 'indent'
vim.wo.foldnestmax = 10
-- }}}

-- Key mappings {{{

-- fzf
map('n', ';', '<cmd>Buffers<CR>')
map('n', '<Leader>f', '<cmd>Files<CR>')
map('n', '<Leader>a', '<cmd>Ag<CR>')
map('n', '<Leader>t', '<cmd>Tags<CR>')

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
-- require('lualine').setup { options = { icons_enabled = false, theme = 'dracula', }, }
require('lualine').setup { options = { icons_enabled = false, theme = 'molokai', }, }
-- }}}

-- }}}

-- Commands {{{
-- Set comment strings.
cmd 'autocmd FileType c,cpp,cs,java setlocal commentstring=//\\ %s'
cmd 'autocmd FileType rkt,scm setlocal commentstring=;\\ %s'
-- }}}

-- vim: foldmethod=marker:foldlevel=10
