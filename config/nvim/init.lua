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
local paq = require('paq-nvim').paq
paq {'savq/paq-nvim', opt = true}
paq {'junegunn/fzf', run = fn['fzf#install']}
paq {'junegunn/fzf.vim'}
paq {'nvim-treesitter/nvim-treesitter'}
paq {'neovim/nvim-lspconfig'}
paq {'kosayoda/nvim-lightbulb'}
paq {'hrsh7th/nvim-compe'}
paq {'elixir-editors/vim-elixir'}
paq {'wlangstroth/vim-racket'}
paq {'kovisoft/paredit'}
paq {'Olical/conjure'}
paq {'tpope/vim-commentary'}
paq {'tpope/vim-fugitive'}
paq {'vimwiki/vimwiki'}
paq {'kyazdani42/nvim-web-devicons', opt = true}
paq {'hoob3rt/lualine.nvim'}
paq {'dracula/vim', as='dracula'}
-- }}}

-- Options {{{

-- Buffer options
cmd 'colorscheme dracula'

local indent = 4
vim.bo.tabstop = indent
vim.bo.shiftwidth = indent
vim.bo.expandtab = true
vim.bo.autoindent = true
vim.bo.smartindent = true
vim.bo.textwidth = 80

-- Global options
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

-- local s = ' %f %m %r %{FugitiveStatusline()} %=' ..
--           '%y %{&fileencoding?&fileencoding:&encoding} [%{&fileformat}] ' ..
--           '%l:%c (%P)'
-- opt('o', 'statusline', s)

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
local ts = require 'nvim-treesitter.configs'
ts.setup {ensure_installed = 'maintained', highlight = {enable = true}}
-- }}}

-- VimWiki {{{
g['vimwiki_list'] = {{path = '~/Documents/Wiki'}}
-- }}}

-- LSP {{{
local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys 
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', '<Leader>,', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', '<Leader>.', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<Leader>a', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', '<Leader>d', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap("n", "<Leader>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  buf_set_keymap('n', '<Leader>h', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', '<Leader>m', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<Leader>r', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<Leader>s', '<cmd>lua vim.lsp.buf.document_symbol()<CR>', opts)
  buf_set_keymap('n', '<Leader>i', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<Leader>l', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)

  -- buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  -- buf_set_keymap('n', '<Leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  -- buf_set_keymap('n', '<Leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  -- buf_set_keymap('n', '<Leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  -- buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  -- buf_set_keymap('n', '<Leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  -- buf_set_keymap('n', '<Leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)

end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
      -- signs = false,
      virtual_text = false,
  }
)
  
-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = { "bashls", "clangd", "gopls", "racket_langserver" }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup { on_attach = on_attach }
end
-- }}}

-- Compe {{{
require'compe'.setup {
    enabled = true;
    autocomplete = true;
    debug = false;
    min_length = 1;
    preselect = 'enable';
    throttle_time = 80;
    source_timeout = 200;
    incomplete_delay = 400;
    max_abbr_width = 100;
    max_kind_width = 100;
    max_menu_width = 100;
    documentation = true;

    source = {
        path = true;
        nvim_lsp = true;
    }
}

local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    return col == 0 or vim.fn.getline(','):sub(col, col):match('%s')
end

_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  else
    return t "<S-Tab>"
  end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
-- }}}

-- lualine {{{
require('lualine').setup {
  options = {
    icons_enabled = false,
    theme = 'dracula',
  },
}
  
-- }}}

-- }}}

-- Commands {{{
-- Set comment strings.
cmd 'autocmd FileType c,cpp,cs,java setlocal commentstring=//\\ %s'
cmd 'autocmd FileType rkt,scm setlocal commentstring=;\\ %s'

-- Show lightbulb when code actions are available.
cmd [[autocmd CursorHold,CursorHoldI * lua require('nvim-lightbulb').update_lightbulb()]]
-- }}}

-- vim: foldmethod=marker:foldlevel=10
