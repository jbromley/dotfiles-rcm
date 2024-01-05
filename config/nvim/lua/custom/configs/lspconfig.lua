local on_attach = require("plugins.configs.lspconfig").on_attach
local capabilities = require("plugins.configs.lspconfig").capabilities

local lspconfig = require "lspconfig"

-- if you just want default config for the servers then put them in a table
local servers = { "clangd", "erlangls", "gleam"}

for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
  }
end

-- 
-- lspconfig.pyright.setup { blabla}

lspconfig.elixirls.setup {
  -- cmd = { "/home/jay/.local/share/rtx/installs/elixir-ls/0.18.1/language_server.sh" },
  cmd = { "/home/jay/.local/share/nvim/mason/packages/elixir-ls/language_server.sh", };
  on_attach = on_attach,
  capabilities = capabilities,
}

lspconfig.racket_langserver.setup { }
