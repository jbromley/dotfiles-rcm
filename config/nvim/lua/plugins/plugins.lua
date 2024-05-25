-- Additional plugins and plugin configurations

return {
  {
    "benknoble/vim-racket",
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        racket_langserver = {
          cmd = { "racket", "--lib", "racket-langserver" },
          filetypes = { "racket", "scheme" },
        },
      },
    },
  },
  {
    "nvim-neorg/neorg",
    version = "v7.0.0",
    dependencies = { "nvim-lua/plenary.nvim" },
    build = ":Neorg sync-parsers",
    -- tag = "*",
    lazy = true,
    ft = "norg",
    cmd = "Neorg",
    config = function()
      require("neorg").setup({
        load = {
          ["core.defaults"] = {},
          ["core.concealer"] = { config = { icons = { todo = { undone = { icon = " " } } } } },
          ["core.dirman"] = {
            config = {
              workspaces = {
                notes = "~/Documents/notes",
              },
            },
          },
        },
      })
    end,
  },
}
