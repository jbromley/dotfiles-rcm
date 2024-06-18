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
      inlay_hints = { enabled = false },
    },
  },
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim", -- required
      "sindrets/diffview.nvim", -- optional - Diff integration

      -- Only one of these is needed, not both.
      "nvim-telescope/telescope.nvim", -- optional
      -- "ibhagwan/fzf-lua", -- optional
    },
    config = true,
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
  {
    "nvim-orgmode/orgmode",
    event = "VeryLazy",
    ft = { "org" },
    config = function()
      -- Setup orgmode
      require("orgmode").setup({
        org_agenda_files = "~/Org/**/*",
        org_default_notes_file = "~/Org/notes.org",
        org_todo_keywords = { "TODO(t)", "IN PROGRESS(i)", "|", "DONE(d)" },
        org_hide_leading_stars = true,
      })

      -- NOTE: If you are using nvim-treesitter with ~ensure_installed = "all"~ option
      -- add ~org~ to ignore_install
      -- require('nvim-treesitter.configs').setup({
      --   ensure_installed = 'all',
      --   ignore_install = { 'org' },
      -- })
    end,
  },
}
