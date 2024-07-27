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

      local wk = require("which-key")
      wk.add({
        { "<leader>o", group = "Org mode" },
        { "<leader>ob", group = "org tangle" },
        { "<leader>od", group = "org date" },
        { "<leader>oi", group = "org insert" },
        { "<leader>ol", group = "org link" },
        { "<leader>on", group = "org note" },
        { "<leader>ox", group = "org clock" },
      })

      -- NOTE: If you are using nvim-treesitter with ~ensure_installed = "all"~ option
      -- add ~org~ to ignore_install
      -- require('nvim-treesitter.configs').setup({
      --   ensure_installed = 'all',
      --   ignore_install = { 'org' },
      -- })
    end,
  },
  {
    "m4xshen/hardtime.nvim",
    dependencies = { "MunifTanjim/nui.nvim", "nvim-lua/plenary.nvim" },
    opts = {},
  },
}
