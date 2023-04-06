return {
  -- { "Mofiqul/dracula.nvim" },
  {
    "folke/tokyonight.nvim",
    lazy = true,
  },
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "tokyonight",
    },
  },
  -- {
  --   "folke/noice.nvim",
  --   -- enabled = false,
  --   opts = {
  --     presets = {
  --       bottom_search = true,
  --       command_palette = true,
  --       -- long_message_to_split = true,
  --     },
  --     cmdline = { view = "cmdline" },
  --   },
  -- },
}
