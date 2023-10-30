return {
  {
    "tpope/vim-commentary",
  },
  {
    "tpope/vim-fugitive",
  },
  {
    "vimwiki/vimwiki",
    delay = false,
    init = function(_)
      vim.g.vimwiki_list = { { path = "~/Documents/Wiki", syntax = "markdown", ext = ".md" } }
    end,
    lazy = true,
    cmd = "VimwikiIndex",
    keys = {
      { "<leader>ww", "<Cmd>VimwikiIndex<CR>", desc = "Open Vimwiki index" },
      { "<leader>w<Space>x", "<Cmd>VimwikiToggleListItem<CR>", desc = "Toggle Vimwiki list item" },
    },
  },
  {
    "gleam-lang/gleam.vim",
  },
}
