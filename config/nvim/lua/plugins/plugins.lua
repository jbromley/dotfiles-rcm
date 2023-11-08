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
      vim.g.vimwiki_ext2syntax = { [".md"] = "markdown", [".markdown"] = "markdown", [".mdown"] = "markdown" }
    end,
    lazy = true,
    cmd = "VimwikiIndex",
    keys = {
      { "<leader>kk", "<Cmd>VimwikiIndex<CR>", desc = "Open Vimwiki index" },
      { "<leader>kx", "<Cmd>VimwikiToggleListItem<CR>", desc = "Toggle Vimwiki list item" },
    },
  },
  {
    "gleam-lang/gleam.vim",
  },
}
