return {
  "tpope/vim-commentary",
  "tpope/vim-fugitive",
  {
    "vimwiki/vimwiki",
    delay = false,
    init = function(_)
      vim.g.vimwiki_list = { { path = "~/Documents/Wiki" } }
    end,
    lazy = true,
    cmd = "VimwikiIndex",
    keys = {
      { "<leader>ww", "<Cmd>VimwikiIndex<CR>", desc = "Open Vimwiki index" },
    },
    {
      "elixir-editors/vim-elixir",
      lazy = true,
      event = "BufEnter *.ex",
    },
  },
  {
    "elixir-editors/vim-elixir",
  },
}
