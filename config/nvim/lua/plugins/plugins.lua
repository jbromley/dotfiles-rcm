return {
  "tpope/vim-commentary",
  "tpope/vim-fugitive",
  {
    "tpope/vim-dadbod",
    delay = true,
  },
  {
    "kristijanhusak/vim-dadbod-ui",
    delay = true,
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
}
