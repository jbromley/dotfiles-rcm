return {
  -- Add dracula theme for Neovim.
  { "Mofiqul/dracula.nvim" },

  -- Configure LazyVim to load dracula.
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "dracula",
    },
  },
}
