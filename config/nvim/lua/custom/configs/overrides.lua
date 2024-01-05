local M = {}

M.treesitter = {
  ensure_installed = {
    "vim",
    "lua",
    "c",
    "cpp",
    "markdown",
    "markdown_inline",
    "erlang",
    "elixir",
    "gleam",
    "racket"
  },
  indent = {
    enable = true,
    -- disable = {
    --   "python"
    -- },
  },
}

M.mason = {
  ensure_installed = {
    -- lua stuff
    "lua-language-server",
    "stylua",

    -- c/cpp stuff
    "clangd",
    "clang-format",

    -- Python
    "pyright",

    -- BEAM languages 
    "erlang-ls",
    "elixir-ls",
    "gleam"
  },
}

-- git support in nvimtree
M.nvimtree = {
  git = {
    enable = true,
  },

  renderer = {
    highlight_git = true,
    icons = {
      show = {
        git = true,
      },
    },
  },
}

return M
