-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
local alpha = function()
  return string.format("%x", math.floor((255 * vim.g.transparency) or 0.875))
end

vim.g.neovide_transparency = 0.96875
vim.g.neovide_window_blurred = true

-- Some LSPs use node. Make sure it is in the path.
if vim.fn.executable("node") == 0 then
  vim.env.PATH = vim.env.PATH .. ":" .. vim.env.HOME .. "/.local/share/mise/installs/node/20/bin"
end

-- Set the font for GUI clients.
vim.opt.guifont = "JetBrainsMono Nerd Font:h10"
