local wezterm = require 'wezterm';
local act = wezterm.action

my_keys = {
  {key = 'Tab', mods = 'SHIFT|CTRL', action = act.ActivateTabRelative(-1)},
  {key = 'Tab', mods = 'CTRL', action = act.ActivateTabRelative(1)},

  {key = '{', mods = 'SHIFT|CMD', action =  act.ActivateTabRelative(-1)},
  {key = '}', mods = 'SHIFT|CMD', action =  act.ActivateTabRelative(1)},

  {key = 'PageUp', mods = 'CTRL', action = act.ActivateTabRelative(-1)},
  {key = 'PageDown', mods = 'CTRL', action = act.ActivateTabRelative(1)},

  {key = 'Enter', mods = 'CMD', action = act.ToggleFullScreen},

  {key = '"', mods = 'SHIFT|ALT|CTRL', action = act.SplitVertical{domain =  'CurrentPaneDomain'}},
  {key = '%', mods = 'SHIFT|ALT|CTRL', action = act.SplitHorizontal{domain =  'CurrentPaneDomain'}},
  {key = "5", mods = 'ALT|CMD', action = act.SplitVertical{domain =  'CurrentPaneDomain'}},
  {key = '\\', mods = 'ALT|CMD', action = act.SplitHorizontal{domain =  'CurrentPaneDomain'}},

  -- {key = '!', mods = 'CTRL', action = act.ActivateTab(0)},
  {key = '1', mods = 'CMD', action = act.ActivateTab(0)},
  {key = '2', mods = 'CMD', action = act.ActivateTab(1)},
  {key = '3', mods = 'CMD', action = act.ActivateTab(2)},
  {key = '4', mods = 'CMD', action = act.ActivateTab(3)},
  {key = '5', mods = 'CMD', action = act.ActivateTab(4)},
  {key = '6', mods = 'CMD', action = act.ActivateTab(5)},
  {key = '7', mods = 'CMD', action = act.ActivateTab(6)},
  {key = '8', mods = 'CMD', action = act.ActivateTab(7)},
  {key = '9', mods = 'CMD', action = act.ActivateTab(-1)},

  {key = '+', mods = 'SHIFT|CMD', action = act.IncreaseFontSize},
  {key = '_', mods = 'SHIFT|CMD', action = act.DecreaseFontSize},
  {key = ')', mods = 'SHIFT|CMD', action = act.ResetFontSize},

  {key = 'c', mods = 'CMD', action = act.CopyTo 'Clipboard'},

  {key = 'f', mods = 'CMD', action = act.Search 'CurrentSelectionOrEmptyString'},

  {key = 'k', mods = 'CMD', action = act.ClearScrollback 'ScrollbackOnly'},

  {key = 'l', mods = 'CMD', action = act.ShowDebugOverlay},

  {key = 'm', mods = 'CMD', action = act.Hide},

  {key = 'n', mods = 'CMD', action = act.SpawnWindow},

  {key = 'p', mods = 'CMD', action = act.PaneSelect{alphabet =  '', mode =  'Activate'}},

  {key = 'r', mods = 'CMD', action = act.ReloadConfiguration},

  {key = 't', mods = 'CMD', action = act.SpawnTab 'CurrentPaneDomain'},

  {key = 'u', mods = 'CMD', action = act.CharSelect{copy_on_select = true, copy_to = 'ClipboardAndPrimarySelection'}},

  {key = 'v', mods = 'CMD', action = act.PasteFrom 'Clipboard'},

  {key = 'w', mods = 'CMD', action = act.CloseCurrentTab{confirm = true}},

  {key = 'x', mods = 'CMD', action = act.ActivateCopyMode},

  {key = 'z', mods = 'CMD', action = act.TogglePaneZoomState},

  {key = 'phys:Space', mods = 'SHIFT|CTRL', action = act.QuickSelect},

  {key = 'PageUp', mods = '', action = act.ScrollByPage(-1)},
  {key = 'PageDown', mods = '', action = act.ScrollByPage(1)},

  {key = 'PageUp', mods = 'SHIFT', action = act.ScrollByLine(-1)},
  {key = 'PageDown', mods = 'SHIFT', action = act.ScrollByLine(1)},

  {key = 'PageUp', mods = 'CMD', action = act.MoveTabRelative(-1)},
  {key = 'PageDown', mods = 'CMD', action = act.MoveTabRelative(1)},

  {key = 'LeftArrow', mods = 'CMD', action = act.ActivatePaneDirection 'Left'},
  {key = 'RightArrow', mods = 'CMD', action = act.ActivatePaneDirection 'Right'},
  {key = 'UpArrow', mods = 'CMD', action = act.ActivatePaneDirection 'Up'},
  {key = 'DownArrow', mods = 'CMD', action = act.ActivatePaneDirection 'Down'},

  {key = 'LeftArrow', mods = 'SHIFT|CMD', action = act.AdjustPaneSize{'Left', 1}},
  {key = 'RightArrow', mods = 'SHIFT|CMD', action = act.AdjustPaneSize{'Right', 1}},
  {key = 'UpArrow', mods = 'SHIFT|CMD', action = act.AdjustPaneSize{'Up', 1}},
  {key = 'DownArrow', mods = 'SHIFT|CMD', action = act.AdjustPaneSize{'Down', 1}},

  {key = 'Insert', mods = 'SHIFT', action = act.PasteFrom 'PrimarySelection'},
  {key = 'Paste', mods = 'NONE', action = act.PasteFrom 'Clipboard'},

  {key = 'Insert', mods = 'CTRL', action = act.CopyTo 'PrimarySelection'},
  {key = 'Copy', mods = 'NONE', action = act.CopyTo 'Clipboard'},
}

return {
  term = "wezterm",
  initial_cols = 120,
  initial_rows = 50,
  color_scheme = 'Dracula (Official)',

  window_frame = {
    font_size = 11.0,
  },

  -- font = wezterm.font 'Fira Code',
  font_size = 11.0,

  tab_max_width = 256,

  window_decorations = "RESIZE",
  window_background_opacity = 0.95,

  inactive_pane_hsb = {
    saturation = 0.8,
    brightness = 0.75,
  },

  disable_default_key_bindings = true,
  keys = my_keys,

  ssh_domains = {
    {
      name = 'llama',
      remote_address = 'llama.ai.appliedinvention.com',
      username = 'jay'
    },
  }
}

-- vim: foldmethod=marker:foldlevel=10
