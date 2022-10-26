local wezterm = require 'wezterm';
local act = wezterm.action

my_keys = {
  -- Copy, paste, and select text.
  { key = 'c', mods = 'SHIFT|CTRL', action = act.CopyTo 'ClipboardAndPrimarySelection' },
  { key = 'v', mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
  { key = 'x', mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },
  { key = 'phys:Space', mods = 'SHIFT|CTRL', action = act.QuickSelect },

  -- Window commands
  { key = 'm', mods = 'LEADER', action = act.Hide },
  { key = 'n', mods = 'LEADER', action = act.SpawnWindow },
  { key = 'Enter', mods = 'LEADER', action = act.ToggleFullScreen },

  -- Font size
  { key = '-', mods = 'LEADER', action = act.DecreaseFontSize },
  { key = '=', mods = 'LEADER', action = act.IncreaseFontSize },
  { key = '0', mods = 'LEADER', action = act.ResetFontSize },

  -- Tab commands
  { key = 't', mods = 'LEADER', action = act.SpawnTab 'CurrentPaneDomain' },
  { key = 'w', mods = 'LEADER', action = act.CloseCurrentTab{ confirm = true } },
  { key = 'Tab', mods = 'LEADER', action = act.ActivateTabRelative(1) },
  { key = 'Tab', mods = 'SHIFT|LEADER', action = act.ActivateTabRelative(-1) },
  { key = '[', mods = 'LEADER', action = act.ActivateTabRelative(-1) },
  { key = ']', mods = 'LEADER', action = act.ActivateTabRelative(1) },
  { key = '1', mods = 'LEADER', action = act.ActivateTab(0) },
  { key = '2', mods = 'LEADER', action = act.ActivateTab(1) },
  { key = '3', mods = 'LEADER', action = act.ActivateTab(2) },
  { key = '4', mods = 'LEADER', action = act.ActivateTab(3) },
  { key = '5', mods = 'LEADER', action = act.ActivateTab(4) },
  { key = '6', mods = 'LEADER', action = act.ActivateTab(5) },
  { key = '7', mods = 'LEADER', action = act.ActivateTab(6) },
  { key = '8', mods = 'LEADER', action = act.ActivateTab(7) },
  { key = '9', mods = 'LEADER', action = act.ActivateTab(8) },
  { key = '{', mods = 'SHIFT|LEADER', action = act.MoveTabRelative(-1) },
  { key = '}', mods = 'SHIFT|LEADER', action = act.MoveTabRelative(1) },

  -- Panes
  { key = '%', mods = 'SHIFT|LEADER', action = act.SplitVertical{ domain =  'CurrentPaneDomain' } },
  { key = '"', mods = 'SHIFT|LEADER', action = act.SplitHorizontal{ domain =  'CurrentPaneDomain' } },
  { key = 'p', mods = 'LEADER', action = act.PaneSelect{ alphabet =  '', mode =  'Activate' } },
  { key = 'z', mods = 'LEADER', action = act.TogglePaneZoomState },

  { key = 'h', mods = 'LEADER', action = act.ActivatePaneDirection 'Left' },
  { key = 'l', mods = 'LEADER', action = act.ActivatePaneDirection 'Right' },
  { key = 'k', mods = 'LEADER', action = act.ActivatePaneDirection 'Up' },
  { key = 'j', mods = 'LEADER', action = act.ActivatePaneDirection 'Down' },

  { key = 'h', mods = 'SHIFT|LEADER', action = act.AdjustPaneSize{ 'Left', 1 } },
  { key = 'l', mods = 'SHIFT|LEADER', action = act.AdjustPaneSize{ 'Right', 1 } },
  { key = 'k', mods = 'SHIFT|LEADER', action = act.AdjustPaneSize{ 'Up', 1 } },
  { key = 'j', mods = 'SHIFT|LEADER', action = act.AdjustPaneSize{ 'Down', 1 } },

  -- Search
  { key = 'f', mods = 'SHIFT|CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },

  -- Miscellaneous
  { key = 'PageUp', mods = 'SHIFT', action = act.ScrollByPage(-1) },
  { key = 'PageDown', mods = 'SHIFT', action = act.ScrollByPage(1) },
  { key = 'PageUp', mods = 'CTRL', action = act.ScrollByLine(-1) },
  { key = 'PageDown', mods = 'CTRL', action = act.ScrollByLine(1) },
  { key = 'k', mods = 'SHIFT|CTRL', action = act.ClearScrollback 'ScrollbackOnly' },
  { key = 'l', mods = 'SHIFT|CTRL', action = act.ShowDebugOverlay },
  { key = 'r', mods = 'SHIFT|CTRL', action = act.ReloadConfiguration },
  { key = 'u', mods = 'SHIFT|CTRL', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
}

return {
    term = "wezterm",
    initial_cols = 104,
    initial_rows = 48,

    window_frame = {
        font_size = 11.0,
    },

    font_size = 11.0,

    tab_max_width = 256,

    window_decorations = "RESIZE",
    window_background_opacity = 0.875,

    inactive_pane_hsb = {
      saturation = 0.8,
      brightness = 0.75,
    },

    disable_default_key_bindings = true,
    leader = { key ='VoidSymbol', mods='', timeout_milliseconds=500 },
    keys = my_keys,
}

-- vim: foldmethod=marker:foldlevel=10
