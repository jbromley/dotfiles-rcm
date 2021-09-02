local wezterm = require 'wezterm';

local my_keys = {
    {key = "r", mods = "CTRL|SHIFT", action = "ReloadConfiguration"},

    -- Copy and paste
    {key = "c", mods = "CTRL|SHIFT", action = wezterm.action{CopyTo="ClipboardAndPrimarySelection"}},
    {key = "v", mods = "CTRL|SHIFT", action = wezterm.action{PasteFrom="Clipboard"}},
    {key = "x", mods="CTRL|SHIFT", action = "ActivateCopyMode"},
    {key = " ", mods="CTRL|SHIFT", action = "QuickSelect"},

    -- Search
    {key = "f", mods="CTRL|SHIFT", action = wezterm.action{Search = {CaseInSensitiveString = ""}}},

    -- Font size adjustment
    {key = "-", mods = "CTRL", action = "DecreaseFontSize"},
    {key = "=", mods = "CTRL", action = "IncreaseFontSize"},
    {key = "0", mods = "CTRL", action = "ResetFontSize"},

    -- New windows and tab creation, closing, and navigation
    -- {key = "n", mods = "CTRL|SHIFT", action = "SpawnWindow"},
    {key = "w", mods = "LEADER", action = "SpawnWindow"},
    -- {key = "t", mods = "CTRL|SHIFT", action = wezterm.action{SpawnTab = "CurrentPaneDomain"}},
    {key = "t", mods = "LEADER", action = wezterm.action{SpawnTab = "CurrentPaneDomain"}},
    -- {key = "t", mods = "CTRL|ALT", action = wezterm.action{SpawnTab = "DefaultDomain"}},
    {key = "T", mods = "LEADER", action = wezterm.action{SpawnTab = "DefaultDomain"}},
    -- {key = "w", mods = "CTRL|SHIFT", action = wezterm.action{CloseCurrentTab = {confirm = true}}},
    {key = "x", mods = "LEADER", action = wezterm.action{CloseCurrentTab = {confirm = true}}},
    -- {key = "[", mods = "SUPER|ALT", action = wezterm.action{ActivateTabRelative = -1}},
    -- {key = "]", mods = "SUPER|ALT", action = wezterm.action{ActivateTabRelative = 1}},
    {key = "[", mods = "LEADER", action = wezterm.action{ActivateTabRelative = -1}},
    {key = "]", mods = "LEADER", action = wezterm.action{ActivateTabRelative = 1}},
    -- {key = "o", mods = "SUPER|ALT", action = "ActivateLastTab"},
    {key = "o", mods = "LEADER", action = "ActivateLastTab"},
    -- {key = "[", mods = "CTRL|SUPER|ALT", action = wezterm.action{MoveTabRelative = -1}},
    -- {key = "]", mods = "CTRL|SUPER|ALT", action = wezterm.action{MoveTabRelative = 1}},
    {key = "{", mods = "LEADER", action = wezterm.action{MoveTabRelative = -1}},
    {key = "}", mods = "LEADER", action = wezterm.action{MoveTabRelative = 1}},
    -- {key = "n", mods = "SUPER|ALT", action = "ShowTabNavigator"},
    {key = "n", mods = "LEADER", action = "ShowTabNavigator"},
    {key = "Enter", mods = "ALT", action = "ToggleFullScreen"},

    -- Scrollback buffer
    {key = "PageUp", mods = "SHIFT", action = wezterm.action{ScrollByLine = -1}},
    {key = "PageUp", mods = "CTRL", action = wezterm.action{ScrollByPage = -1}},
    {key = "PageDown", mods = "SHIFT", action = wezterm.action{ScrollByLine = 1}},
    {key = "PageDown", mods = "CTRL", action = wezterm.action{ScrollByPage = 1}},
    {key = "k", mods="CTRL|SHIFT", action = wezterm.action{ClearScrollback = "ScrollbackOnly"}},

    -- Pane creation, resizing, and navigation
    {key = "|", mods="LEADER", action = wezterm.action{SplitHorizontal = {domain = "CurrentPaneDomain"}}},
    {key = "-", mods="LEADER", action = wezterm.action{SplitVertical = {domain = "CurrentPaneDomain"}}},
    {key = "h", mods="LEADER", action = wezterm.action{ActivatePaneDirection = "Left"}},
    {key = "l", mods="LEADER", action = wezterm.action{ActivatePaneDirection = "Right"}},
    {key = "k", mods="LEADER", action = wezterm.action{ActivatePaneDirection = "Up"}},
    {key = "j", mods="LEADER", action = wezterm.action{ActivatePaneDirection = "Down"}},
    {key = "LeftArrow", mods="SHIFT", action = wezterm.action{AdjustPaneSize = {"Left", 1}}},
    {key = "RightArrow", mods="SHIFT", action = wezterm.action{AdjustPaneSize = {"Right", 1}}},
    {key = "UpArrow", mods="SHIFT", action = wezterm.action{AdjustPaneSize = {"Up", 1}}},
    {key = "DownArrow", mods="SHIFT", action = wezterm.action{AdjustPaneSize = {"Down", 1}}},
    {key = "z", mods = "LEADER", action = "TogglePaneZoomState"},

    -- Debugging
    {key = "F2", mods = "CTRL", action = "ShowDebugOverlay"},
}

for i = 1, 8 do
    table.insert(my_keys, {key = tostring(i), mods = "LEADER", action = wezterm.action{ActivateTab = i - 1}})
end

return {
    font = wezterm.font("JetBrainsMono Nerd Font"),
    font_size = 11.0,

    color_scheme = "Dracula Pro",

    tab_max_width = 32,
    tab_bar_at_bottom = true,

    window_decorations = "NONE",
    window_background_opacity = 0.875,

    -- Key bindings
    disable_default_key_bindings = true,
    leader = {key = "VoidSymbol", mods = ""},
    keys = my_keys,
}
