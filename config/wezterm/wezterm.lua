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

local my_tab_bar = {
   -- The color of the strip that goes along the top of the window
   --background = "#2a2a80",
   background = "#000040",

      -- The active tab is the one that has focus in the window
      active_tab = {
        -- The color of the background area for the tab
        bg_color = "#5555ff",
        -- The color of the text for the tab
        fg_color = "#fcfcff",

        -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
        -- label shown for this tab.
        -- The default is "Normal"
        intensity = "Bold",

        -- Specify whether you want "None", "Single" or "Double" underline for
        -- label shown for this tab.
        -- The default is "None"
        underline = "None",

        -- Specify whether you want the text to be italic (true) or not (false)
        -- for this tab.  The default is false.
        italic = false,

        -- Specify whether you want the text to be rendered with strikethrough (true)
        -- or not for this tab.  The default is false.
        strikethrough = false,
      },

      -- Inactive tabs are the tabs that do not have focus
      inactive_tab = {
        bg_color = "#2a2a80",
        fg_color = "#babacc",

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `inactive_tab`.
        intensity = "Normal",
      },

      -- You can configure some alternate styling when the mouse pointer
      -- moves over inactive tabs
      inactive_tab_hover = {
        bg_color = "#5455e0",
        fg_color = "#c8c8c8",
        italic = true,

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `inactive_tab_hover`.
      },

      -- The new tab button that let you create new tabs
      new_tab = {
        bg_color = "#4040c0",
        fg_color = "#fcfcff",

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `new_tab`.
      },

      -- You can configure some alternate styling when the mouse pointer
      -- moves over the new tab button
      new_tab_hover = {
        bg_color = "#5555ff",
        fg_color = "#fcfcff",
        italic = true,

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `new_tab_hover`.
      }
}

return {
    font = wezterm.font("JetBrainsMono Nerd Font"),
    font_size = 11.0,

    color_scheme = "Dracula Pro",

    colors = {
       tab_bar = my_tab_bar,
    },

    window_padding = {
       top = 4,
       left = 4,
       bottom = 4,
       right = 4,
    },

    tab_max_width = 32,
    tab_bar_at_bottom = false,

    window_decorations = "NONE",
    window_background_opacity = 0.875,

    -- Key bindings
    disable_default_key_bindings = true,
    leader = {key = "VoidSymbol", mods = ""},
    keys = my_keys,
}
