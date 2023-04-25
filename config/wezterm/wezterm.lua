local wezterm = require("wezterm")
local act = wezterm.action

my_keys = {
	{ key = "Tab", mods = "SHIFT|CTRL", action = act.ActivateTabRelative(-1) },
	{ key = "Tab", mods = "CTRL", action = act.ActivateTabRelative(1) },

	{ key = "{", mods = "SHIFT|CTRL", action = act.ActivateTabRelative(-1) },
	{ key = "}", mods = "SHIFT|CTRL", action = act.ActivateTabRelative(1) },

	{ key = "PageUp", mods = "CTRL", action = act.ActivateTabRelative(-1) },
	{ key = "PageDown", mods = "CTRL", action = act.ActivateTabRelative(1) },

	{ key = "Enter", mods = "ALT", action = act.ToggleFullScreen },

	{ key = '"', mods = "SHIFT|ALT|CTRL", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ key = "%", mods = "SHIFT|ALT|CTRL", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },

	{ key = "!", mods = "SHIFT|CTRL", action = act.ActivateTab(0) },
	{ key = "@", mods = "SHIFT|CTRL", action = act.ActivateTab(1) },
	{ key = "#", mods = "SHIFT|CTRL", action = act.ActivateTab(2) },
	{ key = "$", mods = "SHIFT|CTRL", action = act.ActivateTab(3) },
	{ key = "%", mods = "SHIFT|CTRL", action = act.ActivateTab(4) },
	{ key = "^", mods = "SHIFT|CTRL", action = act.ActivateTab(5) },
	{ key = "&", mods = "SHIFT|CTRL", action = act.ActivateTab(6) },
	{ key = "*", mods = "SHIFT|CTRL", action = act.ActivateTab(7) },
	{ key = "(", mods = "SHIFT|CTRL", action = act.ActivateTab(-1) },

	{ key = "+", mods = "SHIFT|CTRL", action = act.IncreaseFontSize },
	{ key = "_", mods = "SHIFT|CTRL", action = act.DecreaseFontSize },
	{ key = ")", mods = "SHIFT|CTRL", action = act.ResetFontSize },

	{ key = "c", mods = "SHIFT|CTRL", action = act.CopyTo("Clipboard") },

	{ key = "f", mods = "SHIFT|CTRL", action = act.Search("CurrentSelectionOrEmptyString") },

	{ key = "k", mods = "SHIFT|CTRL", action = act.ClearScrollback("ScrollbackOnly") },

	{ key = "l", mods = "SHIFT|CTRL", action = act.ShowDebugOverlay },

	{ key = "m", mods = "SHIFT|CTRL", action = act.Hide },

	{ key = "n", mods = "SHIFT|CTRL", action = act.SpawnWindow },

	{ key = "p", mods = "SHIFT|CTRL", action = act.ActivateCommandPalette },

	{ key = "r", mods = "SHIFT|CTRL", action = act.ReloadConfiguration },

	{ key = "s", mods = "SHIFT|CTRL", action = act.PaneSelect({ alphabet = "", mode = "Activate" }) },

	{ key = "t", mods = "SHIFT|CTRL", action = act.SpawnTab("CurrentPaneDomain") },

	{
		key = "u",
		mods = "SHIFT|CTRL",
		action = act.CharSelect({ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }),
	},

	{ key = "v", mods = "SHIFT|CTRL", action = act.PasteFrom("Clipboard") },

	{ key = "w", mods = "SHIFT|CTRL", action = act.CloseCurrentTab({ confirm = true }) },

	{ key = "x", mods = "SHIFT|CTRL", action = act.ActivateCopyMode },

	{ key = "z", mods = "SHIFT|CTRL", action = act.TogglePaneZoomState },

	{ key = "phys:Space", mods = "SHIFT|CTRL", action = act.QuickSelect },

	{ key = "PageUp", mods = "", action = act.ScrollByPage(-1) },
	{ key = "PageDown", mods = "", action = act.ScrollByPage(1) },

	{ key = "PageUp", mods = "SHIFT", action = act.ScrollByLine(-1) },
	{ key = "PageDown", mods = "SHIFT", action = act.ScrollByLine(1) },

	{ key = "PageUp", mods = "SHIFT|CTRL", action = act.MoveTabRelative(-1) },
	{ key = "PageDown", mods = "SHIFT|CTRL", action = act.MoveTabRelative(1) },

	{ key = "LeftArrow", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Left") },
	{ key = "RightArrow", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Right") },
	{ key = "UpArrow", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Up") },
	{ key = "DownArrow", mods = "SHIFT|CTRL", action = act.ActivatePaneDirection("Down") },

	{ key = "h", mods = "ALT|CTRL", action = act.ActivatePaneDirection("Left") },
	{ key = "l", mods = "ALT|CTRL", action = act.ActivatePaneDirection("Right") },
	{ key = "k", mods = "ALT|CTRL", action = act.ActivatePaneDirection("Up") },
	{ key = "j", mods = "ALT|CTRL", action = act.ActivatePaneDirection("Down") },

	{ key = "LeftArrow", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Left", 1 }) },
	{ key = "RightArrow", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Right", 1 }) },
	{ key = "UpArrow", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Up", 1 }) },
	{ key = "DownArrow", mods = "SHIFT|ALT|CTRL", action = act.AdjustPaneSize({ "Down", 1 }) },

	{ key = "Insert", mods = "SHIFT", action = act.PasteFrom("PrimarySelection") },
	{ key = "Paste", mods = "NONE", action = act.PasteFrom("Clipboard") },

	{ key = "Insert", mods = "CTRL", action = act.CopyTo("PrimarySelection") },
	{ key = "Copy", mods = "NONE", action = act.CopyTo("Clipboard") },
}

return {
	term = "wezterm",
	initial_cols = 120,
	initial_rows = 50,
	color_scheme = "Dracula (Official)",

	window_frame = {
		font_size = 11.0,
	},

	font = wezterm.font("JetBrains Mono"),
	font_size = 11.0,

	tab_max_width = 256,

	window_decorations = "RESIZE",
	window_background_opacity = 0.95,

	inactive_pane_hsb = {
		saturation = 0.8,
		brightness = 0.75,
	},

	disable_default_key_bindings = true,
	-- leader = { key ='VoidSymbol', mods='', timeout_milliseconds=500 },
	keys = my_keys,

	ssh_domains = {
		{
			name = "llama",
			remote_address = "llama.ai.appliedinvention.com",
			username = "jay",
		},
	},
}

-- vim: foldmethod=marker:foldlevel=10
