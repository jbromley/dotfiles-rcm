local wezterm = require("wezterm")
local act = wezterm.action

local my_keys = {
	{ key = "Tab", mods = "SHIFT|LEADER", action = act.ActivateTabRelative(-1) },
	{ key = "Tab", mods = "LEADER", action = act.ActivateTabRelative(1) },

	{ key = "[", mods = "LEADER", action = act.ActivateTabRelative(-1) },
	{ key = "]", mods = "LEADER", action = act.ActivateTabRelative(1) },

	{ key = "PageUp", mods = "LEADER", action = act.ActivateTabRelative(-1) },
	{ key = "PageDown", mods = "LEADER", action = act.ActivateTabRelative(1) },

	{ key = "Enter", mods = "LEADER", action = act.ToggleFullScreen },

	{ key = '"', mods = "SHIFT|LEADER", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ key = "%", mods = "SHIFT|LEADER", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },

	{ key = "1", mods = "LEADER", action = act.ActivateTab(0) },
	{ key = "2", mods = "LEADER", action = act.ActivateTab(1) },
	{ key = "3", mods = "LEADER", action = act.ActivateTab(2) },
	{ key = "4", mods = "LEADER", action = act.ActivateTab(3) },
	{ key = "5", mods = "LEADER", action = act.ActivateTab(4) },
	{ key = "6", mods = "LEADER", action = act.ActivateTab(5) },
	{ key = "7", mods = "LEADER", action = act.ActivateTab(6) },
	{ key = "8", mods = "LEADER", action = act.ActivateTab(7) },
	{ key = "9", mods = "LEADER", action = act.ActivateTab(-1) },

	{ key = "+", mods = "SHIFT|LEADER", action = act.IncreaseFontSize },
	{ key = "_", mods = "SHIFT|LEADER", action = act.DecreaseFontSize },
	{ key = ")", mods = "SHIFT|LEADER", action = act.ResetFontSize },

	{ key = "c", mods = "SHIFT|CTRL", action = act.CopyTo("Clipboard") },

	{ key = "f", mods = "SHIFT|CTRL", action = act.Search("CurrentSelectionOrEmptyString") },

	{ key = "k", mods = "SHIFT|LEADER", action = act.ClearScrollback("ScrollbackOnly") },

	{ key = "l", mods = "SHIFT|LEADER", action = act.ShowDebugOverlay },

	{ key = "m", mods = "LEADER", action = act.Hide },

	{ key = "n", mods = "LEADER", action = act.SpawnWindow },

	{ key = "p", mods = "LEADER", action = act.ActivateCommandPalette },

	{ key = "r", mods = "LEADER", action = act.ReloadConfiguration },

	{ key = "s", mods = "LEADER", action = act.PaneSelect({ alphabet = "", mode = "Activate" }) },

	{ key = "t", mods = "LEADER", action = act.SpawnTab("CurrentPaneDomain") },

	{
		key = "u",
		mods = "SHIFT|CTRL",
		action = act.CharSelect({ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }),
	},

	{ key = "v", mods = "SHIFT|CTRL", action = act.PasteFrom("Clipboard") },

	{ key = "w", mods = "LEADER", action = act.CloseCurrentTab({ confirm = true }) },

	{ key = "x", mods = "SHIFT|CTRL", action = act.ActivateCopyMode },

	{ key = "z", mods = "LEADER", action = act.TogglePaneZoomState },

	{ key = "phys:Space", mods = "LEADER", action = act.QuickSelect },

	{ key = "PageUp", mods = "", action = act.ScrollByPage(-1) },
	{ key = "PageDown", mods = "", action = act.ScrollByPage(1) },

	{ key = "PageUp", mods = "SHIFT", action = act.ScrollByLine(-1) },
	{ key = "PageDown", mods = "SHIFT", action = act.ScrollByLine(1) },

	{ key = "PageUp", mods = "SHIFT|LEADER", action = act.MoveTabRelative(-1) },
	{ key = "PageDown", mods = "SHIFT|LEADER", action = act.MoveTabRelative(1) },

	{ key = "LeftArrow", mods = "SHIFT|LEADER", action = act.ActivatePaneDirection("Left") },
	{ key = "RightArrow", mods = "SHIFT|LEADER", action = act.ActivatePaneDirection("Right") },
	{ key = "UpArrow", mods = "SHIFT|LEADER", action = act.ActivatePaneDirection("Up") },
	{ key = "DownArrow", mods = "SHIFT|LEADER", action = act.ActivatePaneDirection("Down") },

	{ key = "h", mods = "LEADER", action = act.ActivatePaneDirection("Left") },
	{ key = "l", mods = "LEADER", action = act.ActivatePaneDirection("Right") },
	{ key = "k", mods = "LEADER", action = act.ActivatePaneDirection("Up") },
	{ key = "j", mods = "LEADER", action = act.ActivatePaneDirection("Down") },

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
	leader = { key = "VoidSymbol", mods = "", timeout_milliseconds = 500 },
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
