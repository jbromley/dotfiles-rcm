local wezterm = require("wezterm")
local act = wezterm.action

-- Customize key bindings.
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

	{ key = "c", mods = "LEADER", action = act.CopyTo("Clipboard") },

	{ key = "f", mods = "SHIFT|CTRL", action = act.Search("CurrentSelectionOrEmptyString") },

	{ key = "k", mods = "SHIFT|LEADER", action = act.ClearScrollback("ScrollbackOnly") },

	{ key = "l", mods = "SHIFT|LEADER", action = act.ShowDebugOverlay },

	{ key = "m", mods = "LEADER", action = act.Hide },

	{ key = "n", mods = "LEADER", action = act.SpawnWindow },

	{ key = "p", mods = "LEADER", action = act.ActivateCommandPalette },

	{ key = "r", mods = "SHIFT|LEADER", action = act.ReloadConfiguration },

	{ key = "s", mods = "LEADER", action = act.PaneSelect({ alphabet = "", mode = "Activate" }) },

	{ key = "t", mods = "LEADER", action = act.SpawnTab("CurrentPaneDomain") },

	{
		key = "u",
		mods = "SHIFT|CTRL",
		action = act.CharSelect({ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }),
	},

	{ key = "v", mods = "LEADER", action = act.PasteFrom("Clipboard") },

	{ key = "w", mods = "LEADER", action = act.CloseCurrentTab({ confirm = true }) },

	{ key = "x", mods = "LEADER", action = act.ActivateCopyMode },

	{ key = "z", mods = "LEADER", action = act.TogglePaneZoomState },

	{ key = "phys:Space", mods = "LEADER", action = act.QuickSelect },

	{ key = "PageUp", action = act.ScrollByPage(-1) },
	{ key = "PageDown", action = act.ScrollByPage(1) },

	{ key = "PageUp", mods = "SHIFT", action = act.ScrollByLine(-1) },
	{ key = "PageDown", mods = "SHIFT", action = act.ScrollByLine(1) },

	{ key = ",", mods = "LEADER", action = act.MoveTabRelative(-1) },
	{ key = ".", mods = "LEADER", action = act.MoveTabRelative(1) },

	{ key = "LeftArrow", mods = "LEADER", action = act.ActivatePaneDirection("Left") },
	{ key = "RightArrow", mods = "LEADER", action = act.ActivatePaneDirection("Right") },
	{ key = "UpArrow", mods = "LEADER", action = act.ActivatePaneDirection("Up") },
	{ key = "DownArrow", mods = "LEADER", action = act.ActivatePaneDirection("Down") },

	{ key = "h", mods = "LEADER", action = act.ActivatePaneDirection("Left") },
	{ key = "l", mods = "LEADER", action = act.ActivatePaneDirection("Right") },
	{ key = "k", mods = "LEADER", action = act.ActivatePaneDirection("Up") },
	{ key = "j", mods = "LEADER", action = act.ActivatePaneDirection("Down") },

	{ key = "r", mods = "LEADER", action = act.ActivateKeyTable({ name = "resize_pane", one_shot = false }) },

	{ key = "Insert", mods = "SHIFT", action = act.PasteFrom("PrimarySelection") },
	{ key = "Paste", mods = "NONE", action = act.PasteFrom("Clipboard") },

	{ key = "Insert", mods = "CTRL", action = act.CopyTo("PrimarySelection") },
	{ key = "Copy", mods = "NONE", action = act.CopyTo("Clipboard") },
}

local my_key_tables = {
	resize_pane = {
		{ key = "LeftArrow", action = act.AdjustPaneSize({ "Left", 1 }) },
		{ key = "RightArrow", action = act.AdjustPaneSize({ "Right", 1 }) },
		{ key = "UpArrow", action = act.AdjustPaneSize({ "Up", 1 }) },
		{ key = "DownArrow", action = act.AdjustPaneSize({ "Down", 1 }) },
		{ key = "Escape", action = "PopKeyTable" },
	},
}

wezterm.on("update-status", function(window, _)
	local name = window:active_key_table()
	if name then
		name = name .. " "
	end
	window:set_right_status(wezterm.format({
		{ Attribute = { Intensity = "Normal" } },
		{ Text = name or "" },
	}))
end)

-- Fix hyperlink selection rules to avoid picking up closing brackets or parentheses.
local my_hyperlink_rules = {
	-- Matches: a URL in parens: (URL)
	{
		regex = "\\((\\w+://\\S+)\\)",
		format = "$1",
		highlight = 1,
	},
	-- Matches: a URL in brackets: [URL]
	{
		regex = "\\[(\\w+://\\S+)\\]",
		format = "$1",
		highlight = 1,
	},
	-- Matches: a URL in curly braces: {URL}
	{
		regex = "\\{(\\w+://\\S+)\\}",
		format = "$1",
		highlight = 1,
	},
	-- Matches: a URL in angle brackets: <URL>
	{
		regex = "<(\\w+://\\S+)>",
		format = "$1",
		highlight = 1,
	},
	-- Then handle URLs not wrapped in brackets
	{
		-- Before
		--regex = '\\b\\w+://\\S+[)/a-zA-Z0-9-]+',
		--format = '$0',
		-- After
		regex = "[^(]\\b(\\w+://\\S+[)/a-zA-Z0-9-]+)",
		format = "$1",
		highlight = 1,
	},
	-- implicit mailto link
	{
		regex = "\\b\\w+@[\\w-]+(\\.[\\w-]+)+\\b",
		format = "mailto:$0",
	},
}

-- Customize the cursor color.
local my_colors = {
	cursor_bg = "magenta",
}

return {
	term = "wezterm",
	initial_cols = 120,
	initial_rows = 50,
	color_scheme = "Dracula (Official)",
	colors = my_colors,

	window_frame = {
		font_size = 10.0,
	},

	font = wezterm.font("JetBrains Mono"),
	font_size = 10.5,

	tab_bar_at_bottom = true,
	tab_max_width = 256,

	window_decorations = "RESIZE",
	window_background_opacity = 0.90,

	inactive_pane_hsb = {
		saturation = 0.9,
		brightness = 0.9,
	},

	disable_default_key_bindings = true,
	leader = { key = "VoidSymbol", mods = "", timeout_milliseconds = 500 },
	keys = my_keys,
	key_tables = my_key_tables,

	hyperlink_rules = my_hyperlink_rules,

	ssh_domains = {
		{
			name = "llama",
			remote_address = "llama.ai.appliedinvention.com",
			username = "jay",
		},
	},
}

-- vim: foldmethod=marker:foldlevel=10
