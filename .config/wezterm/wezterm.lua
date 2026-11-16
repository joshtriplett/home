local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local act = wezterm.action

config.check_for_updates = false
config.mux_enable_ssh_agent = false

config.enable_wayland = true

config.scrollback_lines = 100000
config.alternate_buffer_wheel_scroll_speed = 1

config.font = wezterm.font 'DejaVu Sans Mono'
config.font_size = 16

config.window_frame = {
  font = wezterm.font 'DejaVu Sans',
  font_size = 14,
}
config.window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
}
config.enable_scroll_bar = false
config.show_tab_index_in_tab_bar = false
config.show_new_tab_button_in_tab_bar = false

config.selection_word_boundary = " \t\n{}[]()\"'`:;"

local function copy_or_interrupt(window, pane)
  if window:get_selection_text_for_pane(pane) ~= '' then
    window:perform_action(act.CopyTo 'Clipboard', pane)
    window:perform_action(act.ClearSelection, pane)
  else
    window:perform_action(act.SendKey { mods = 'CTRL', key = 'c' }, pane)
  end
end

config.keys = {
  { mods = 'CTRL', key = 'c', action = wezterm.action_callback(copy_or_interrupt) },
  { mods = 'CTRL|ALT', key = 'n', action = act.SpawnTab 'CurrentPaneDomain' },
  { mods = 'ALT', key = 'LeftArrow', action = act.ActivateTabRelative(-1) },
  { mods = 'ALT', key = 'RightArrow', action = act.ActivateTabRelative(1) },
  { mods = 'CTRL|ALT', key = 'h', action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
  { mods = 'CTRL|ALT', key = 'v', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
  { mods = 'CTRL|ALT', key = 'LeftArrow', action = act.ActivatePaneDirection 'Left' },
  { mods = 'CTRL|ALT', key = 'RightArrow', action = act.ActivatePaneDirection 'Right' },
  { mods = 'CTRL|ALT', key = 'UpArrow', action = act.ActivatePaneDirection 'Up' },
  { mods = 'CTRL|ALT', key = 'DownArrow', action = act.ActivatePaneDirection 'Down' },
}
config.mouse_bindings = {
  -- Block selection with Ctrl-mouse, because Alt-mouse moves the window
  {
    event = { Down = { streak = 1, button = 'Left' } },
    mods = 'CTRL',
    action = act.SelectTextAtMouseCursor 'Block',
  },
  {
    event = { Drag = { streak = 1, button = 'Left' } },
    mods = 'CTRL',
    action = act.ExtendSelectionToMouseCursor 'Block',
  },
  {
    event = { Up = { streak = 1, button = 'Left' } },
    mods = 'CTRL',
    action = act.CompleteSelection 'ClipboardAndPrimarySelection',
  },
}

require('colors').apply(config)

local function start_maximized(cmd)
  local _, _, window = wezterm.mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end

wezterm.on('gui-startup', start_maximized)

return config
