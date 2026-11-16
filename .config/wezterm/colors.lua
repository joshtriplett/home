local wezterm = require 'wezterm'

local M = {}

local adwaita = {
  dark = {
    focused = {
      background = '#2E2E32',
      foreground = '#FFFFFF',
      hover_background = '#3D3D40',
      hover_foreground = '#FFFFFF',
      edge = '#4D4D51',
      border_bottom = '#1D1D22',
      active_foreground = '#FFFFFF',
    },
    unfocused = {
      background = '#222226',
      foreground = '#919193',
      hover_background = '#2A2A2E',
      hover_foreground = '#919193',
      edge = '#333336',
      border_bottom = '#16161A',
      active_foreground = '#808080',
    },
  },
  light = {
    focused = {
      background = '#FFFFFF',
      foreground = '#333338',
      hover_background = '#F1F1F1',
      hover_foreground = '#303035',
      edge = '#E0E0E1',
      border_bottom = '#E0E0E1',
      active_foreground = '#333338',
    },
    unfocused = {
      background = '#FAFAFB',
      foreground = '#969699',
      hover_background = '#F3F3F4',
      hover_foreground = '#959598',
      edge = '#EBEBEC',
      border_bottom = '#DCDCDE',
      active_foreground = '#99999B',
    },
  },
}

local function adwaita_tab_bar(is_dark, focused)
  local colors = adwaita[is_dark and 'dark' or 'light'][focused and 'focused' or 'unfocused']

  return {
    background = colors.background,
    inactive_tab_edge = colors.edge,
    inactive_tab_edge_hover = colors.edge,
    active_tab = { bg_color = is_dark and '#000000' or '#FFFFFF', fg_color = colors.active_foreground },
    inactive_tab = { bg_color = colors.background, fg_color = colors.foreground },
    inactive_tab_hover = { bg_color = colors.hover_background, fg_color = colors.hover_foreground },
    new_tab = { bg_color = colors.background, fg_color = colors.foreground },
    new_tab_hover = { bg_color = colors.hover_background, fg_color = colors.hover_foreground },
  }
end

local function colors_for(is_dark, focused)
  if is_dark then
    return {
      foreground = '#FFFFFF',
      background = '#000000',
      cursor_bg = '#FFFFFF',
      cursor_fg = '#000000',
      cursor_border = '#FFFFFF',
      selection_bg = '#FFFFFF',
      selection_fg = '#000000',
      split = '#555555',
      ansi = {
        '#000000', '#FF0000', '#00AA00', '#AAAA00',
        '#5555FF', '#FF00FF', '#00AAAA', '#AAAAAA',
      },
      brights = {
        '#757575', '#FF5555', '#55FF55', '#FFFF55',
        '#7777FF', '#FF66FF', '#55FFFF', '#FFFFFF',
      },
      tab_bar = adwaita_tab_bar(is_dark, focused),
    }
  else
    return {
      foreground = '#000000',
      background = '#FFFFFF',
      cursor_bg = '#000000',
      cursor_fg = '#FFFFFF',
      cursor_border = '#000000',
      selection_bg = '#000000',
      selection_fg = '#FFFFFF',
      split = '#CCCCCC',
      ansi = {
        '#000000', '#AA0000', '#007700', '#776600',
        '#3333CC', '#AA00AA', '#007777', '#666666',
      },
      brights = {
        '#555555', '#CC0000', '#008000', '#887500',
        '#4444DD', '#BB00BB', '#008080', '#444444',
      },
      tab_bar = adwaita_tab_bar(is_dark, focused),
    }
  end
end

local function tables_equal(a, b)
  if type(a) ~= 'table' or type(b) ~= 'table' then
    return a == b
  end
  for k, v in pairs(a) do
    if not tables_equal(v, b[k]) then
      return false
    end
  end
  for k in pairs(b) do
    if a[k] == nil then
      return false
    end
  end
  return true
end

function M.apply(config)
  -- The GUI API is unavailable when the mux server evaluates this file.
  local appearance = wezterm.gui and wezterm.gui.get_appearance() or 'Dark'
  local is_dark = appearance:find('Dark') ~= nil

  config.colors = colors_for(is_dark, true)
  config.bold_brightens_ansi_colors = 'BrightAndBold'

  local frame = config.window_frame or {}
  local theme = adwaita[is_dark and 'dark' or 'light']
  frame.active_titlebar_bg = theme.focused.background
  frame.active_titlebar_fg = theme.focused.foreground
  frame.active_titlebar_border_bottom = theme.focused.border_bottom
  frame.inactive_titlebar_bg = theme.unfocused.background
  frame.inactive_titlebar_fg = theme.unfocused.foreground
  frame.inactive_titlebar_border_bottom = theme.unfocused.border_bottom
  config.window_frame = frame

  local function update_window_colors(window)
    local is_dark = window:get_appearance():find('Dark') ~= nil
    local overrides = window:get_config_overrides() or {}
    local colors = colors_for(is_dark, window:is_focused())
    -- set_config_overrides produces another window-config-reloaded, so avoid loops
    if not tables_equal(colors, overrides.colors) then
      overrides.colors = colors
      window:set_config_overrides(overrides)
    end
  end

  wezterm.on('window-focus-changed', update_window_colors)
  wezterm.on('window-config-reloaded', update_window_colors)
end

return M
