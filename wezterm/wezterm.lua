local wezterm = require 'wezterm'
local mux = wezterm.mux

wezterm.on('gui-startup', function(cmd)
   local _, _, window = mux.spawn_window(cmd or {})
   window:gui_window():maximize()
end)

return {
   window_padding = {
      left = '1cell',
      right = '1cell',
      top = '0.5cell',
      bottom = '0.5cell'
   },
   use_ime = true,
   default_prog = { 'C:\\msys64\\usr\\bin\\zsh.exe', '-l' },
   font = wezterm.font 'Sarasa Mono SC',
   window_decorations = "INTEGRATED_BUTTONS|RESIZE",
   use_fancy_tab_bar = false,
   integrated_title_buttons = { 'Hide', 'Maximize', 'Close' },
   set_environment_variables = {
      CHERE_INVOKING = 'enabled_from_arguments',
      TERM = 'xterm-256color',
      HOME = 'D:\\chin-home'
   },
   skip_close_confirmation_for_processes_named = {
      'bash',
      'sh',
      'zsh',
      'fish',
      'tmux',
      'nu',
      'zsh.exe',
      'bash.exe',
      'cmd.exe',
      'pwsh.exe',
   },
   keys = {
      {
         key = 'd',
         mods = 'CTRL | SHIFT',
         action = wezterm.action.CloseCurrentPane { confirm = true },
      },
      {
         key = '5',
         mods = 'CTRL',
         action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
      },
      {
         key = '\'',
         mods = 'CTRL',
         action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
      },
   },
   colors = {
      -- The default text color
      foreground = '#282828',
      -- The default background color
      background = '#f8f8f8',

      -- Overrides the cell background color when the current cell is occupied by the
      -- cursor and the cursor style is set to Block
      cursor_bg = '#000000',
      -- Overrides the text color when the current cell is occupied by the cursor
      cursor_fg = '#ffffff',
      -- Specifies the border color of the cursor when the cursor style is set to Block,
      -- or the color of the vertical or horizontal bar when the cursor style is set to
      -- Bar or Underline.
      cursor_border = '#006800',

      -- the foreground color of selected text
      selection_fg = '#000000',
      -- the background color of selected text
      selection_bg = '#bdbdbd',

      -- The color of the scrollbar "thumb"; the portion that represents the current viewport
      scrollbar_thumb = '#222222',

      -- The color of the split lines between panes
      split = '#444444',

     ansi = {
         '#808080',
         '#a60000',
         '#006800',
         '#6f5500',
         '#0031a9',
         '#721045',
         '#005e8b',
         '#000000'
      },
      brights = {
         '#606060',
         '#b22222',
         '#228b22',
         '#a0522d',
         '#483d8b',
         '#a020f0',
         '#008b8b',
         '#595959'
      }
   }
}
