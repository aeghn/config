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
   color_scheme = 'Bamboo Light',
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
      'powershell.exe'
   }
}
