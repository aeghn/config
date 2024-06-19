local wezterm = require 'wezterm'

return {
window_padding = {
  left = '1cell',
  right = '1cell',
  top = '0.5cell',
  bottom = '0.5cell',
}
,

   default_prog = { 'C:\\msys64\\usr\\bin\\zsh.exe', '-l' },
   font = wezterm.font 'JetBrains Mono',
   color_scheme = 'zenbones',
   window_decorations = "INTEGRATED_BUTTONS|RESIZE",
   use_fancy_tab_bar = false,
   set_environment_variables = {
      CHERE_INVOKING = 'enabled_from_arguments'
   }
}
