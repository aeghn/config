local wezterm = require 'wezterm'

return {
   default_prog = { 'C:\\msys64\\usr\\bin\\zsh.exe', '-l' },
   font = wezterm.font 'JetBrains Mono',
   color_scheme = 'zenbones',
   set_environment_variables = {
      CHERE_INVOKING = 'enabled_from_arguments',
      HOME = 'E:\\Litter',
      WENV = 'D:\\wenv'
   }
}
