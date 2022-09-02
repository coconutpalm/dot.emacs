local wezterm = require 'wezterm';

return {
   --default_prog = {"c:\\Windows\\system32\\bash.exe", "-l"},
   --color_scheme = "Spacedust",
   --color_scheme = "SeaShells",
   --color_scheme = "Peppermint",
   --color_scheme = "Neutron",
   color_scheme = "Bitmute (terminal.sexy)",
   --color_scheme = "Colorful Colors (terminal.sexy)",

   font = wezterm.font("Noto Mono"),

   exit_behavior = "Close",
   hide_tab_bar_if_only_one_tab = true,
   pane_focus_follows_mouse = true,

   inactive_pane_hsb = {
      saturation = 0.9,
      brightness = 0.8,
   },

   keys = {
      {key="|", mods="META", action=wezterm.action { SplitHorizontal={domain="CurrentPaneDomain"}}},
      {key="\\", mods="META", action=wezterm.action { SplitHorizontal={domain="CurrentPaneDomain"}}},
      {key="-", mods="META", action=wezterm.action { SplitVertical={domain="CurrentPaneDomain"}}},
      {key="_", mods="META", action=wezterm.action { SplitVertical={domain="CurrentPaneDomain"}}},

      {key="LeftArrow", mods="SUPER", action=wezterm.action { ActivatePaneDirection="Left"}},
      {key="RightArrow", mods="SUPER", action=wezterm.action { ActivatePaneDirection="Right"}},
      {key="UpArrow", mods="SUPER", action=wezterm.action { ActivatePaneDirection="Up"}},
      {key="DownArrow", mods="SUPER", action=wezterm.action { ActivatePaneDirection="Down"}},

      {key="LeftArrow", mods="META", action=wezterm.action { ActivateTabRelative=-1 }},
      {key="RightArrow", mods="META", action=wezterm.action { ActivateTabRelative=1}}
   }
}
