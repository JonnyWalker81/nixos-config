---------------------------
-- AwesomeWM Theme
-- Based on XMonad config
---------------------------

local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

local theme = {}

-- Font (matching XMobar)
theme.font          = "FiraCode Bold 7"

-- Omarchy color scheme (matching XMonad)
-- Background colors
theme.bg_normal     = "#1a1b26"  -- Omarchy dark navy (XMobar bg)
theme.bg_focus      = "#1a1b26"
theme.bg_urgent     = "#9ECE6A"  -- Omarchy accent green
theme.bg_minimize   = "#24283B"  -- Omarchy darker navy
theme.bg_systray    = theme.bg_normal

-- Foreground colors
theme.fg_normal     = "#a9b1d6"  -- Omarchy soft blue-gray
theme.fg_focus      = "#B4F9F8"  -- Omarchy bright cyan
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#a9b1d6"

-- Border colors (matching XMonad exactly)
theme.border_normal = "#24283B"  -- Omarchy dark navy
theme.border_focus  = "#B4F9F8"  -- Omarchy bright cyan
theme.border_marked = "#9ECE6A"  -- Omarchy accent green

-- Border width (matching XMonad)
theme.border_width  = dpi(2)

-- Gaps (default, will be overridden per-layout)
theme.useless_gap   = dpi(5)

-- Widget colors (matching XMobar Omarchy theme)
theme.widget_weather = "#B4F9F8"  -- cyan (matches XMobar weather)
theme.widget_cpu     = "#9ECE6A"  -- green (matches XMobar cpu)
theme.widget_mem     = "#a9b1d6"  -- soft blue-gray (matches XMobar mem)
theme.widget_disk    = "#a9b1d6"  -- soft blue-gray (matches XMobar disk)
theme.widget_uptime  = "#B4F9F8"  -- cyan (matches XMobar uptime)
theme.widget_date    = "#9ECE6A"  -- green (matches XMobar date)
theme.widget_display = "#ff9e64"  -- orange (matches XMobar display profile)

-- Taglist colors (Omarchy theme)
theme.taglist_bg_focus = "#B4F9F8"      -- bright cyan
theme.taglist_fg_focus = "#1a1b26"      -- dark background
theme.taglist_bg_urgent = "#9ECE6A"     -- accent green
theme.taglist_fg_urgent = "#1a1b26"
theme.taglist_bg_occupied = "#a9b1d6"   -- soft blue-gray
theme.taglist_fg_occupied = "#1a1b26"
theme.taglist_bg_empty = "#24283B"      -- dark navy
theme.taglist_fg_empty = "#a9b1d6"

-- Tasklist
theme.tasklist_bg_focus = "#1a1b26"
theme.tasklist_fg_focus = "#B4F9F8"

-- Titlebar
theme.titlebar_bg_focus  = theme.bg_focus
theme.titlebar_bg_normal = theme.bg_normal

-- Tooltip
theme.tooltip_font = theme.font
theme.tooltip_opacity = 0.9
theme.tooltip_fg_color = theme.fg_normal
theme.tooltip_bg_color = theme.bg_normal

-- Mouse finder
theme.mouse_finder_color = "#CC9393"
theme.mouse_finder_timeout = 10
theme.mouse_finder_animate_timeout = 0.05
theme.mouse_finder_radius = 20
theme.mouse_finder_factor = 2

-- Menu
theme.menu_height = dpi(20)
theme.menu_width  = dpi(180)
theme.menu_border_width = dpi(1)
theme.menu_border_color = theme.border_normal
theme.menu_fg_normal = theme.fg_normal
theme.menu_fg_focus = theme.fg_focus
theme.menu_bg_normal = theme.bg_normal
theme.menu_bg_focus = theme.bg_focus

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = themes_path.."default/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = themes_path.."default/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = themes_path.."default/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = themes_path.."default/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = themes_path.."default/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = themes_path.."default/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themes_path.."default/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = themes_path.."default/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themes_path.."default/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = themes_path.."default/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themes_path.."default/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = themes_path.."default/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themes_path.."default/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = themes_path.."default/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themes_path.."default/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = themes_path.."default/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themes_path.."default/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = themes_path.."default/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themes_path.."default/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = themes_path.."default/titlebar/maximized_focus_active.png"

-- Wallpaper (matching Hyprland)
theme.wallpaper = function(s)
    return os.getenv("HOME") .. "/.local/share/wallpapers/current"
end

-- You can use your own layout icons like this:
theme.layout_fairh = themes_path.."default/layouts/fairhw.png"
theme.layout_fairv = themes_path.."default/layouts/fairvw.png"
theme.layout_floating  = themes_path.."default/layouts/floatingw.png"
theme.layout_magnifier = themes_path.."default/layouts/magnifierw.png"
theme.layout_max = themes_path.."default/layouts/maxw.png"
theme.layout_fullscreen = themes_path.."default/layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path.."default/layouts/tilebottomw.png"
theme.layout_tileleft   = themes_path.."default/layouts/tileleftw.png"
theme.layout_tile = themes_path.."default/layouts/tilew.png"
theme.layout_tiletop = themes_path.."default/layouts/tiletopw.png"
theme.layout_spiral  = themes_path.."default/layouts/spiralw.png"
theme.layout_dwindle = themes_path.."default/layouts/dwindlew.png"
theme.layout_cornernw = themes_path.."default/layouts/cornernww.png"
theme.layout_cornerne = themes_path.."default/layouts/cornernew.png"
theme.layout_cornersw = themes_path.."default/layouts/cornersww.png"
theme.layout_cornerse = themes_path.."default/layouts/cornersew.png"

-- Custom layout icons for our custom layouts
theme.layout_threeCol = themes_path.."default/layouts/tileleftw.png"
theme.layout_threeColMid = themes_path.."default/layouts/fairhw.png"
theme.layout_grid = themes_path.."default/layouts/fairvw.png"
theme.layout_threeRow = themes_path.."default/layouts/tilebottomw.png"
theme.layout_space = themes_path.."default/layouts/magnifierw.png"

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

return theme
