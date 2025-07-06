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
theme.font          = "FiraCode 6"

-- Background colors (matching XMobar)
theme.bg_normal     = "#282c34"
theme.bg_focus      = "#282c34"
theme.bg_urgent     = "#ff6c6b"
theme.bg_minimize   = "#444444"
theme.bg_systray    = theme.bg_normal

-- Foreground colors
theme.fg_normal     = "#ff6c6b"
theme.fg_focus      = "#5DFFFF"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#ffffff"

-- Border colors (matching XMonad)
theme.border_normal = "#2c698d"
theme.border_focus  = "#5DFFFF"
theme.border_marked = "#91231c"

-- Border width (matching XMonad)
theme.border_width  = dpi(2)

-- Gaps (matching XMonad spacing)
theme.useless_gap   = dpi(2)

-- Widget colors (matching XMobar widgets)
theme.widget_weather = "#98be65"
theme.widget_cpu     = "#ecbe7b"
theme.widget_mem     = "#ff6c6b"
theme.widget_disk    = "#a9a1e1"
theme.widget_uptime  = "#98be65"
theme.widget_date    = "#46d9ff"

-- Taglist colors
theme.taglist_bg_focus = "#5DFFFF"
theme.taglist_fg_focus = "#282c34"
theme.taglist_bg_urgent = "#E3411C"
theme.taglist_fg_urgent = "#ffffff"
theme.taglist_bg_occupied = "#5296F0"
theme.taglist_fg_occupied = "#282c34"
theme.taglist_bg_empty = "#282c34"
theme.taglist_fg_empty = "#666666"

-- Tasklist
theme.tasklist_bg_focus = "#282c34"
theme.tasklist_fg_focus = "#5DFFFF"

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

-- Wallpaper
theme.wallpaper = function(s)
    return "~/Downloads/wallpaper/building_city_japan_tokyo_during_nighttime_hd_travel-1920x1080.jpg"
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

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

return theme
