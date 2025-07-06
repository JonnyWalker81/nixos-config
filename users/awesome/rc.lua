-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_configuration_dir() .. "theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "ghostty"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 = Super key, Mod1 = Alt.
modkey = "Mod1"  -- Alt key to match XMonad

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,              -- tall
    awful.layout.suit.tile.bottom,       -- grid-like
    awful.layout.suit.max,               -- monocle
    awful.layout.suit.floating,          -- floats
    awful.layout.suit.magnifier,         -- oneBig
    -- We'll need custom layouts for threeCol, threeColMid, threeRow, space
}
-- }}}

-- {{{ Helper functions for custom layouts
-- Three column layout
local function three_column(screen, t)
    local p = awful.tag.getproperty(t, "layout_three_column") or {
        ncol = 3,
        master_width_factor = 0.33
    }
    
    local wa = screen.workarea
    local cls = awful.client.visible(screen, t)
    
    if #cls == 0 then return end
    
    local width = wa.width / p.ncol
    
    for i, c in ipairs(cls) do
        local g = {}
        g.width = width - 2 * c.border_width
        g.height = wa.height - 2 * c.border_width
        g.x = wa.x + (i - 1) * width
        g.y = wa.y
        
        c:geometry(g)
    end
end

-- Three column mid layout (master in middle)
local function three_column_mid(screen, t)
    local wa = screen.workarea
    local cls = awful.client.visible(screen, t)
    
    if #cls == 0 then return end
    
    if #cls == 1 then
        -- Single client takes full screen
        local c = cls[1]
        local g = {}
        g.width = wa.width - 2 * c.border_width
        g.height = wa.height - 2 * c.border_width
        g.x = wa.x
        g.y = wa.y
        c:geometry(g)
    elseif #cls == 2 then
        -- Two clients split screen
        local width = wa.width / 2
        for i, c in ipairs(cls) do
            local g = {}
            g.width = width - 2 * c.border_width
            g.height = wa.height - 2 * c.border_width
            g.x = wa.x + (i - 1) * width
            g.y = wa.y
            c:geometry(g)
        end
    else
        -- Three or more: master in middle
        local side_width = wa.width * 0.25
        local mid_width = wa.width * 0.5
        
        -- Middle (master) client
        local master = cls[1]
        local g = {}
        g.width = mid_width - 2 * master.border_width
        g.height = wa.height - 2 * master.border_width
        g.x = wa.x + side_width
        g.y = wa.y
        master:geometry(g)
        
        -- Left side clients
        local left_count = math.floor((#cls - 1) / 2)
        if left_count > 0 then
            local height = wa.height / left_count
            for i = 1, left_count do
                local c = cls[i + 1]
                g = {}
                g.width = side_width - 2 * c.border_width
                g.height = height - 2 * c.border_width
                g.x = wa.x
                g.y = wa.y + (i - 1) * height
                c:geometry(g)
            end
        end
        
        -- Right side clients
        local right_start = left_count + 2
        local right_count = #cls - right_start + 1
        if right_count > 0 then
            local height = wa.height / right_count
            for i = 0, right_count - 1 do
                local c = cls[right_start + i]
                g = {}
                g.width = side_width - 2 * c.border_width
                g.height = height - 2 * c.border_width
                g.x = wa.x + side_width + mid_width
                g.y = wa.y + i * height
                c:geometry(g)
            end
        end
    end
end

-- Create custom layout objects
awful.layout.suit.threeCol = {
    name = "threeCol",
    arrange = three_column,
}
awful.layout.suit.threeColMid = {
    name = "threeColMid",
    arrange = three_column_mid,
}

-- Add custom layouts to the layouts table
table.insert(awful.layout.layouts, awful.layout.suit.threeCol)
table.insert(awful.layout.layouts, awful.layout.suit.threeColMid)
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock(" %b %d %Y - (%H:%M) ")

-- Weather widget
local weather_widget = wibox.widget {
    text = " Loading weather... ",
    widget = wibox.widget.textbox,
}

-- Update weather every 6 minutes
gears.timer {
    timeout = 360,
    call_now = true,
    autostart = true,
    callback = function()
        awful.spawn.easy_async_with_shell("~/scripts/wttr.sh 2>/dev/null || echo 'N/A'", function(stdout)
            weather_widget.text = " " .. stdout:gsub("\n", "") .. " "
        end)
    end
}

-- CPU widget
local cpu_widget = wibox.widget {
    text = " cpu: 0% ",
    widget = wibox.widget.textbox,
}

-- Update CPU every 2 seconds
gears.timer {
    timeout = 2,
    call_now = true,
    autostart = true,
    callback = function()
        awful.spawn.easy_async_with_shell("top -bn1 | grep 'Cpu(s)' | awk '{print $2}' | cut -d'%' -f1", function(stdout)
            local cpu = tonumber(stdout) or 0
            cpu_widget.text = string.format(" cpu: (%d%%) ", cpu)
        end)
    end
}

-- Memory widget
local mem_widget = wibox.widget {
    text = " mem: 0M (0%) ",
    widget = wibox.widget.textbox,
}

-- Update memory every 2 seconds
gears.timer {
    timeout = 2,
    call_now = true,
    autostart = true,
    callback = function()
        awful.spawn.easy_async_with_shell("free -m | awk 'NR==2{printf \"%d\", $3}'", function(used)
            awful.spawn.easy_async_with_shell("free -m | awk 'NR==2{printf \"%.0f\", $3*100/$2}'", function(percent)
                mem_widget.text = string.format(" mem: %sM (%s%%) ", used:gsub("\n", ""), percent:gsub("\n", ""))
            end)
        end)
    end
}

-- Disk widget
local disk_widget = wibox.widget {
    text = " hdd: 0G free ",
    widget = wibox.widget.textbox,
}

-- Update disk every 60 seconds
gears.timer {
    timeout = 60,
    call_now = true,
    autostart = true,
    callback = function()
        awful.spawn.easy_async_with_shell("df -h / | awk 'NR==2{print $4}'", function(stdout)
            disk_widget.text = " hdd: " .. stdout:gsub("\n", "") .. " free "
        end)
    end
}

-- Uptime widget
local uptime_widget = wibox.widget {
    text = " uptime: 0d 0h ",
    widget = wibox.widget.textbox,
}

-- Update uptime every 60 seconds
gears.timer {
    timeout = 60,
    call_now = true,
    autostart = true,
    callback = function()
        awful.spawn.easy_async_with_shell("uptime -p | sed 's/up //' | sed 's/ hours\\?/h/g' | sed 's/ days\\?/d/g' | sed 's/ minutes\\?/m/g' | sed 's/, / /g'", function(stdout)
            uptime_widget.text = " uptime: " .. stdout:gsub("\n", "") .. " "
        end)
    end
}

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  c:emit_signal(
                                                      "request::activate",
                                                      "tasklist",
                                                      {raise = true}
                                                  )
                                              end
                                          end),
                     awful.button({ }, 3, function()
                                              awful.menu.client_list({ theme = { width = 250 } })
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table with XMonad workspace names
    awful.tag({ "coding", "web", "services", "work", "misc", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    
    -- Create layout name widget
    s.mylayoutname = wibox.widget {
        widget = wibox.widget.textbox,
        align = "center"
    }
    
    local function update_layout_name()
        local layout = awful.layout.get(s)
        if layout then
            local name = awful.layout.getname(layout) or "unknown"
            s.mylayoutname.text = " [" .. name .. "] "
        end
    end
    
    -- Update layout name when layout changes
    awful.tag.attached_connect_signal(s, "property::layout", update_layout_name)
    awful.tag.attached_connect_signal(s, "property::selected", update_layout_name)
    update_layout_name()
    
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s, height = 28 })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.textbox("   "),
            weather_widget,
            wibox.widget.textbox("    "),
            cpu_widget,
            wibox.widget.textbox("    "),
            mem_widget,
            wibox.widget.textbox("    "),
            disk_widget,
            wibox.widget.textbox("    "),
            uptime_widget,
            wibox.widget.textbox("    "),
            mytextclock,
            wibox.widget.textbox("   "),
            s.mylayoutname,
            s.mylayoutbox,
        },
    }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = gears.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
              {description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Standard program
    awful.key({ modkey, "Shift"   }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    -- XMonad keybindings
    awful.key({ modkey,           }, "p", function () awful.spawn("rofi -show run") end,
              {description = "launch rofi", group = "launcher"}),
    awful.key({ modkey,           }, "w", function () awful.spawn("rofi -show window") end,
              {description = "window switcher", group = "launcher"}),
    awful.key({ modkey,           }, "r", function () awful.spawn("rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'") end,
              {description = "clipboard manager", group = "launcher"}),
    awful.key({ modkey, "Shift"   }, "p", function () awful.spawn("gmrun") end,
              {description = "launch gmrun", group = "launcher"}),
    awful.key({ modkey, "Shift"   }, "w", function () awful.spawn("chromium") end,
              {description = "launch web browser", group = "launcher"}),
    awful.key({ modkey,           }, "v", function () awful.client.focus.byidx(0) end,
              {description = "focus master", group = "client"}),
    awful.key({ modkey,           }, "Return", function () awful.client.swap.byidx(0) end,
              {description = "swap with master", group = "client"}),
    awful.key({ modkey,           }, "q", function () awful.spawn("xmonad --recompile; xmonad --restart") end,
              {description = "restart xmonad", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "m", function () awful.spawn("xmobar -x 0 ~/.config/xmobar/.xmobarrc") end,
              {description = "restart xmobar", group = "awesome"}),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    -- XMonad window spacing keybindings
    awful.key({ modkey, "Shift"   }, "d", function () beautiful.useless_gap = math.max(0, beautiful.useless_gap - 1) end,
              {description = "decrease gaps", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "i", function () beautiful.useless_gap = beautiful.useless_gap + 1 end,
              {description = "increase gaps", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        "request::activate", "key.unminimize", {raise = true}
                    )
                  end
              end,
              {description = "restore minimized", group = "client"}),

    -- Prompt
    awful.key({ modkey },            "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"}),
    -- Menubar
    awful.key({ modkey }, "d", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"}),

    -- Toggle struts (hide/show wibar) - matching XMonad's mod+b
    awful.key({ modkey }, "b",
              function ()
                  for s in screen do
                      s.mywibox.visible = not s.mywibox.visible
                  end
              end,
              {description = "toggle wibar", group = "awesome"})
)

clientkeys = gears.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "(un)maximize", group = "client"}),
    awful.key({ modkey, "Control" }, "m",
        function (c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end ,
        {description = "(un)maximize vertically", group = "client"}),
    awful.key({ modkey, "Shift"   }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end ,
        {description = "(un)maximize horizontally", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
          "pinentry",
        },
        class = {
          "Arandr",
          "Blueman-manager",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
          "Wpa_gui",
          "veromix",
          "xtightvncviewer",
          "MPlayer",      -- From XMonad config
          "Gimp"},        -- From XMonad config

        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "ConfigManager",  -- Thunderbird's about:config.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },

    -- Set browsers to always map on the "web" workspace
    { rule = { class = "Firefox" },
      properties = { screen = 1, tag = "web" } },
    { rule = { class = "Chromium" },
      properties = { screen = 1, tag = "web" } },
    { rule = { class = "chromium" },
      properties = { screen = 1, tag = "web" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{{ Autostart applications
awful.spawn.with_shell("feh --bg-scale ~/Downloads/wallpaper/building_city_japan_tokyo_during_nighttime_hd_travel-1920x1080.jpg")
awful.spawn.with_shell("pgrep greenclip || greenclip daemon")
awful.spawn.with_shell("pgrep clipcatd || clipcatd")
awful.spawn.with_shell("pgrep emacs || emacs --daemon")
-- }}}
