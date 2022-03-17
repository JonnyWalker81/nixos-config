--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import System.Exit
import System.IO

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as SS
import XMonad.Layout.MultiToggle as MT
import XMonad.Layout.SimpleFloat
import XMonad.Layout.WindowArranger
import XMonad.Layout.Named
import XMonad.Layout.Magnifier
import XMonad.Layout.Grid
import XMonad.Layout.BoringWindows hiding (Merge)
import XMonad.Layout.Spacing
import XMonad.Layout.Column
import XMonad.Layout.ComboP
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.ComboP
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.WindowNavigation

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce(spawnOnce)
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
-- myTerminal      = "alacritty"
myTerminal      = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myWorkspaces    = ["emacs", "web", "services", "misc"] ++ map show ["5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#E0E0E2"
myFocusedBorderColor = "#567568"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    -- , ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm,               xK_p     ), spawn "rofi -show run")
    , ((modm,               xK_w     ), spawn "rofi -show window")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask             , xK_m     ), spawn "xmobar -x 0 ~/.config/xmobar/.xmobarrc")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
-- Layouts:
myTabTheme = defaultTheme { decoHeight = 28
                   , activeColor = "#333333"
                   , inactiveColor = "#151515"
                   , activeBorderColor = "#4B5054"
                   , inactiveBorderColor = "#4B5054"
                   , activeTextColor = "#ebac54"
                   , inactiveTextColor = "#666666"
                   }

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

-- myLayout = servicesLayout $ (tiled ||| Mirror tiled ||| Full |||  grid)
--   where
--      -- default tiling algorithm partitions the screen into two panes
--      -- tiled   = gaps [(U,5), (D,5), (L,5), (R,5)] $ smartBorders $ Tall nmaster delta ratio
--      tiled   = spacing 5 $ smartBorders $ Tall nmaster delta ratio

--      -- The default number of windows in the master pane
--      nmaster = 1

--      -- Default proportion of screen occupied by master pane
--      ratio   = 1/2

--      -- Percent of screen to increment by when resizing panes
--      delta   = 3/100

--      grid = spacing 5 $ Grid

--      servicesLayout = spacing 5 $ onWorkspace "services" Grid


myLayout = avoidStruts
    -- $ onWorkspace "Programming" layout_toggle_emacs
    -- $ onWorkspace "Browsering" layout_toggle_browse
    -- $ onWorkspace "Temporary" layout_magnify_circle
    $ onWorkspace "emacs" layout_full
    $ onWorkspace "web" layout_full
    $ onWorkspace "services" layout_grid
    $ layout_toggle
  where

  -- layout specific variables

    -- basic information
    goldenRatio = 233 / 377
    magStep = toRational (1+goldenRatio)
    ratio12 = 1 / 2
    ratio45 = 4 / 5
    delta = 3 / 100
    nmaster = 1

    -- basic layouts
    layout_grid = spacing 5 $ Grid
    layout_tall = spacing 5 $ Tall nmaster delta ratio12
    layout_mirror_tall = spacing 3 $ Mirror $ Tall nmaster delta ratio12
    layout_circle = Circle
    layout_full = Full
    layout_tabup = tabbed shrinkText myTabTheme
    layout_tabs = (layout_tabup *//* layout_tabup)
    layout_magnify_grid = spacing 5 $ windowArrange $ magnifiercz' magStep $ MT.mkToggle (REFLECTX ?? EOT) $ MT.mkToggle (REFLECTY ?? EOT) $ Grid
    layout_magnify_circle = spacing 5 $ windowArrange $ magnifiercz' magStep $ MT.mkToggle (REFLECTX ?? EOT) $ MT.mkToggle (REFLECTY ?? EOT) $ Circle


    -- cominbation layouts
    -- layout_trinity_www = spacing 3 $combineTwoP (TwoPane delta goldenRatio) (Full) (layout_tabs) (ClassName "Google-chrome")
    -- layout_trinity_emacs = spacing 3 $ combineTwoP (TwoPane delta goldenRatio) (Full) (layout_tabs) (ClassName "Emacs")
    -- layout_trinity_term = spacing 3 $ combineTwoP (TwoPane delta goldenRatio) (Full) (layout_tabs) (ClassName "URxvt")
    layout_trinity_col = spacing 5 $ ThreeColMid nmaster delta ratio12
    -- layout_toggle_trinity = toggleLayouts Full (layout_trinity_col ||| layout_trinity_www ||| layout_trinity_term ||| layout_trinity_emacs ||| Full)

    -- workspace layouts
    layout_emacs = spacing 5 $ Mirror $ Tall nmaster delta ratio45
    layout_browse = spacing 5 $ Tall nmaster delta ratio45
    layout_toggle_emacs = toggleLayouts Full (layout_emacs ||| layout_magnify_grid ||| layout_tall)
    layout_toggle_browse = toggleLayouts Full (layout_browse ||| layout_magnify_grid ||| layout_tall)

    -- toggle layouts
    layout_toggle1 = toggleLayouts Full (layout_grid ||| layout_magnify_grid ||| layout_mirror_tall)
    layout_toggle2 = toggleLayouts Full (layout_mirror_tall||| layout_grid ||| layout_magnify_grid)

    layout_toggle = toggleLayouts layout_tall (layout_tall  ||| layout_full ||| layout_tabup ||| layout_circle ||| layout_grid ||| layout_mirror_tall ||| layout_magnify_circle ||| layout_magnify_grid ||| layout_trinity_col ||| simpleFloat)


------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty


xmobarTitleColor = "#FFB6B0"

xmobarCurrentWorkspaceColor = "#CEFFAC"
------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = dynamicLogWithPP $ myXmobarPP myXmobar 

-- dynamicLogWithPP $ xmobarPP {
--  --     ppOutput = hPutStrLn xmproc
--     ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
--     , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
--     , ppSep = "   "
--   }

myLogHook h = dynamicLogWithPP $ def 
  { ppOutput = hPutStrLn h
   , ppCurrent = xmobarColor "#21E31C" "" . wrap "{" "}"
   , ppHidden = xmobarColor "#5296F0" ""
   , ppHiddenNoWindows = xmobarColor "#f8080" ""
   , ppTitle = xmobarColor "ffffff" "" . shorten 60
   , ppUrgent = xmobarColor "#E3411C" ""
  }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
-- By default, do nothing.

myStartupHook = do
  spawnOnce "feh --bg-scale ~/Downloads/wallpaper/GRDZSqf-most-popular-computer-wallpaper.jpg &"
  spawnOnce "picom &"
  spawnOnce "greenclip daemon &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/.xmobarrc"
  xmonad $ docks $  defaults xmproc
--  xmonad =<< statusBar cmd pp kb conf
--    where
--     uhook = withUrgencyHookC NoUrgencyHook urgentConfig
--     cmd = "bash -c \"tee >(xmobar -x 0 ~/.config/xmobar/xmobarrc)\""
--     pp = customPP
--     kb = toggleStrutsKey
--     conf = uhook defaults


urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont } 

customPP = defaultPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults h = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts $  myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook h,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
