--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Word
import Data.Monoid
import System.Exit
import System.IO

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import qualified XMonad.StackSet as SS
import XMonad.Layout.MultiToggle as MT
import XMonad.Layout.SimpleFloat
import XMonad.Layout.WindowArranger
import XMonad.Layout.Renamed
import XMonad.Layout.Magnifier
-- import XMonad.Layout.Grid
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
-- import XMonad.Layout.BoringWindows hiding (Merge)
import XMonad.Layout.ResizableTile
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
import XMonad.Layout.CircleEx
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.ComboP
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WindowNavigation

import XMonad.Actions.MouseResize

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce(spawnOnce)
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
-- myTerminal      = "alacritty"
-- myTerminal      = "wezterm"
-- myTerminal      = "kitty"
myTerminal      = "ghostty"

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
myWorkspaces    = ["coding", "web", "services", "work", "misc"] ++ map show ["6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#24283B" -- Omarchy code background
-- myFocusedBorderColor = "#98c379"  -- "#567568"
myFocusedBorderColor = "#B4F9F8" -- Omarchy bright cyan

mySpace :: Integer
mySpace = 4

-- Window spacing configuration
myWindowSpacing :: Integer
myWindowSpacing = 8  -- Default spacing between windows
myLargeWindowSpacing :: Integer
myLargeWindowSpacing = 12  -- Larger spacing for 'space' layout

clipboardy :: MonadIO m => m () -- Don't question it
-- clipboardy = spawn "rofi -modi \"\63053 :greenclip print\" -show \"\63053 \" -run-command '{cmd}'"
clipboardy = spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'"
-- clipboardy = spawn "clipcat-menu"

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

    -- , ((modm,               xK_r), spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'")
    , ((modm,               xK_r), clipboardy)

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modm .|. shiftMask, xK_d), decWindowSpacing 1)
    , ((modm .|. shiftMask, xK_i), incWindowSpacing 1)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_v     ), windows W.focusMaster  )

    -- Toggle Maximize windows
    , ((modm,               xK_m     ), toggleFullscreen )

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
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

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
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

toggleFullscreen :: X ()
toggleFullscreen =
    withWindowSet $ \ws ->
    withFocused $ \w -> do
        let fullRect = W.RationalRect 0 0 1 1
        let isFullFloat = w `M.lookup` W.floating ws == Just fullRect
        windows $ if isFullFloat then W.sink w else W.float w fullRect

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

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i 0 i 0) True (Border 0 i 0 i) True


-- myTabTheme = defaultTheme { decoHeight = 28
--                    , activeColor = "#333333"
--                    , inactiveColor = "#151515"
--                    , activeBorderColor = "#4B5054"
--                    , inactiveBorderColor = "#4B5054"
--                    , activeTextColor = "#ebac54"
--                    , inactiveTextColor = "#666666"
--                    }

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
  -- $ onWorkspace "coding" monocle
  -- $ onWorkspace "web" monocle
  -- $ onWorkspace "services" grid
  -- $ onWorkspace "work" grid
  -- $ onWorkspace "misc" threeColMid
  $ mouseResize $ windowArrange $ toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT)  myDefaultLayout
             where
                 myDefaultLayout = tall ||| grid ||| threeCol ||| threeColMid ||| threeRow ||| oneBig ||| noBorders monocle ||| space ||| floats


tall          = renamed [Replace "tall"]     $ spacingWithEdge 5 $ ResizableTall 1 (3/100) (1/2) []
grid          = renamed [Replace "grid"]     $ spacingWithEdge 5 $ mkToggle (single MIRROR) $ Grid (16/10)
threeCol      = renamed [Replace "threeCol"] $ spacingWithEdge 5 $ ThreeCol 1 (3/100) (1/2)
threeColMid   = renamed [Replace "threeColMid"]  $ spacingWithEdge 5 $ ThreeColMid 1 (3/100) (1/2)
threeRow      = renamed [Replace "threeRow"] $ limitWindows 3  $ spacingWithEdge 8 $ Mirror $ mkToggle (single MIRROR) zoomRow
oneBig        = renamed [Replace "oneBig"]   $ limitWindows 6  $ spacingWithEdge 8 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5/9) (8/12)
monocle       = renamed [Replace "monocle"]  $ limitWindows 15 $ Full
space         = renamed [Replace "space"]    $ limitWindows 4  $ spacingWithEdge 12 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
floats        = renamed [Replace "floats"]   $ limitWindows 15 $ simplestFloat

-- myLayout = avoidStruts
--     -- $ onWorkspace "Programming" layout_toggle_emacs
--     -- $ onWorkspace "Browsering" layout_toggle_browse
--     -- $ onWorkspace "Temporary" layout_magnify_circle
--     $ onWorkspace "emacs" layout_full
--     $ onWorkspace "web" layout_full
--     $ onWorkspace "services" layout_grid
--     $ layout_toggle
--   where

--   -- layout specific variables

--     -- basic information
--     goldenRatio = 233 / 377
--     magStep = toRational (1+goldenRatio)
--     ratio12 = 1 / 2
--     ratio45 = 4 / 5
--     delta = 3 / 100
--     nmaster = 1

--     -- basic layouts
--     layout_grid = spacing 20 $ Grid
--     layout_tall = mySpacing' 20 $ Tall nmaster delta ratio12
--     layout_mirror_tall = spacing 10 $ Mirror $ Tall nmaster delta ratio12
--     layout_circle = Circle
--     layout_full = Full
--     layout_tabup = tabbed shrinkText myTabTheme
--     layout_tabs = (layout_tabup *//* layout_tabup)
--     layout_magnify_grid = spacing 10 $ windowArrange $ magnifiercz' magStep $ MT.mkToggle (REFLECTX ?? EOT) $ MT.mkToggle (REFLECTY ?? EOT) $ Grid
--     layout_magnify_circle = spacing 10 $ windowArrange $ magnifiercz' magStep $ MT.mkToggle (REFLECTX ?? EOT) $ MT.mkToggle (REFLECTY ?? EOT) $ Circle


--     -- cominbation layouts
--     -- layout_trinity_www = spacing 3 $combineTwoP (TwoPane delta goldenRatio) (Full) (layout_tabs) (ClassName "Google-chrome")
--     -- layout_trinity_emacs = spacing 3 $ combineTwoP (TwoPane delta goldenRatio) (Full) (layout_tabs) (ClassName "Emacs")
--     -- layout_trinity_term = spacing 3 $ combineTwoP (TwoPane delta goldenRatio) (Full) (layout_tabs) (ClassName "URxvt")
--     layout_trinity_col = spacing 10 $ ThreeColMid nmaster delta ratio12
--     -- layout_toggle_trinity = toggleLayouts Full (layout_trinity_col ||| layout_trinity_www ||| layout_trinity_term ||| layout_trinity_emacs ||| Full)

--     -- workspace layouts
--     layout_emacs = spacing 10 $ Mirror $ Tall nmaster delta ratio45
--     layout_browse = spacing 10 $ Tall nmaster delta ratio45
--     layout_toggle_emacs = toggleLayouts Full (layout_emacs ||| layout_magnify_grid ||| layout_tall)
--     layout_toggle_browse = toggleLayouts Full (layout_browse ||| layout_magnify_grid ||| layout_tall)

--     -- toggle layouts
--     layout_toggle1 = toggleLayouts Full (layout_grid ||| layout_magnify_grid ||| layout_mirror_tall)
--     layout_toggle2 = toggleLayouts Full (layout_mirror_tall||| layout_grid ||| layout_magnify_grid)

--     layout_toggle = toggleLayouts layout_tall (layout_tall  ||| layout_full ||| layout_tabup ||| layout_circle ||| layout_grid ||| layout_mirror_tall ||| layout_magnify_circle ||| layout_magnify_grid ||| layout_trinity_col ||| simpleFloat)


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
-- myEventHook = mempty


xmobarTitleColor = "#a9b1d6" -- Omarchy soft blue-gray

xmobarCurrentWorkspaceColor = "#B4F9F8" -- Omarchy bright cyan
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
   , ppCurrent = xmobarColor "#B4F9F8" "" . wrap "{" "}" -- Omarchy bright cyan
   , ppHidden = xmobarColor "#a9b1d6" "" -- Omarchy soft blue-gray
   , ppHiddenNoWindows = xmobarColor "#24283B" "" -- Omarchy darker navy
   , ppTitle = xmobarColor xmobarTitleColor "" . shorten 60
   , ppUrgent = xmobarColor "#9ECE6A" "" -- Omarchy accent green
  }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
-- By default, do nothing.

myStartupHook = do
  -- Run wallpaper setup script to ensure wallpaper is downloaded and set
  spawnOnce "sh /home/cipher/nixos-config/scripts/setup-wallpaper.sh &"
  -- Also run .fehbg to set wallpaper (created by setup script)
  spawnOnce "sh ~/.fehbg &"
  -- picom is now managed by home-manager service
  spawnOnce "greenclip daemon &"
  spawnOnce "clipcatd &"
  spawnOnce "emacs --daemon" -- emacs daemon for the emacsclient
  setWMName "LG3D" -- Helps with Java app compatibility

setTransparentHook :: Event -> X All
setTransparentHook ConfigureEvent{ev_event_type = createNotify, ev_window = id} = do
  return (All True)
  setOpacity id opacity
  return (All True) where
    opacityFloat = 1
    opacity = floor $ fromIntegral (maxBound :: Word32) * opacityFloat
    setOpacity id op = spawn $ "xprop -id " ++ show id ++ " -f _NET_WM_WINDOW_OPACITY 32c -set _NET_WM_WINDOW_OPACITY " ++ show op
setTransparentHook _ = return (All True)

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

-- customPP = defaultPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
-- Custom EWMH activation hook for performance
myEwmhActivateHook :: ManageHook
myEwmhActivateHook = composeOne
    [ className =? "Firefox" -?> doFocus
    , className =? "chromium" -?> doFocus
    , pure True -?> doFocus
    ]

defaults h = setEwmhActivateHook myEwmhActivateHook $ ewmhFullscreen $ ewmh def {
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
        handleEventHook    = handleEventHook def, -- setTransparentHook <+> handleEventHook def,
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

