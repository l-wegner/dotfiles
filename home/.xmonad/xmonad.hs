--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap (submap)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab (unGrab)
import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import Data.Maybe (fromJust,isJust,Maybe(Just))
import qualified Data.Map        as M

-- Colors
cUrgent="#ff5555"
cBorderFocus="#005900"
cBorder="#777777"
cXmbCurrent="#00D000"
cXmbVisible="#0d9300"
cXmbWinCount="#0d9300"
cXmbLayoutName="#005900"
cXmbTitle="#005900"
cXmbSep="#005900"
cXmbHidden="#005900"
cXmbHiddenEmpty="#005900"

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "alacritty"

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
myModMask       = mod4Mask
myAltMask       = mod1Mask
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["main", "alt", "web", "comm", "media" ] ++ map show [6..9]
myWorkspaceIndices :: M.Map String String
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces $ map show [1..] 

clickable ws = "<action=xdotool key super+" ++ i ++">"++ws++"</action>"
   where i = fromJust $ M.lookup ws myWorkspaceIndices

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = cBorder
myFocusedBorderColor = cBorderFocus

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch rofi desktop files
    , ((modm,               xK_p     ), spawn "rofi -show drun")

    -- launch rofi commands
    , ((modm .|. shiftMask, xK_p     ), spawn "rofi -show run")

    -- cycle through workspaces
    , ((myAltMask ,         xK_Tab   ), nextWS)

    -- toggle workspaces
    , ((myAltMask .|. shiftMask, xK_Tab   ), toggleWS)

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_Tab ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_Tab ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

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

    -- move window to next WS and switch to it
    , ((modm .|. shiftMask .|. myAltMask, xK_j), shiftToNext >> nextWS)

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- move window to previous WS and switch to it
    , ((modm .|. shiftMask .|. myAltMask, xK_j),   shiftToPrev >> prevWS)

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

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    -- launch switch key layout
    , ((modm ,              xK_space ), spawn ".xmonad/switch-kb-layout.sh de us")

    -- set multimedia keys (identified with `xev`)
    -- source https://lambdablob.com/posts/xmonad-audio-volume-alsa-pulseaudio/
    , ((0, xF86XK_AudioMute),        spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    -- source https://unix.stackexchange.com/questions/439486/how-can-i-make-media-keys-work-with-i3
    , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioNext), spawn "playerctl next")
    , ((0, xF86XK_AudioPrev), spawn "playerctl previous")

    -- screenshotting
    , ((0, xK_Print), spawn "scrot \"$HOME/Pictures/Screenshot from %Y-%m-%d %H-%M-%S.png\" > $HOME/errors.log 2>&1 ")
    -- unGrab required by scrot -s
    , ((shiftMask, xK_Print), unGrab >> spawn "scrot -s  \"$HOME/Pictures/Screenshot from %Y-%m-%d %H-%M-%S.png\" > $HOME/errors.log 2>&1 ")
    , ((controlMask, xK_Print), spawn "scrot -e 'xclip -selection clipboard -t image/png -i \"$f\"; rm \"$f\"'  \"$HOME/Pictures/Screenshot from %Y-%m-%d %H-%M-%S.png\" > $HOME/errors.log 2>&1 ")
    , ((controlMask .|. shiftMask, xK_Print), unGrab >> spawn "scrot -s -e 'xclip -selection clipboard -t image/png -i \"$f\"; rm \"$f\"'  \"$HOME/Pictures/Screenshot from %Y-%m-%d %H-%M-%S.png\" > $HOME/errors.log 2>&1 ")

    -- timewarrior
    , ((modm, xK_s ) , submap . M.fromList $
       [ (( 0, xK_space ) , spawn "timew stop")
       , (( 0, xK_c )     , spawn "timew start cx")
       , (( 0, xK_w )     , spawn "timew start wc")
       , (( 0, xK_s )     , spawn "timew stop; timew start")
       ])

    -- desk light
    , ((modm , xK_d )  , submap . M.fromList $
       [ (( 0, xK_l ) , spawn "elrs" )
       ]
      )

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

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts ( tiled ||| Mirror tiled ||| Full )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

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
    , className =? "firefox"     --> doShift ( myWorkspaces !! 2 )
    , className =? "strawberry"     --> doShift ( myWorkspaces !! 4 )
    , className =? "discord"     --> doShift ( myWorkspaces !! 3 )
    , className =? "Conky"          --> doFloat
    , className =? "battle.net.exe" --> doFullFloat
--    , className =? "steam_app_*"    --> doFloat
    , className =? "steam_app_238960" --> doFullFloat

    , isFullscreen                  --> doFullFloat ]


------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = ewmhDesktopsEventHook 

------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = \xmproc -> workspaceHistoryHook >> dynamicLogWithPP xmobarPP
  { ppOutput =  hPutStrLn xmproc -- pipe output to xmobar process
  , ppTitle  = xmobarColor cXmbTitle "" . shorten 50
  --, ppCurrent = xmobarColor cXmbCurrent "" . wrap ("<box type=Bottom width=2 mb=2 color="++ cXmbCurrent ++ ">") "</box>"
  , ppCurrent = xmobarColor cXmbCurrent "" . wrap "" ""
  , ppVisible = xmobarColor cXmbVisible "" . wrap "" "" . clickable
  , ppSep =  "<fc=" ++ cXmbSep ++ "> | </fc>"
  , ppHidden = xmobarColor cXmbHidden "" . wrap "" "." . clickable
  , ppHiddenNoWindows = xmobarColor cXmbHiddenEmpty ""  . clickable
  , ppUrgent = xmobarColor cUrgent "" . wrap "" "!"
  , ppExtras  = [ windowCount ]
  , ppOrder  = \(ws:l:t:ex) -> [ws]++ex++[l,t]
  , ppLayout = xmobarColor cXmbLayoutName ""
  }

windowCount :: X (Maybe String)
windowCount = gets $ Just . xmobarColor cXmbWinCount "" . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  setWMName "LG3D"
  spawn "setxkbmap us"
  spawn "killall trayer"
  spawnOnce "nitrogen --restore &"
  spawnOnce "compton &"
  spawnOnce "nm-applet &"
  spawnOnce "pasystray &"
  spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor primary --transparent true --alpha 0 --tint 0x000000 --height 18 &")

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
  xmonad $ docks ( defaults xmproc )

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults xmobarproc = ewmh $ def {
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
        layoutHook         = smartBorders myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook xmobarproc,
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
