--
-- Desc     : A clean and well-documented xmonad configuration file (based on
--            the $HOME/.cabal/share/xmonad-0.10.1/man/xmonad.hs template file).
--
--            It uses:
--              * xmobar as a status bar,
--              * a ScratchPad (for a hidden terminal),
--              * a double IM layout (for Pidgin and Skype),
--              * a layout prompt (with auto-complete), and
--              * some convenient doFloat and moveTo{Mail, Web, IM} shortcuts.
--
import           XMonad                          hiding ((|||))

import 			 Graphics.X11.ExtraTypes.XF86

import 			 XMonad.Actions.GridSelect
import 			 XMonad.Actions.Navigation2D
import 			 XMonad.Actions.Promote
import 			 XMonad.Actions.Submap as SM
import 			 XMonad.Actions.Search as S
import 			 XMonad.Actions.SinkAll
import 			 XMonad.Actions.WindowBringer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.FadeInactive       as FI
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.UrgencyHook
import 		 	 XMonad.Layout.IndependentScreens
import           XMonad.Layout.DecorationMadness
import           XMonad.Layout.LayoutCombinators (JumpToLayout (..), (|||))
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import		     XMonad.Layout.Grid
import 		     XMonad.Layout.Drawer
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.MouseResizableTile
import           XMonad.Prompt as P
import           XMonad.Prompt.Input
import qualified XMonad.StackSet                 as W
import           XMonad.Util.Run                 (spawnPipe)
import           XMonad.Util.Scratchpad
 
import qualified Data.Map                        as M
import           Data.Ratio
import           System.Exit
import           System.IO
 
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "gnome-terminal"
 
-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 1
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod1Mask
 
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask :: KeyMask
myNumlockMask = mod2Mask

numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert]
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces = withScreens 2 [ "A", "B", "C", "D", "E", "F", "G", "H", "J", "K" ]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor  = "black"
myFocusedBorderColor = "red"
 
-- Default offset of drawable screen boundaries from each physical
-- screen. Anything non-zero here will leave a gap of that many pixels
-- on the given edge, on the that screen. A useful gap at top of screen
-- for a menu bar (e.g. 15)
--
-- An example, to set a top gap on monitor 1, and a gap on the bottom of
-- monitor 2, you'd use a list of geometries like so:
--
-- > defaultGaps = [(18,0,0,0),(0,18,0,0)] -- 2 gaps on 2 monitors
--
-- Fields are: top, bottom, left, right.
--
myDefaultGaps :: [(Integer, Integer, Integer, Integer)]
myDefaultGaps = [(0,0,0,0)]


-- Search engine bindings
searchEngineMap method = M.fromList $
	[ ((0, xK_g), method S.google)
	, ((0, xK_i), method S.images)
	, ((0, xK_s), method S.scholar)
	, ((0, xK_d), method S.dictionary)
	, ((0, xK_t), method S.thesaurus)
	, ((0, xK_m), method S.maps)
	, ((0, xK_w), method S.wikipedia)
	, ((0, xK_y), method S.youtube)
	]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask, xK_t), spawn $ XMonad.terminal conf)
 
    -- launch gmrun
    , ((modMask, xK_r), spawn "dmenu_run")
 
    -- close focused window
    , ((modMask, xK_c), kill)

	-- increase brightness
	, ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")

	-- decrease brightness
	, ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")

	-- mute volume
	, ((0, xF86XK_AudioMute), spawn "amixer -D pulse set Master toggle")

	-- increase volume
	, ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 3%+")

	-- decrease volume
	, ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 3%-")

    -- Rotate through the available layout algorithms
    , ((modMask, xK_space), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
	
	--
	, ((modMask .|. shiftMask, xK_g), goToSelected defaultGSConfig)
	
	--
	, ((modMask, xK_g), gotoMenu)

	-- 
	, ((modMask, xK_b), bringMenu)

    -- Move focus to the next window
    , ((modMask, xK_Tab), windows W.focusDown)
    
	-- Move focus to the previous window
    , ((modMask .|. shiftMask, xK_Tab), windows W.focusUp)

	-- move focus left
	, ((modMask, xK_h), windowGo L False)

	-- move focus down
    , ((modMask, xK_j), windowGo D False) 

    -- move focus up
    , ((modMask, xK_k), windowGo U False)

	-- move focus left
	, ((modMask, xK_l), windowGo R False)
	
	-- move window left
	, ((modMask .|. shiftMask, xK_h), windowSwap L False)

	-- move window down
    , ((modMask .|. shiftMask, xK_j), windowSwap D False) 

    -- move window down
    , ((modMask .|. shiftMask, xK_k), windowSwap U False)

	-- move window right
	, ((modMask .|. shiftMask, xK_l), windowSwap R False)

    -- Move focus to the master window
    , ((modMask, xK_m), windows W.focusMaster)
 
    -- Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_m), windows W.swapMaster)
 	
	-- promote window to master or swap master with next window
	, ((modMask, xK_Return), promote)

    -- Shrink the master area
    , ((modMask, xK_u), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask, xK_p), sendMessage Expand)
 
    -- Shrink the master area
    , ((modMask, xK_o), sendMessage ShrinkSlave)
 
    -- Expand the master area
    , ((modMask, xK_i), sendMessage ExpandSlave)

	-- go left screen
	, ((modMask, xK_Left), screenGo L False)

	-- go down screen
	, ((modMask, xK_Down), screenGo D False)

	-- go up screen
	, ((modMask, xK_Up), screenGo U False)

	-- go right screen
	, ((modMask, xK_Right), screenGo R False)
	
	-- move to left screen
	, ((modMask .|. shiftMask, xK_Left), windowToScreen L False)

	-- go down screen
	, ((modMask .|. shiftMask, xK_Down), windowToScreen D False)

	-- go up screen
	, ((modMask .|. shiftMask, xK_Up), windowToScreen U False)

	-- go right screen
	, ((modMask .|. shiftMask, xK_Right), windowToScreen R False)

	--
	, ((modMask, xK_s), SM.submap $ searchEngineMap $ S.promptSearch defaultXPConfig)

    -- Push window back into tiling
    , ((modMask .|. shiftMask, xK_s), withFocused $ windows . W.sink)

	-- Push window back into tiling
    , ((modMask .|. controlMask .|. shiftMask, xK_s), sinkAll)

    -- Increment the number of windows in the master area
    , ((modMask, xK_comma), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
 
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q), io exitSuccess)
 
    -- Restart xmonad
    , ((modMask, xK_q), restart "xmonad" True)

    -- Toggle struts
    , ((modMask, xK_apostrophe), sendMessage ToggleStruts)
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [ ((m .|. modMask, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) ([xK_1 .. xK_9] ++ [xK_0])
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++
 
    --
    -- Switch/move screen wtih keypad 
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_KP_Down, xK_KP_Begin, xK_KP_Page_Down, xK_KP_End] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
 
    ++
 
    --
    -- Switch/move screen with F1-F9 
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_F1 .. xK_F9] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
 
    ++
    --
    [((modMask .|. controlMask, xK_space), myLayoutPrompt)
 
    --
    , ((modMask, xK_f), spawn "nautilus --no-desktop")
 
    --
    -- , ((modMask, xK_y), focusUrgent)
 
    -- lock screen
    , ((modMask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock")
    ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask .|. shiftMask, button1), \w -> focus w >> mouseMoveWindow w)
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask .|. shiftMask, button2), \w -> focus w >> windows W.swapMaster)
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask .|. shiftMask, button3), \w -> focus w >> mouseResizeWindow w)
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
 
myLayout = smartBorders $ standardLayouts
  where
    standardLayouts = wide ||| tall ||| full
    wide = named "wide" $ avoidStruts $ mouseResizableTile {
		masterFrac = (618/1000),
        fracIncrement = (3/100),
        draggerType = BordersDragger
    }
    tall = named "tall" $ avoidStruts $ Mirror $ mouseResizableTile {
		masterFrac = (618/1000),
        fracIncrement = (3/100),
        draggerType = BordersDragger
    }
    full = named "full" $ avoidStruts $ Full

-- Set up the Layout prompt
myLayoutPrompt :: X ()
myLayoutPrompt = inputPromptWithCompl myXPConfig "Layout"
                 (mkComplFunFromList' allLayouts) ?+ (sendMessage . JumpToLayout)
  where
    allLayouts = ["wide", "tall", "full"]

    myXPConfig :: XPConfig
    myXPConfig = defaultXPConfig {
        autoComplete= Just 1000
    }
 
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
myManageHook :: ManageHook
myManageHook = scratchpadManageHookDefault <+> manageDocks
               <+> fullscreenManageHook <+> myFloatHook
               <+> manageHook defaultConfig
  where fullscreenManageHook = composeOne [ isFullscreen -?> doFullFloat ]
 
myFloatHook = composeAll
	[
		--className =? "Nautilus"                  --> doFloat
	]
    --[ className =? "GIMP"                  --> doFloat
    --, className =? "feh"                   --> doFloat
    --, className =? "Iceweasel"             --> moveToWeb
    --, className =? "Emacs"                 --> moveToCode
    --, className =? "Icedove"               --> moveToMail
    --, className =? "MPlayer"               --> moveToMedia
    --, className =? "Pidgin"                --> moveToIM
    --, classNotRole ("Pidgin", "")          --> doFloat
    --, className =? "Skype"                 --> moveToIM
    --, classNotRole ("Skype", "MainWindow") --> doFloat
    --, className =? "Gajim"                 --> moveToIM
    --, classNotRole ("Gajim", "roster")     --> doFloat
    --, manageDocks]
  where
    --moveTo2  = doF $ W.shift "2"
    --moveToIM    = doF $ W.shift "8.irc"
    --moveToWeb   = doF $ W.shift "1.web"
    --moveToMedia = doF $ W.shift "7.media"
    --moveToCode  = doF $ W.shift "2.code"
 
    --classNotRole :: (String, String) -> Query Bool
    --classNotRole (c,r) = className =? c <&&> role /=? r
 
    --role = stringProperty "WM_WINDOW_ROLE"
 
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = return ()
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0.8
 
------------------------------------------------------------------------
-- Default Config
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
   -- simple stuff
      terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
 
   -- key bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
 
   -- hooks, layouts
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , logHook            = myLogHook
    , startupHook        = myStartupHook
}
 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = do
     xmproc <- spawnPipe "`which xmobar` ~/.xmonad/xmobarrc"
     xmonad $ withUrgencyHook NoUrgencyHook defaults {
         logHook = do FI.fadeInactiveLogHook 0xbbbbbbbb
                      dynamicLogWithPP $ xmobarPP {
                            ppOutput = hPutStrLn xmproc
                          , ppTitle  = xmobarColor "#ff66ff" "" . shorten 50
                          , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                      }
     }
