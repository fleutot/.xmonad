import XMonad hiding ((|||))
import XMonad.Actions.RotSlaves
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.IM as IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Master
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Util.Dzen
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Replace
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import System.IO
import Data.Ratio ((%))

--myWorkspaces = ["1.edit", "2.term", "3.doc", "4.mail", "5.www", "6.chat", "7.priv", "8.media", "9.admin"]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myBorderWidth = 2
myNormalBorderColor = "#37322a"
myFocusedBorderColor = "#dd4814"
myUrgentBgColor = "#dd4814"
myManageHook = composeAll
               [ className =? "Skype" --> doShift "6"
             , className =? "HipChat" --> doShift "6"
             , className =? "lcam-main" --> doFloat
             , manageDocks
             ]
myTabConfig = defaultTheme { inactiveBorderColor = myNormalBorderColor
            , inactiveColor = myNormalBorderColor
            , inactiveTextColor = "grey10"
            , activeBorderColor = "bisque3"
            , activeColor = "bisque3"
            , activeTextColor = "grey10"
            , urgentColor = myUrgentBgColor
            , urgentTextColor = "grey10"
            , urgentBorderColor = myUrgentBgColor
            , fontName = "-*-terminus-*-r-*-*-12-*-*-*-*-*-*-*"
            , decoHeight = 16
            }

-- skypeLayout = IM.withIM (1%7) skypeRoster Grid

skypeRoster = (IM.Title "gauthier.fleutot - Skype™")
mySpacing = 7

myLayout = renamed [Replace "\x25eb"] (smartSpacing mySpacing $ smartBorders $ ResizableTall 1 (delta) (ratio) [])
         ||| renamed [Replace "Wide"] (smartSpacing mySpacing $ Mirror tiled)
         ||| renamed [Replace "\x25a1"] (smartBorders Full)
	 ||| renamed [Replace "\x260f"] (smartSpacing 10 $ GridRatio (3/1))  -- char
         ||| renamed [Replace "Mastered Tabbed"] (multimastered 1 (delta) (ratio) $ tabbed shrinkText myTabConfig)
         ||| renamed [Replace "\x2505"] (smartSpacing mySpacing $ ThreeCol 1 (delta) (1/3))  -- three columns
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

showVol = dzenConfig return . show

color_fg   = "#f09f52"
color_hi_2 = "#ffd6b0"
color_hi_1 = "#ffc38a"
color_lo_1 = "#cd7d20"
color_lo_2 = "#8e5825"
color_bg   = "black"

myKeys =
       [
       ((mod4Mask .|. controlMask, xK_l), spawn "xscreensaver-command -lock")
       , ((mod4Mask .|. controlMask, xK_z), spawn "xscreensaver-command -lock ; sudo pm-suspend --quirk-dpms-on")
       , ((controlMask .|. shiftMask, xK_Print), spawn "sleep 0.8; scrot -s ~/Pictures/Screenshot_%Y-%m-%d_%H:%M:%S.png")
       , ((controlMask, xK_Print), spawn "scrot -u ~/Pictures/Screenshot_%Y-%m-%d_%H:%M:%S.png")
       , ((0, xK_Print), spawn "scrot ~/Pictures/Screenshot_%Y-%m-%d_%H:%M:%S.png")
       , ((mod4Mask .|. mod1Mask, xK_u), runProcessWithInput "amixer" ["set", "Master", "3%+"] "" >>= dzenConfig return)
       , ((mod4Mask .|. mod1Mask, xK_d), spawn "amixer set Master 3%-" >>= showVol)
       , ((0, 0x1008ff02), spawn "sudo sysbacklight up") -- keysym found with xev
       , ((0, 0x1008ff03), spawn "sudo sysbacklight down")  -- keysym found with xev
       , ((mod4Mask .|. mod1Mask, xK_n), spawn "sudo sysbacklight down")
       -- focus urgent window
       , ((mod4Mask, xK_u), focusUrgent)
       , ((mod4Mask .|. shiftMask, xK_h), sendMessage MirrorShrink)
       , ((mod4Mask .|. shiftMask, xK_l), sendMessage MirrorExpand)
       , ((controlMask .|. mod1Mask, xK_equal), kill)
       , ((mod4Mask, xK_z), sendMessage (JumpToLayout "\x25eb"))
       , ((mod4Mask, xK_x), sendMessage (JumpToLayout "Wide"))
       , ((mod4Mask, xK_f), sendMessage (JumpToLayout "\x25a1"))
       , ((mod4Mask, xK_c), sendMessage (JumpToLayout "\x260f"))
       , ((mod4Mask, xK_v), sendMessage (JumpToLayout "Mastered Tabbed"))
       , ((mod4Mask, xK_b), sendMessage (JumpToLayout "\x2505"))
       , ((mod4Mask .|. shiftMask, xK_f), sendMessage ToggleStruts)
       -- Rotate windows while keeping focus
       , ((mod4Mask .|. controlMask, xK_j), rotAllUp)
       , ((mod4Mask .|. controlMask, xK_k), rotAllDown)
       -- Dmenu with options
       , ((mod4Mask, xK_p), spawn "dmenu_run -fn -*-terminus-*-r-*-*-14-*-*-*-*-*-*-* -nb bisque3 -nf grey35 -sb bisque1 -sf grey10")
       -- Let the mouseModMask modifier re-tile a window, since my right hand is probably on my mouse.
       , ((mouseModMask, xK_t), withFocused $ windows . W.sink)
       ]
       ++
       --  for changing order of monitor output key
       [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
         | (key, sc) <- zip [xK_w, xK_e, xK_r] [0,2,1] -- was [0..] *** change to match your screen order ***
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- This makes the mouse bindings use Alt instead of Super. mod4mask is on my
-- right hand, thus making it hard to use together with the mouse.
mouseModMask    = mod1Mask
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((mouseModMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((mouseModMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((mouseModMask, button3), (\w -> focus w >> mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/.xmobarrc -x 0"
  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ defaultConfig {
        workspaces = myWorkspaces
        , terminal = "gnome-terminal"
        , manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ smartBorders $ myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor color_lo_2 "" . shorten 80
                        , ppCurrent = xmobarColor color_hi_2 "" . wrap " " " " -- color_bg color_fg . wrap " " " "
                        , ppVisible = xmobarColor color_lo_2 "" . wrap " " " " -- color_bg color_lo_2 . wrap " " " "
                        , ppHidden = const "" --xmobarColor color_lo_1 "" . wrap " " " "
                        , ppHiddenNoWindows = const "" --xmobarColor color_lo_2 "" . wrap " " " "
                        , ppUrgent = xmobarColor "black" "#dd4814" . wrap ">" "<"
                        , ppSort = getSortByXineramaRule
                        , ppLayout = xmobarColor color_fg ""
                        , ppWsSep = "" -- if the font has it: " │ ". See .xmobarrc template as well.
                        , ppSep = "  ·  " -- if the font has it: " ║ "
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , mouseBindings      = myMouseBindings
        } `additionalKeys` myKeys
