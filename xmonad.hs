import XMonad hiding ((|||))
import XMonad.Actions.RotSlaves
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
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

import System.IO
import Data.Ratio ((%))

myWorkspaces = ["1.edit", "2.term", "3.doc", "4.mail", "5.www", "6.chat", "7.priv", "8.media", "9.admin"]
myBorderWidth = 2
myNormalBorderColor = "bisque4"
myFocusedBorderColor = "#dd4814"
myManageHook = composeAll
             [ className =? "Skype" --> doShift "6.chat"
             , manageDocks
             ]

skypeLayout = IM.withIM (1%7) skypeRoster Grid
-- skypeRoster = (IM.Title "g.ostervall_cipherstone.com - Skype™")
skypeRoster = (IM.Title "gauthier.fleutot - Skype™")

myLayout = renamed [Replace "Tall"] (smartSpacing 1 $ smartBorders $ ResizableTall 1 (delta) (ratio) [])
         ||| renamed [Replace "Wide"] (smartSpacing 1 $ Mirror tiled)
         ||| (smartBorders Full)
         ||| renamed [Replace "Chat"] (smartSpacing 10 $ skypeLayout)
         ||| renamed [Replace "Mastered Tabbed"] (multimastered 1 (delta) (ratio) $ simpleTabbed)
         ||| ThreeCol 1 (delta) (1/3)
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

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/.xmobarrc -x 1"
    xmonad $ withUrgencyHook NoUrgencyHook
           $ defaultConfig {
        workspaces = myWorkspaces
        , manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ smartBorders $ myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor color_lo_2 "" . shorten 80
                        , ppCurrent = xmobarColor color_hi_2 "" . wrap " " " "
                        , ppVisible = xmobarColor color_bg color_hi_1 . wrap " " " "
                        , ppHidden = xmobarColor color_lo_1 "" . wrap " " " "
                        , ppHiddenNoWindows = xmobarColor color_lo_2 "" . wrap " " " "
                        , ppUrgent = xmobarColor "black" "#dd4814" . wrap ">" "<"
                        -- , ppSort = getSortByXineramaRule
                        , ppLayout = xmobarColor color_fg ""
                        , ppWsSep = "" -- if the font has it: " │ ". See .xmobarrc template as well.
                        , ppSep = "  ·  " -- if the font has it: " ║ "
                        }
        , terminal = "gnome-terminal"
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        } `additionalKeys`
        [ ((mod4Mask .|. controlMask, xK_l), spawn "xscreensaver-command -lock")
        , ((mod4Mask .|. controlMask, xK_z), spawn "xscreensaver-command -lock ; sudo pm-suspend --quirk-dpms-on")
        , ((controlMask, xK_Print), spawn "sleep 0.8; scrot -s ~/Pictures/Screenshot_%Y-%m-%d_%H:%M:%S.png")
        , ((0, xK_Print), spawn "scrot ~/Pictures/Screenshot_%Y-%m-%d_%H:%M:%S.png")
        , ((mod4Mask .|. mod1Mask, xK_u), runProcessWithInput "amixer" ["set", "Master", "2+"] "" >>= dzenConfig return)
        , ((mod4Mask .|. mod1Mask, xK_d), spawn "amixer set Master 2-" >>= showVol)
        -- focus urgent window
        , ((mod4Mask, xK_u), focusUrgent)
        , ((mod4Mask .|. shiftMask, xK_h), sendMessage MirrorShrink)
        , ((mod4Mask .|. shiftMask, xK_l), sendMessage MirrorExpand)
        , ((controlMask .|. mod1Mask, xK_equal), kill)
        , ((mod4Mask, xK_z), sendMessage (JumpToLayout "Tall"))
        , ((mod4Mask, xK_x), sendMessage (JumpToLayout "Wide"))
        , ((mod4Mask, xK_f), sendMessage (JumpToLayout "Full"))
        , ((mod4Mask, xK_c), sendMessage (JumpToLayout "Chat"))
        , ((mod4Mask, xK_v), sendMessage (JumpToLayout "Mastered Tabbed"))
        , ((mod4Mask, xK_b), sendMessage (JumpToLayout "ThreeCol"))
        , ((mod4Mask .|. shiftMask, xK_f), sendMessage ToggleStruts)
        -- Rotate windows while keeping focus
        , ((mod4Mask .|. controlMask, xK_j), rotAllUp)
        , ((mod4Mask .|. controlMask, xK_k), rotAllDown)
        ]
