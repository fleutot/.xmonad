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

import System.IO
import Data.Ratio ((%))

--myWorkspaces = ["1.edit", "2.term", "3.doc", "4.mail", "5.www", "6.chat", "7.priv", "8.media", "9.admin"]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myBorderWidth = 1
myNormalBorderColor = "#363534"
myFocusedBorderColor = "#dd4814"
myManageHook = composeAll
             [ className =? "Skype" --> doShift "6"
             , className =? "lcam-main" --> doFloat
             , manageDocks
             ]

skypeLayout = IM.withIM (1%7) skypeRoster Grid
skypeRoster = (IM.Title "g.ostervall_cipherstone.com - Skype™")
mySpacing = 2

myLayout = renamed [Replace "\x25eb"] (smartSpacing mySpacing $ smartBorders $ ResizableTall 1 (delta) (ratio) [])
         ||| renamed [Replace "Wide"] (smartSpacing mySpacing $ Mirror tiled)
         ||| renamed [Replace "\x25a1"] (smartBorders Full)
         ||| renamed [Replace "\x260f"] (smartSpacing 10 $ skypeLayout)  -- char
         ||| renamed [Replace "Mastered Tabbed"] (multimastered 1 (delta) (ratio) $ simpleTabbed)
         ||| renamed [Replace "\x2505"] (smartSpacing mySpacing $ ThreeCol 1 (delta) (1/3))  -- three columns
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

showVol = dzenConfig return . show

main = do
    --xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/.xmobarrc -x 1"
    xmproc <- spawnPipe "~/.cabal/bin/xmobar ~/.xmonad/.xmobarrc -x 1"
    xmonad $ withUrgencyHook NoUrgencyHook
           $ defaultConfig {
        workspaces = myWorkspaces
        , manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ smartBorders $ myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "bisque4" "" . shorten 80
                        , ppCurrent = xmobarColor "grey20" "bisque3" . wrap " " " "
                        , ppVisible = xmobarColor "grey20" "bisque4" . wrap " " " "
                        , ppHidden = xmobarColor "bisque4" "" . wrap " " " "
                        , ppHiddenNoWindows = xmobarColor "grey30" "" . wrap " " " "
                        , ppUrgent = xmobarColor "black" "#dd4814" . wrap ">" "<"
                        , ppSort = getSortByXineramaRule
                        , ppLayout = xmobarColor "bisque3" ""
                        , ppWsSep = "" -- if the font has it: " │ ". See .xmobarrc template as well.
                        , ppSep = "   ·   " -- if the font has it: " ║ "
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        } `additionalKeys`
        [ ((mod4Mask .|. controlMask, xK_l), spawn "xscreensaver-command -lock")
        , ((mod4Mask .|. controlMask, xK_z), spawn "xscreensaver-command -lock ; sudo pm-suspend --quirk-dpms-on")
        , ((controlMask .|. shiftMask, xK_Print), spawn "sleep 0.8; scrot -s ~/Pictures/Screenshot_%Y-%m-%d_%H:%M:%S.png")
        , ((controlMask, xK_Print), spawn "scrot -u ~/Pictures/Screenshot_%Y-%m-%d_%H:%M:%S.png")
        , ((0, xK_Print), spawn "scrot ~/Pictures/Screenshot_%Y-%m-%d_%H:%M:%S.png")
        , ((mod4Mask .|. mod1Mask, xK_u), runProcessWithInput "amixer" ["set", "Master", "2+"] "" >>= dzenConfig return)
        , ((mod4Mask .|. mod1Mask, xK_d), spawn "amixer set Master 2-" >>= showVol)
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
        , ((mod4Mask, xK_p), spawn "dmenu_run -fn -*-terminus-*-r-*-*-12-*-*-*-*-*-*-* -nb bisque3 -nf grey35 -sb bisque1 -sf grey10")
        ]
