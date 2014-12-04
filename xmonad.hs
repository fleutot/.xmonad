import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.IM as IM
import XMonad.Util.Dzen
import XMonad.Util.Replace
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Data.Ratio ((%))

myWorkspaces = ["1.edit", "2.term", "3.doc", "4.mail", "5.www", "6.chat", "7.priv", "8.media", "9.admin"]
myBorderWidth = 2
myNormalBorderColor = "grey25"
myFocusedBorderColor = "#dd4814"
myManageHook = composeAll
             [ className =? "Skype" --> doShift "6.chat"
             , manageDocks
             ]

-- skypeLayout = IM.withIM (1%7) (IM.Title "g.ostervall_cipherstone.com - Skype™") Grid
skypeLayout = IM.withIM (1%7) skypeRoster Grid
skypeRoster = (IM.Title "g.ostervall_cipherstone.com - Skype™")

-- (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

myLayout = renamed [Replace "Tall"] (ResizableTall 1 (delta) (ratio) [])
         ||| renamed [Replace "!Tall"] (Mirror tiled)
         ||| (Full)
         ||| renamed [Replace "Chat"] skypeLayout
         ||| TwoPane (delta) (ratio)
         ||| ThreeCol 1 (delta) (ratio)
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

showVol = dzenConfig return . show

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/gauthier/.xmonad/.xmobarrc"
    xmonad $ withUrgencyHook NoUrgencyHook
           $ defaultConfig {
        workspaces = myWorkspaces
        , manageHook = myManageHook <+> manageHook defaultConfig
        -- , layoutHook = avoidStruts  $  layoutHook defaultConfig -- old default
        , layoutHook = avoidStruts  $ myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#547D5D" "" . shorten 80
                        , ppCurrent = xmobarColor "grey20" "bisque3" . wrap " " " "
                        , ppVisible = xmobarColor "grey20" "bisque4" . wrap " " " "
                        , ppHidden = xmobarColor "bisque4" "" . wrap " " " "
                        , ppHiddenNoWindows = xmobarColor "grey30" "" . wrap " " " "
                        , ppUrgent = xmobarColor "black" "red" . wrap ">" "<"
                        , ppWsSep = "" -- if the font has it: " │ ". See .xmobarrc template as well.
                        , ppSep = "   *   " -- if the font has it: " ║ "
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        } `additionalKeys`
        [ ((mod4Mask .|. controlMask, xK_l), spawn "xscreensaver-command -lock")
        , ((mod4Mask .|. controlMask, xK_z), spawn "xscreensaver-command -lock ; sudo pm-suspend --quirk-dpms-on")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask .|. mod1Mask, xK_u), runProcessWithInput "amixer" ["set", "Master", "2+"] "" >>= dzenConfig return)
        , ((mod4Mask .|. mod1Mask, xK_d), spawn "amixer set Master 2-" >>= showVol)
        -- focus urgent window
        , ((mod4Mask, xK_u), focusUrgent)
        , ((mod4Mask .|. shiftMask, xK_h), sendMessage MirrorShrink)
        , ((mod4Mask .|. shiftMask, xK_l), sendMessage MirrorExpand)
        , ((controlMask .|. mod1Mask, xK_equal), kill)
        ]


