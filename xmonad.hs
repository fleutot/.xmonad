import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ResizableTile
import XMonad.Util.Dzen
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myWorkspaces = [" 1.edit ", " 2.term ", " 3.doc ", " 4.mail ", " 5.www ", " 6.chat ", " 7.priv ", " 8.media ", " 9.admin "]
myBorderWidth = 2
myNormalBorderColor = "grey25"
myFocusedBorderColor = "#dd4814"
myManageHook = composeAll
             [ className =? "Skype" --> doShift "6.chat"
             , manageDocks
             ]
myLayout = (ResizableTall 1 (3/100) (1/2) []) ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

showVol = dzenConfig return . show

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/gauthier/.xmonad/.xmobarrc"
    xmonad $ defaultConfig {
        workspaces = myWorkspaces
        , manageHook = myManageHook <+> manageHook defaultConfig
        -- , layoutHook = avoidStruts  $  layoutHook defaultConfig -- old default
        , layoutHook = avoidStruts  $ myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#547D5D" "" . shorten 80
                        , ppCurrent = xmobarColor "grey20" "bisque3"
                        , ppVisible = xmobarColor "grey20" "bisque4"
                        , ppUrgent = xmobarColor "black" "red" . wrap "<<" ">>"
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
        ]


