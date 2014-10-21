import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Dzen
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myWorkspaces = ["1.edit", "2.term", "3.doc", "4.mail", "5.www", "6.chat", "7.priv", "8.media", "9.admin"]
myBorderWidth = 2
myNormalBorderColor = "grey25"
myFocusedBorderColor = "#dd4814"
myManageHook = composeAll
             [ className =? "Skype" --> doShift "6.chat"
             , manageDocks
             ]

showVol = dzenConfig return . show

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/gauthier/.xmonad/.xmobarrc"
    xmonad $ defaultConfig {
        workspaces = myWorkspaces
        , manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#7f9f7f" "" . shorten 80
                        , ppCurrent = xmobarColor "gold2" "" . wrap "[" "]"
                        , ppVisible = xmobarColor "orange3" "" . wrap "(" ")"
                        , ppUrgent = xmobarColor "black" "red" . wrap "<<" ">>"
                        , ppWsSep = " │ "
                        , ppSep = " ║ "
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
        , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock ; sudo pm-suspend --quirk-dpms-on")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask .|. mod1Mask, xK_u), runProcessWithInput "amixer" ["set", "Master", "2+"] "" >>= dzenConfig return)
        , ((mod4Mask .|. mod1Mask, xK_d), spawn "amixer set Master 2-" >>= showVol)
        -- focus urgent window
        , ((mod4Mask, xK_u), focusUrgent)
        ]


