import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myWorkspaces = ["1.edit", "2.term", "3.mail", "4.chat", "5.www", "6.priv", "7.media", "8.admin"]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/gauthier/.xmonad/.xmobarrc"
    xmonad $ defaultConfig {
        workspaces = myWorkspaces
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#7f9f7f" "" . shorten 80
                        , ppCurrent = xmobarColor "gold2" "" . wrap "[" "]"
                        , ppVisible = xmobarColor "orange3" "" . wrap "(" ")"
                        , ppWsSep = "  |  "
                        , ppSep = "  ||  "
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
