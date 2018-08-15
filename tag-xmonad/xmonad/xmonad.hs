import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Square
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Graphics.X11.ExtraTypes.XF86

myManageHook = composeAll
    [ className =? "Spotify" --> doFloat
    , resource =? "util_float" --> doFloat
    ]
    
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/jay/.config/xmobar/xmobarrc"
    xmonad $ desktopConfig
        { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        -- , layoutHook = avoidStruts $ layoutHook defaultConfig
        , layoutHook = desktopLayoutModifiers $ Tall 1 (1/100) (3/5) ||| simpleTabbed ||| Full
        , logHook = dynamicLogWithPP xmobarPP
                        { ppCurrent = xmobarColor "orange" "" . wrap "[" "]"
                        , ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "white" "" . shorten 50
                        }
        , modMask = mod4Mask
        , borderWidth = 2
        , terminal = "urxvtc"
    	} `additionalKeys`
        [ ((mod4Mask, xK_b), spawn "firefox")
        , ((mod4Mask, xK_d), spawn "evince")
        , ((mod4Mask, xK_f), spawn "nautilus --no-desktop")
        , ((mod4Mask .|. controlMask, xK_g), gotoMenu)
        , ((mod4Mask .|. controlMask, xK_b), bringMenu)
        , ((mod4Mask .|. controlMask, xK_l), spawn "i3lock -c 14041e -d -e")
        , ((0, xF86XK_AudioPlay), spawn "/home/jay/.local/bin/spcli play")
        , ((0, xF86XK_AudioNext), spawn "/home/jay/.local/bin/spcli next")
        , ((0, xF86XK_AudioPrev), spawn "/home/jay/.local/bin/spcli prev")
        , ((0, xF86XK_AudioLowerVolume), spawn "/home/jay/.local/bin/pavol down")
        , ((0, xF86XK_AudioRaiseVolume), spawn "/home/jay/.local/bin/pavol up")
        , ((0, xF86XK_AudioMute), spawn "/home/jay/.local/bin/pavol mute")
        , ((0, xF86XK_HomePage), spawn "/home/jay/.local/bin/xhtop")
        ]
