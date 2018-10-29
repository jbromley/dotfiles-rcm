import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Themes
import XMonad.Layout.Decoration
import XMonad.Layout.Square
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Graphics.X11.ExtraTypes.XF86

myManageHook = composeAll
    [ className =? "Spotify" --> doFloat
    , className =? "HTop" --> doFloat
    ]

myTabConfig = def { activeBorderColor = "#0000FF"
                  , activeColor = "#000040"
                  , urgentBorderColor = "#00A5FF"
                  , urgentColor = "#002940"
                  , fontName = "xft:DejaVu Sans Mono-8"
                  }

myTallLayout = Tall 1 (1/100) (3/5)
myTabbedLayout = tabbed shrinkText (theme kavonLakeTheme)
myLayout = myTallLayout ||| myTabbedLayout ||| Full
              
main = do
    xmproc <- spawnPipe "/home/jay/.cabal/bin/xmobar /home/jay/.config/xmobar/xmobarrc"
    xmonad $ desktopConfig
        { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = desktopLayoutModifiers $ myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppCurrent = xmobarColor "cyan" "" . wrap "[" "]"
                        , ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "white" "" . shorten 50
                        }
        , modMask = mod4Mask
        , borderWidth = 2
        , focusedBorderColor = "#0000FF"
        , terminal = "urxvtc -e tmux"
        } `additionalKeys`
        [ ((mod4Mask, xK_p), spawn "rofi -show run")
        , ((mod4Mask, xK_b), spawn "firefox")
        , ((mod4Mask, xK_d), spawn "evince")
        , ((mod4Mask, xK_f), spawn "nautilus --no-desktop")
        , ((mod4Mask .|. controlMask, xK_g), gotoMenu)
        , ((mod4Mask .|. controlMask, xK_b), bringMenu)
        , ((mod4Mask .|. controlMask, xK_l), spawn "i3lock -e -i /tmp/lockscreen.png")
        , ((0, xF86XK_AudioPlay), spawn "/home/jay/.local/bin/spcli play")
        , ((0, xF86XK_AudioNext), spawn "/home/jay/.local/bin/spcli next")
        , ((0, xF86XK_AudioPrev), spawn "/home/jay/.local/bin/spcli prev")
        , ((0, xF86XK_AudioLowerVolume), spawn "/home/jay/.local/bin/pavol down")
        , ((0, xF86XK_AudioRaiseVolume), spawn "/home/jay/.local/bin/pavol up")
        , ((0, xF86XK_AudioMute), spawn "/home/jay/.local/bin/pavol mute")
        , ((0, xF86XK_HomePage), spawn "/home/jay/.local/bin/xhtop")
        ]
