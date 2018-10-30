import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO

main = do
  xmproc <- spawnPipe myStatusBar
  xmonad $ desktopConfig { terminal = myTerminal
                         , modMask = myMask
                         , layoutHook = desktopLayoutModifiers $ myLayoutHook
                         , manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
                         , logHook = dynamicLogWithPP $ myXmobarPP xmproc
                         } `additionalKeys` myKeys

myTerminal = "urxvtc"
myMask = mod4Mask
myStatusBar = "/home/jay/.local/bin/xmobar"

myLayoutHook = tiled ||| tabbedLayout ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 2 / 100
    ratio = 1 / 2
    tabbedLayout = tabbed shrinkText myTabConfig

myTabConfig = def { activeBorderColor = "#FF0000"
                  , activeColor = "#400000"
                  , urgentBorderColor = "#00A5FF"
                  , urgentColor = "#002940"
                  , fontName = "xft:Ubuntu Mono-8"
                  , decoHeight = 32
                  }

myManageHook = composeAll
    [ className =? "spotify" --> doFloat
    , resource =? "HTop" --> doFloat
    ]

myXmobarPP h = xmobarPP { ppCurrent = xmobarColor "white" "#800000" . wrap " " " "
                        , ppOutput = hPutStrLn h
                        , ppTitle = xmobarColor "white" "" . shorten 50
                        }

myKeys = [ ((myMask .|. controlMask, xK_Return), spawn "urxvtc -e tmux")
         , ((myMask, xK_p), spawn "rofi -show run")
         , ((myMask, xK_f), spawn "nautilus --no-desktop")
         , ((myMask .|. controlMask, xK_g), gotoMenu)
         , ((myMask .|. controlMask, xK_b), bringMenu)
         , ((myMask .|. controlMask, xK_l), spawn "i3lock -e -i /tmp/lockscreen.png")
         , ((0, xF86XK_AudioPlay), spawn "/home/jay/.local/bin/spcli play")
         , ((0, xF86XK_AudioNext), spawn "/home/jay/.local/bin/spcli next")
         , ((0, xF86XK_AudioPrev), spawn "/home/jay/.local/bin/spcli prev")
         , ((0, xF86XK_AudioLowerVolume), spawn "/home/jay/.local/bin/pavol down")
         , ((0, xF86XK_AudioRaiseVolume), spawn "/home/jay/.local/bin/pavol up")
         , ((0, xF86XK_AudioMute), spawn "/home/jay/.local/bin/pavol mute")
         , ((0, xF86XK_HomePage), spawn "/home/jay/.local/bin/xhtop")
        ]

