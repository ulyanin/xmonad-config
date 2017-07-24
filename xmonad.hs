import XMonad
import XMonad.Layout.Fullscreen
import XMonad.Layout.Spacing
import XMonad.Hooks.SetWMName
import XMonad.Layout.Gaps
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Mosaic
import XMonad.Layout.DwmStyle
import XMonad.Layout.Tabbed
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import System.IO (hPutStrLn)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.CycleWS
import Graphics.X11.ExtraTypes.XF86


xmobarTitleColor :: String
xmobarTitleColor = "#FFB6B0"

xmobarCurrentWorkspaceColor :: String
xmobarCurrentWorkspaceColor = "#CEFFAC"

defaults = def {
          terminal                 = "urxvt"
        , workspaces               = myWorkspaces
        , modMask                  = mod4Mask
        , layoutHook               = myLayoutHook
        , handleEventHook          = fullscreenEventHook  -- для корректного отображения окон в полно экранном режиме
        , startupHook              = setWMName "LG3D"  -- для совместимости определёных приложений, java например(IntelliJ IDEA)
        , borderWidth              = 1
        , normalBorderColor        = "grey"
        , focusedBorderColor       = "orange"
        } `additionalKeys` myKeys  -- добавили к дефолтной конфигурации новые кнпоки!

altMask :: KeyMask
altMask = mod1Mask

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [ ((altMask .|. controlMask, xK_l), spawn "xscreensaver-command --lock")
         , ((altMask .|. controlMask, xK_Left  ), prevWS)
         , ((altMask .|. controlMask, xK_Right ), nextWS)
         , ((0, xF86XK_KbdBrightnessUp), spawn "asus-kbd-backlight up")
         , ((0, xF86XK_KbdBrightnessDown), spawn "asus-kbd-backlight down")
         , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 4%+")
         , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 4%-")
         , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10")
         , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -10")
         ]

myWorkspaces :: [String]
myWorkspaces =  ["1:web","2:dev","3:term","4:vm","5:media"] ++ map show ([6..9] :: [Int])

myTabConfig :: Theme
myTabConfig = def { inactiveBorderColor = "#FF0000"
                  , activeTextColor = "#00FF00"}

myLayoutHook = spacing 6 $ gaps [(U, 20)] $ toggleLayouts (noBorders Full) $
    smartBorders $ Mirror tiled ||| mosaic 2 [3, 2]  ||| tabbed shrinkText myTabConfig
      where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        delta   = 3/100
        ratio   = 3/5


main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
    xmonad $ defaults {
    logHook =  dynamicLogWithPP $ def { ppOutput = System.IO.hPutStrLn xmproc
                                            , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
                                            , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
                                            , ppSep = "   "
                                            , ppWsSep = " "
                                            , ppLayout  = \ x -> case x of
                                                "Spacing 6 Mosaic"                      -> "[:]"
                                                "Spacing 6 Mirror Tall"                 -> "[M]"
                                                "Spacing 6 Hinted Tabbed Simplest"      -> "[T]"
                                                "Spacing 6 Full"                        -> "[ ]"
                                                _                                       -> x
                                            , ppHiddenNoWindows = showNamedWorkspaces
                                            }
} where showNamedWorkspaces wsId = if any (`elem` wsId) ['a'..'z']
                                       then pad wsId
                                       else ""
