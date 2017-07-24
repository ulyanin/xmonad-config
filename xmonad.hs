import           XMonad

import           System.IO                    (hPutStrLn)

import           Graphics.X11.ExtraTypes.XF86
import           XMonad.Actions.CycleWS
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.DwmStyle
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.Mosaic
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ToggleLayouts
import           XMonad.Util.EZConfig         (additionalKeys)
import           XMonad.Util.Run              (spawnPipe)

defaults = def
    { terminal           = "urxvt"
    , modMask            = mod4Mask
    , workspaces         = myWorkspaces
    , layoutHook         = myLayoutHook
    , handleEventHook    = fullscreenEventHook
    , startupHook        = setWMName "LG3D"
    , borderWidth        = 1
    , normalBorderColor  = "black"
    , focusedBorderColor = "#00bfff"
    } `additionalKeys` myKeys

altMask :: KeyMask
altMask = mod1Mask

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [ ((altMask .|. controlMask, xK_l), spawn "slock")
         , ((altMask .|. controlMask, xK_Left  ), prevWS)
         , ((altMask .|. controlMask, xK_Right ), nextWS)
         , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 4%+")
         , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 4%-")
         , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10")
         , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -10")
         ]

myWorkspaces :: [String]
myWorkspaces =  ["1:web","2:dev","3:term","4:vm","5:media"] ++ map show ([6..9] :: [Int])

myTabConfig :: Theme
myTabConfig = def {
        activeColor         = "#6666cc"
      , activeBorderColor   = "#000000"
      , inactiveColor       = "#666666"
      , inactiveBorderColor = "#000000"
      , decoHeight          = 10
}



-- myLayoutHook = spacing 6 $ gaps [(U, 20)] $ toggleLayouts (noBorders Full) $
--     smartBorders $ Mirror tiled ||| mosaic 2 [3, 2]  ||| tabbed shrinkText myTabConfig
--       where
--         tiled = Tall nmaster delta ratio
--         nmaster = 1
--         delta   = 3/100
--         ratio   = 3/5


myLayoutHook = spacing 6 $ gaps[(U, 15)] $ toggleLayouts (noBorders Full) $ smartBorders $
    mosaic 2 [3,2] ||| Mirror tiled ||| Full
    where
        tiled     = Tall nmaster delta ratio
        nmaster   = 1
        delta     = 3/100
        ratio     = 3/5




xmobarTitleColor :: String
xmobarTitleColor = "#C5E384"

xmobarCurrentWorkspaceColor :: String
xmobarCurrentWorkspaceColor = "#B5B331"

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
    xmonad $ defaults {
        logHook = dynamicLogWithPP $ def {
            ppOutput       = System.IO.hPutStrLn xmproc
            , ppTitle      = xmobarColor xmobarTitleColor "" . shorten 100 . wrap " ✮ <fc=orange>" "</fc> ✮ "
            , ppCurrent    = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
            , ppSep        = "   "
            , ppWsSep      = " "
            , ppLayout     = \x -> case x of
                "Spacing 6 Mosaic"      -> "[:]"
                "Spacing 6 Mirror Tall" -> "[M]"
                "Spacing 6 Full"        -> "[ ]"
                _                       -> x
            , ppHiddenNoWindows = showNamedWorkspaces
        }
    } where showNamedWorkspaces wsId = if any (`elem` wsId) ['a'..'z']
            then pad wsId
            else ""
