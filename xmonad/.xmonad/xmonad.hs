import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import           XMonad.Actions.Navigation2D
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.NoBorders
import           XMonad.StackSet                    as W
import           XMonad.Util.EZConfig               (additionalKeys)

main :: IO ()
main = xmonad =<< statusBar myBar myPP myToggleStrutsKey myConfig

myConfig = def
  { modMask = modm
  , borderWidth = 1
  , focusedBorderColor = myColors !! 3
  , layoutHook = myLayout
  , normalBorderColor = myColors !! 8
  } `additionalKeys`
  myAdditionalKeys

altMask :: KeyMask
altMask = mod1Mask

ctrlMask :: KeyMask
ctrlMask = controlMask

modm :: KeyMask
modm = mod4Mask

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys = [ ((modm, xK_d), spawn "rofi -show run")
                   , ((modm, xK_n), spawn "networkmanager_dmenu")
                   , ((modm, xK_b), spawn "emacsclient -ca ''")
                   , ((modm, xK_c), spawn "firefox")
                   , ((modm, xK_Return), spawn "urxvt")
                   , ((0, xF86XK_Display), spawn "slimlock")
                   , ((modm, xF86XK_AudioRaiseVolume), spawn "mpc next")
                   , ((shiftMask, xF86XK_AudioLowerVolume), spawn "amixer set Master 0%")
                   , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+")
                   , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "amixer set Master 100%")
                   , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
                   , ((modm, xF86XK_AudioMute), spawn "mpc toggle")
                   , ((0, xF86XK_AudioMicMute), spawn "amixer set Capture toggle")
                   , ((modm, xF86XK_AudioLowerVolume), spawn "mpc prev")
                   , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
                   , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -5")
                   , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +5")
                   , ((shiftMask, xF86XK_MonBrightnessDown), spawn "xbacklight = 5")
                   , ((shiftMask, xF86XK_MonBrightnessUp), spawn "xbacklight = 100")

                   , ((modm .|. altMask .|. ctrlMask , xK_h), sendMessage $ ShrinkFrom L)
                   , ((modm .|. altMask .|. ctrlMask , xK_j), sendMessage $ ShrinkFrom D)
                   , ((modm .|. altMask .|. ctrlMask , xK_k), sendMessage $ ShrinkFrom U)
                   , ((modm .|. altMask .|. ctrlMask , xK_l), sendMessage $ ShrinkFrom R)
                   , ((modm .|. altMask, xK_h), sendMessage $ ExpandTowards L)
                   , ((modm .|. altMask, xK_j), sendMessage $ ExpandTowards D)
                   , ((modm .|. altMask, xK_k), sendMessage $ ExpandTowards U)
                   , ((modm .|. altMask, xK_l), sendMessage $ ExpandTowards R)
                   , ((modm .|. ctrlMask, xK_n), sendMessage SelectNode)
                   , ((modm .|. shiftMask, xK_h), windowSwap L True)
                   , ((modm .|. shiftMask, xK_j), windowSwap D True)
                   , ((modm .|. shiftMask, xK_k), windowSwap U True)
                   , ((modm .|. shiftMask, xK_l), windowSwap R True)
                   , ((modm .|. shiftMask, xK_n), sendMessage MoveNode)
                   , ((modm, xK_a), sendMessage FocusParent)
                   , ((modm, xK_h), windowGo L True)
                   , ((modm, xK_j), windowGo D True)
                   , ((modm, xK_k), windowGo U True)
                   , ((modm, xK_l), windowGo R True)
                   , ((modm, xK_r), sendMessage Rotate)
                   , ((modm, xK_s), sendMessage Swap)
                   , ((modm .|. shiftMask, xK_Return), windows W.swapMaster) ]

myBar :: String
myBar = "xmobar"

myLayout = avoidStruts . smartBorders $
  emptyBSP ||| Full

myPP :: PP
myPP = xmobarPP{ppCurrent = xmobarColor (myColors !! 4) ""
               , ppHidden = xmobarColor (myColors !! 7) ""
               , ppLayout = xmobarColor (myColors !! 5) ""
               , ppSep = " | "
               , ppTitle = xmobarColor (myColors !! 2) "" . shorten 100}

myToggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
myToggleStrutsKey XConfig{} = (0, xF86XK_LaunchA)

myColors :: [String]
myColors = [ "#1d1f21"
           , "#cc6666"
           , "#b5bd68"
           , "#f0c674"
           , "#81a2be"
           , "#b294bb"
           , "#8abeb7"
           , "#c5c8c6"
           , "#969896"
           , "#cc6666"
           , "#b5bd68"
           , "#f0c674"
           , "#81a2be"
           , "#b294bb"
           , "#8abeb7"
           , "#ffffff" ]
