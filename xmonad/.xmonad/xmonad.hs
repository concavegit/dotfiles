{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Graphics.X11.ExtraTypes.XF86
import           Data.Either
import           Shelly
import qualified Data.Text.Read                as T
import qualified Network.MPD                   as MPD
import           XMonad
import           XMonad.Actions.Navigation2D
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.NoBorders
import           XMonad.StackSet               as W
import           XMonad.Util.EZConfig           ( additionalKeys )
import           Data.Functor

main :: IO ()
main = xmonad =<< statusBar myBar myPP myToggleStrutsKey myConfig

myConfig =
        def { modMask            = modm
            , borderWidth        = 1
            , focusedBorderColor = myColors !! 3
            , layoutHook         = myLayout
            , normalBorderColor  = myColors !! 8
            }
                `additionalKeys` myAdditionalKeys

altMask :: KeyMask
altMask = mod1Mask

ctrlMask :: KeyMask
ctrlMask = controlMask

modm :: KeyMask
modm = mod4Mask

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys =
        [ ((modm, xK_d)      , spawn "rofi -show run")
        , ((modm, xK_n)      , spawn "networkmanager_dmenu")
        , ((modm, xK_f)      , spawn "dmenu_extended_run")
        , ((modm, xK_i), spawn "dmenu_extended_run \"-> Internet search: \"")
        , ((modm, xK_b)      , spawn "emacsclient -ca ''")
        , ((modm, xK_c)      , spawn "qutebrowser")
        , ((modm, xK_Return) , spawn "urxvtc")
        , ((0, xF86XK_Search), spawn "dm-tool lock")
        , ( (modm, xF86XK_AudioRaiseVolume)
          , (liftIO . MPD.withMPD) MPD.next $> ()
          )
        , ( (shiftMask, xF86XK_AudioLowerVolume)
          , spawn "amixer -qD pulse set Master 0%"
          )
        , ( (0, xF86XK_AudioRaiseVolume)
          , spawn "amixer -qD pulse set Master 5%+"
          )
        , ( (shiftMask, xF86XK_AudioRaiseVolume)
          , spawn "amixer -qD pulse set Master 100%"
          )
        , ((0, xF86XK_AudioMute)   , spawn "amixer -qD pulse set Master toggle")
        , ((modm, xF86XK_AudioMute), (liftIO . MPD.withMPD) togglePause $> ())
        , ( (0, xF86XK_AudioMicMute)
          , spawn "amixer -qD pulse set Capture toggle"
          )
        , ( (modm, xF86XK_AudioLowerVolume)
          , (liftIO . MPD.withMPD) MPD.previous Data.Functor.$> ()
          )
        , ( (0, xF86XK_AudioLowerVolume)
          , spawn "amixer -qD pulse set Master 5%-"
          )
        , ((0, xF86XK_MonBrightnessDown), liftIO decBrightness)
        , ((0, xF86XK_MonBrightnessUp)  , liftIO incBrightness)
        , ( (shiftMask, xF86XK_MonBrightnessDown)
          , spawn "xbacklight -steps 1 -set 2 -time 0"
          )
        , ( (shiftMask, xF86XK_MonBrightnessUp)
          , spawn "xbacklight -steps 1 -set 100 -time 0"
          )
        , ((modm .|. altMask .|. ctrlMask, xK_h), sendMessage $ ShrinkFrom L)
        , ((modm .|. altMask .|. ctrlMask, xK_j), sendMessage $ ShrinkFrom D)
        , ((modm .|. altMask .|. ctrlMask, xK_k), sendMessage $ ShrinkFrom U)
        , ((modm .|. altMask .|. ctrlMask, xK_l), sendMessage $ ShrinkFrom R)
        , ((modm .|. altMask, xK_h)             , sendMessage $ ExpandTowards L)
        , ((modm .|. altMask, xK_j)             , sendMessage $ ExpandTowards D)
        , ((modm .|. altMask, xK_k)             , sendMessage $ ExpandTowards U)
        , ((modm .|. altMask, xK_l)             , sendMessage $ ExpandTowards R)
        , ((modm .|. ctrlMask, xK_n)            , sendMessage SelectNode)
        , ((modm .|. shiftMask, xK_h)           , windowSwap L True)
        , ((modm .|. shiftMask, xK_j)           , windowSwap D True)
        , ((modm .|. shiftMask, xK_k)           , windowSwap U True)
        , ((modm .|. shiftMask, xK_l)           , windowSwap R True)
        , ((modm .|. shiftMask, xK_n)           , sendMessage MoveNode)
        , ((modm, xK_a)                         , sendMessage FocusParent)
        , ((modm, xK_h)                         , windowGo L True)
        , ((modm, xK_j)                         , windowGo D True)
        , ((modm, xK_k)                         , windowGo U True)
        , ((modm, xK_l)                         , windowGo R True)
        , ((modm, xK_r)                         , sendMessage Rotate)
        , ((modm, xK_s)                         , sendMessage Swap)
        , ((modm .|. shiftMask, xK_Return)      , windows W.swapMaster)
        ]

myBar :: String
myBar = "xmobar"

myLayout = avoidStruts . smartBorders $ emptyBSP ||| Full

myPP :: PP
myPP = xmobarPP { ppCurrent = xmobarColor (myColors !! 4) ""
                , ppHidden  = xmobarColor (myColors !! 7) ""
                , ppLayout  = xmobarColor (myColors !! 5) ""
                , ppSep     = " | "
                , ppTitle   = xmobarColor (myColors !! 2) "" . shorten 100
                }

myToggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
myToggleStrutsKey XConfig{} = (modm, xF86XK_Search)

myColors :: [String]
myColors =
        [ "#1d1f21"
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
        , "#ffffff"
        ]


togglePause :: MPD.MPD ()
togglePause = MPD.stState <$> MPD.status >>= \case
        MPD.Playing -> MPD.pause True
        MPD.Paused  -> MPD.pause False
        MPD.Stopped -> pure ()

getBrightness :: IO Int
getBrightness = do
        uninstallSignalHandlers
        brightness <- shelly
                (fromRight 0 . fmap (round . fst) . T.rational <$> run
                        "/usr/bin/xbacklight"
                        ["-display", ":0.0", "-get"]
                )
        installSignalHandlers
        pure brightness

brightnessRatio :: Floating t => t
brightnessRatio = 1 / 1.61803

brightnessLevels :: [Int]
brightnessLevels = map round (iterate (* brightnessRatio) 100)

nextBrightness :: Int -> Int
nextBrightness = min 100 . last . flip takeWhile brightnessLevels . (<)

prevBrightness :: Int -> Int
prevBrightness =
        max 1 . head . flip dropWhile brightnessLevels . (<=) . fromIntegral

incBrightness :: IO ()
incBrightness =
        nextBrightness
                <$> getBrightness
                >>= spawn
                .   ("xbacklight -display :0.0 -time 0 -steps 1 -set " ++)
                .   show

decBrightness :: IO ()
decBrightness =
        prevBrightness
                <$> getBrightness
                >>= spawn
                .   ("xbacklight -display :0.0 -time 0 -steps 1 -set " ++)
                .   show
