{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
--import XMonad.Config.Gnome
import XMonad.Config.Xfce
import XMonad.Util.EZConfig
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Named
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.ComboP
import XMonad.Layout.Column
import XMonad.Layout.TwoPane
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders
import XMonad.Config.Desktop
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.ManageHelpers
import DBus as D
import DBus.Client as D
--import Control.Exception(catchDyn)
import Control.Monad (liftM2)
import Codec.Binary.UTF8.String as UTF8

main = do
  dbus <- D.connectSession
  getWellKnownName dbus
  xmonad $ xfceConfig
       { terminal   = "urxvt"
       , modMask    = mod4Mask
       , logHook    = dynamicLogWithPP (myPrettyPrinter dbus)
       , layoutHook = myLayoutHook
       , manageHook = myManageHook <+> manageHook xfceConfig }
       `additionalKeysP` myKeys

myManageHook = composeAll [ className =? "mplayer2"        --> viewShift "8"
                          , className =? "mplayer2"        --> doFloat
                          , className =? "Galculator"      --> doCenterFloat
                          , className =? "Xfce4-appfinder" --> doFloat
                          , className =? "Xfrun4"          --> doFloat
                          , className =? "Xfce4-notifyd"   --> doIgnore
                          , isFullscreen                   --> doFullFloat
                          ]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift

myKeys =
    [ -- Adding Backspace as Retun
      ("M-S-<Backspace>", spawn "urxvt")
    , ("M-<Backspace>"  , windows W.swapMaster)
    ]
    ++
    [
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
      ("M-" ++ mask ++ [key], action screen)
           | (key, screen)  <- zip ['w','e','r'] ([1,0] ++ [2..])
           , (action, mask) <- [(viewScreen, ""), (sendToScreen, "S-")]
    ]

myLayoutHook = desktopLayoutModifiers $
               onWorkspace "9" gimpLayout $
               ((layoutHook xfceConfig) ||| Accordion)

-- gimpLayout = withIM (0.18) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.20) (Role "gimp-dock") Full
gimpLayout = named "Gimp" $ split 0.20 Grid gimpMainAndRight (Role "gimp-toolbox")
gimpMainAndRight = split (0.73 / (0.73 + 0.20)) simpleTabbed (Column 1.6) (Role "gimp-image-window")
split x = combineTwoP (TwoPane 0.03 x)

myPrettyPrinter :: D.Client -> PP
myPrettyPrinter dbus = defaultPP
                       { ppOutput  = dbusOutput dbus
                       , ppTitle   = pangoColor "#93a1a1" . shorten 50 . pangoSanitize
                       , ppCurrent = pangoColor "#d33682" . wrap "[" "]" . pangoSanitize
                       , ppVisible = pangoColor "#6c71c4" . wrap "(" ")" . pangoSanitize
                       , ppHidden  = pangoColor "#fdf6e3" . wrap " " " "
                       , ppUrgent  = pangoColor "#dc322f"
                       , ppLayout   = const ""
                       , ppSep      = " "
                       }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
       [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
                        D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
                      }
  D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
    where
      left  = "<span foreground=\"" ++ fg ++ "\">"
      right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
    where
      sanitize '>'  xs = "&gt;" ++ xs
      sanitize '<'  xs = "&lt;" ++ xs
      sanitize '\"' xs = "&quot;" ++ xs
      sanitize '&'  xs = "&amp;" ++ xs
      sanitize x    xs = x:xs
