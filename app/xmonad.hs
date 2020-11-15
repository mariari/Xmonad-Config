module Main where

import qualified Data.Monoid as Monoid

import qualified XMonad.Core                    as Core
import qualified XMonad.Actions.ShowText        as ShowText

import qualified XMonad.StackSet                as W
import qualified XMonad.Actions.GroupNavigation as GroupNav
import qualified XMonad.Util.Run                as Run
import qualified XMonad.Actions.Navigation2D    as Nav2D
import qualified System.IO                      as IO
import qualified XMonad.Hooks.DynamicLog        as DL
import qualified XMonad.Hooks.ManageDocks       as ManageDocks
import qualified XMonad.Util.SpawnOnce          as Once
import qualified XMonad.Util.EZConfig           as EZ
import qualified XMonad.Layout.Fullscreen       as Full
import qualified XMonad.Hooks.UrgencyHook       as Urgency
import XMonad

import qualified UrgencyHook
import qualified Layouts
import qualified Configuration as Config
import qualified Keys

-- Rules which are applied to each new window.  The (optional) part before
-- '-->' is a matching rule.  The rest is an action to perform.
myManageHook :: Query (Monoid.Endo WindowSet)
myManageHook = composeAll
  [ resource =? "Do"       --> doIgnore          -- Leave Gnome Do alone.
  , resource =? "Pidgin"   --> doShift Config.w3 -- Force to IM workspace.
  , resource =? "skype"    --> doShift Config.w7 -- Force to Skype workspace.
  , resource =? "gimp-2.6" --> doShift Config.w8 -- Special-case the GIMP.
  , resource =? "tilda"    --> doFloat
  , resource =? "guake"    --> doFloat
  , ManageDocks.manageDocks                       -- For xmobar
  ]


foo :: X ()
foo = do
  state <- get
  let windowSet = Core.windowset state
  ShowText.flashText def 10000 (show (W.allWindows windowSet))


----------------------------------------------------------------
-- HOOKS
----------------------------------------------------------------

-- Print the current xmonad status to a pipe for display by xmobar.
-- Instead of using the usual xmobarPP configuration, we use defaultPP and
-- override the colors to match the Ubuntu 10.10 "Clearlooks" theme.
-- Note that we assume "position = Bottom" appears in your xmobar config,
-- and that the Gnome bottom panel has been removed.
--
myLogHook :: IO.Handle -> X ()
myLogHook xmobarPipe = DL.dynamicLogWithPP xmobarPrinter
  where
    xmobarPrinter = def
      { DL.ppOutput  = IO.hPutStrLn xmobarPipe
      , DL.ppCurrent = DL.xmobarColor "black" Config.themeHighlight . DL.wrap "[" "]"
      , DL.ppTitle   = DL.xmobarColor "pink" "" . DL.shorten 50
      , DL.ppVisible = DL.wrap "(" ")"
      , DL.ppUrgent  = DL.xmobarColor "pink" Config.themeHighlight }


myHandleEventHook :: Event -> X Monoid.All
myHandleEventHook = ManageDocks.docksEventHook <> handleEventHook def

trayer :: String
trayer = "trayer --edge top --align center --SetDockType true --SetPartialStrut true \
         \ --expand true --width 12 --transparent true --tint 0x191970 --height 14 \
         \ --monitor primary"

myStartupHook :: X ()
myStartupHook = do -- setWMName "LG3D" -- Helps with certain Java apps, IRRC.
  Once.spawnOnce "sh ./screenlayout/MainSetup.sh"
  Once.spawnOnce trayer
  Once.spawnOnce "sh ~/.fehbg"
  Once.spawnOnce "urxvtd"
  Once.spawnOnce "guake"
  Once.spawnOnce "nm-applet"
  Once.spawnOnce "fcitx"
  Once.spawnOnce "mpd"
  Once.spawnOnce "mate-volume-control-applet"
  Once.spawnOnce "dunst"

main :: IO ()
main = do
  -- moved here due to big signature that was generated by the result
  let myConfig xmobarPipe =
        Full.fullscreenSupport
        $ Urgency.withUrgencyHook UrgencyHook.LibNotify
        $ def
        { workspaces  = Config.myWorkspaces
        , modMask     = Config.modm
        , terminal    = "urxvtc"
        , layoutHook  = Layouts.hook
        , manageHook  = myManageHook
        , logHook     = myLogHook xmobarPipe >> GroupNav.historyHook
        , startupHook = myStartupHook
        , normalBorderColor  = Config.blue
        , focusedBorderColor = Config.pink
        , handleEventHook    = myHandleEventHook
        } `EZ.additionalKeysP` Keys.stringMap
          `EZ.additionalKeys`  Keys.maskMap
  xmobarPipe <- Run.spawnPipe "xmobar ~/.xmonad/bar/xmobarrc"
  xmonad $ Nav2D.withNavigation2DConfig def (myConfig xmobarPipe)
