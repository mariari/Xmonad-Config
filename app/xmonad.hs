{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}

module Main where

import Data.List
import Data.Monoid
import System.Exit

import System.IO                            -- for xmonbar
import Control.Applicative ((<|>))

import XMonad --hiding ((|||))
import qualified XMonad.Core as Core
import qualified XMonad.Actions.ShowText as ShowText
import qualified XMonad.Actions.Warp as Warp
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.ToggleLayouts
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Layout.Fullscreen

import XMonad.Prompt.Man

import XMonad.Layout.Decoration
import XMonad.Layout.ResizableTile
import XMonad.Actions.Navigation2D
--import XMonad.Util.Replace

import XMonad.Actions.CopyWindow            -- like cylons, except x windows
import XMonad.Actions.MessageFeedback       -- pseudo conditional key bindings
--import XMonad.Actions.Volume

import XMonad.Hooks.UrgencyHook

--import XMonad.Layout hiding ( (|||) )       -- ||| from X.L.LayoutCombinators
import XMonad.Layout.BinarySpacePartition
--import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts             -- Layouts inside windows. Excellent.

import XMonad.Util.NamedWindows
import Layouts
import Configuration

data XCond = WS | LD

-- | Choose an action based on the current workspace id (WS) or
-- layout description (LD).
chooseAction :: XCond -> (String -> X ()) -> X ()
chooseAction WS f = withWindowSet (f . W.currentTag)
chooseAction LD f = withWindowSet (f . description . W.layout . W.workspace . W.current)


-- | If current workspace or layout string is listed, run the associated
-- action (only the first match counts!) If it isn't listed, then run the default
-- action (marked with empty string, \"\"), or do nothing if default isn't supplied.
bindOn :: XCond -> [(String, X ())] -> X ()
bindOn xc bindings = chooseAction xc $ chooser
  where
    chooser xc =
      maybe (return ())
            snd
            (find ((xc ==) . fst) bindings <|> find (("" ==) . fst) bindings)

toggleCopyToAll = wsContainingCopies >>= f
  where
    f []    = windows copyToAll
    f (_:_) = killAllOtherCopies


data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)

instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (\_ -> x)

barFull :: ModifiedLayout AvoidStruts Simplest a
barFull = avoidStruts Simplest

-- Rules which are applied to each new window.  The (optional) part before
-- '-->' is a matching rule.  The rest is an action to perform.
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
  [ resource =? "Do"       --> doIgnore     -- Leave Gnome Do alone.
  , resource =? "Pidgin"   --> doShift w3   -- Force to IM workspace.
  , resource =? "skype"    --> doShift w7   -- Force to Skype workspace.
  , resource =? "gimp-2.6" --> doShift w8   -- Special-case the GIMP.
  , resource =? "tilda"    --> doFloat
  , resource =? "guake"    --> doFloat
  , manageDocks                             -- For xmobar
  ]

-- "M-" means the xmonad modifier key, and not "Meta".
myKeys :: [(String, X ())]
myKeys =
  [ ("M-g", goToSelected def)   -- Display window selection grid.
  , ("M-f",   sendMessage ToggleStruts <+> sendMessage ToggleLayout)
  , ("M-S-f", sendMessage ToggleLayout)
  , ("M-d", spawn "dmenu_run -nb \"#101010\" -nf \"#999999\" -sb \"#191919\" -sf \"#ff6699\"")
  , ("M-n", spawn "passmenu -nb \"#101010\" -nf \"#999999\" -sb \"#191919\" -sf \"#ff6699\"")
  , ("M-S-q", kill)
  , ("M-S-z", io (exitWith ExitSuccess))
  , ("C-S-2", spawn "puush -c")
  , ("C-S-3", spawn "puush -a")
  , ("C-S-4", spawn "puush -b")
  , ("C-S-5", spawn "puush -d")
  , ("C-S-1", spawn "puush -e")
  , ("<XF86AudioPlay>", spawn "~/scripts/mpcPausePlay.sh")
  , ("<XF86AudioStop>", spawn "mpc rand")
  , ("<XF86AudioNext>", spawn "mpc next")
  , ("<XF86AudioPrev>", spawn "mpc prev")
  , ("<XF86TouchPadToggle>"   , spawn "~/shellscript/toggletouchpad.sh")
  , ("<XF86MonBrightnessUp>"  , spawn "xbacklight -steps 1 -time 0 -inc 5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -steps 1 -time 0 -dec 5")

  , ("<XF86AudioMute>"        , spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("<XF86AudioLowerVolume>" , spawn "pactl set-sink-volume @DEFAULT_SINK@ '-5%'")
  , ("<XF86AudioRaiseVolume>" , spawn "pactl set-sink-volume @DEFAULT_SINK@ '+5%'")
--  , ("M-S-h", sendMessage $ IncLayoutN (-1))
--  , ("M-S-l", sendMessage $ IncLayoutN 1)
  ] <> myAddWorkspace wsExtra
    <> myAddWorkspace (zip myWorkspaces (fmap (: []) ['1'..'9']))
    <> [ ( "S-" <> "M-" <> [key]
         , screenWorkspace scr >>= flip whenJust (windows . W.shift)
         )
           | (key, scr)  <- zip "wer" [2,0,1]] -- was [0..] *** change to match your screen order ***
    <> [ ( mask <> "M-" <> [key]
         , screenWorkspace scr >>= flip whenJust (windows . action) >> warpToMidScren scr
         )
           | (key, scr)  <- zip "wer" [2,0,1] -- was [0..] *** change to match your screen order ***
           , (action, mask) <- [ (W.view, ""), (W.greedyView, "C-")]]
    <> zipM  "M-C-" "Merge w/sublayout" dirKeys dirs (sendMessage . pullGroup)
    -- <> zipM  "M-C-" "unmege" dirKeys dirs (sendMessage . )
--  <> zipM' "M-"               "Navigate screen"                           arrowKeys dirs screenGo True
    <> zipM' "M-"   "Navigate window"              arrowKeys dirs windowGo True
    <> zipM' "C-S-" "Move window"                  arrowKeys dirs windowSwap True
    <> [ ("M-;", windows W.swapDown) -- sawp tabs down
       , ("M-'", windows W.swapUp)   -- swap tabs up
       ]
    <> [ ("M-["    , tryMsgR (ExpandTowards L) (Shrink))
       , ("M-]"    , tryMsgR (ExpandTowards R) (Expand))
       , ("M-S-["  , tryMsgR (ExpandTowards U) (MirrorShrink))
       , ("M-S-]"  , tryMsgR (ExpandTowards D) (MirrorExpand))
       , ("M-C-["  , tryMsgR (ShrinkFrom R) (Shrink))
       , ("M-C-]"  , tryMsgR (ShrinkFrom L) (Expand))
       , ("M-C-S-[", tryMsgR (ShrinkFrom D) (MirrorShrink))
       , ("M-C-S-]", tryMsgR (ShrinkFrom U) (MirrorExpand))
       ]

warpToMidScren :: ScreenId -> X ()
warpToMidScren x = Warp.warpToScreen x 0.5 0.5

dirs :: [Direction2D]
dirs = [D, U, L, R]

arrowKeys :: [String]
arrowKeys = ["<D>","<U>","<L>","<R>"]

dirKeys :: [String]
dirKeys   = ["j","k","h","l"]

zipM :: [a] -> p -> [[a]] -> [t] -> (t -> b) -> [([a], b)]
zipM  m _nm ks as f   = zipWith (\k d -> (m ++ k, f d))   ks as

zipM' :: [a] -> p -> [[a]] -> [t1] -> (t1 -> t2 -> b) -> t2 -> [([a], b)]
zipM' m _nm ks as f b = zipWith (\k d -> (m ++ k, f d b)) ks as

tryMsgR :: (Message a, Message b) => a -> b -> X ()
tryMsgR x y = sequence_ [(tryMessageWithNoRefreshToCurrent x y), refresh]

myKeys' :: [((KeyMask, KeySym), X ())]
myKeys' = [ ((modm, xK_F1), manPrompt def)
          , ((modm, xK_b), sendMessage ToggleStruts)
          , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
          , ((modm, xK_c), toSubl NextLayout)
          , ((modm, xK_q)
            -- %! Restart xmonad
            , spawn "if type xmonad; then xmonad --recompile && xmonad --restart; \
                    \ else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
          , ((modm, xK_a), foo)
          ]

myAddWorkspace :: [(String, String)] -> [(String, X ())]
myAddWorkspace ws = fmap (\(w, k) -> ("M-"   <> k, windows (W.view w))) ws
                 <> fmap (\(w, k) -> ("M-S-" <> k, windows (W.shift w))) ws
                 <> fmap (\(w, k) -> ("M-C-" <> k, windows (W.greedyView w))) ws

foo :: X ()
foo = do
  state <- get
  let windowSet = Core.windowset state
  ShowText.flashText def 10000 (show (W.allWindows windowSet))

---------------------------------------------------------------------------
-- Urgency Hook
---------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- W.findTag w <$> gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]
-- cf https://github.com/pjones/xmonadrc

----------------------------------------------------------------
-- HOOKS
----------------------------------------------------------------

-- Print the current xmonad status to a pipe for display by xmobar.
-- Instead of using the usual xmobarPP configuration, we use defaultPP and
-- override the colors to match the Ubuntu 10.10 "Clearlooks" theme.
-- Note that we assume "position = Bottom" appears in your xmobar config,
-- and that the Gnome bottom panel has been removed.
--
myLogHook :: Handle -> X ()
myLogHook xmobarPipe = dynamicLogWithPP xmobarPrinter
  where
    xmobarPrinter = def
      { ppOutput  = hPutStrLn xmobarPipe
      , ppCurrent = xmobarColor "black" themeHighlight . wrap "[" "]"
      , ppTitle   = xmobarColor "pink" "" . shorten 50
      , ppVisible = wrap "(" ")"
      , ppUrgent  = xmobarColor "pink" themeHighlight }


myHandleEventHook :: Event -> X All
myHandleEventHook = docksEventHook <+> handleEventHook def

trayer :: String
trayer = "trayer --edge top --align center --SetDockType true --SetPartialStrut true "
      <> "--expand true --width 12 --transparent true --tint 0x191970 --height 14 --monitor primary"


myStartupHook :: X ()
myStartupHook = do -- setWMName "LG3D" -- Helps with certain Java apps, IRRC.
  spawnOnce "sh ./screenlayout/MainSetup.sh"
  spawnOnce trayer
  spawnOnce "sh ~/.fehbg"
  spawnOnce "urxvtd"
  spawnOnce "guake"
  spawnOnce "nm-applet"
  spawnOnce "fcitx"
  spawnOnce "mpd"
  spawnOnce "mate-volume-control-applet"
  spawnOnce "dunst"

-- main = xmonad $ withNavigation2DConfig def
--               $ additionalNav2DKeys (xK_Up, xK_Left, xK_Down, xK_Right)
--                                     [(mod4Mask,               windowGo  ),
--                                      (mod4Mask .|. shiftMask, windowSwap)]
--                                     False
--               $ additionalNav2DKeys (xK_u, xK_l, xK_d, xK_r)
--                                     [(mod4Mask,               screenGo  ),
--                                      (mod4Mask .|. shiftMask, screenSwap)]
--                                     False
--               $ def

-- main = do
--   xmobarPipe <- spawnPipe "xmobar /home/loli/.xmonad/xmobarrc"
--   xmonad (myConfig xmobarPipe)

main :: IO ()
main = do
  -- moved here due to big signature that was generated by the result
  let myConfig xmobarPipe =
        fullscreenSupport
        $ withUrgencyHook LibNotifyUrgencyHook
        $ def
        { workspaces  = myWorkspaces
        , modMask     = modm
        , terminal    = "urxvtc"
        , layoutHook  = myLayout
        , manageHook  = myManageHook
        , logHook     = myLogHook xmobarPipe
        , startupHook = myStartupHook
        , normalBorderColor  = blue
        , focusedBorderColor = pink
        , handleEventHook    = myHandleEventHook
        } `additionalKeysP` myKeys
          `additionalKeys`  myKeys'
  xmobarPipe <- spawnPipe "xmobar ~/.xmonad/bar/xmobarrc"
  xmonad $ withNavigation2DConfig def (myConfig xmobarPipe)

