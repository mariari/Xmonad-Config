{-# LANGUAGE FlexibleContexts #-}
module Keys where

import System.Exit

import qualified XMonad.Actions.GroupNavigation as GroupNav
import qualified XMonad.Layout.BoringWindows    as Boring
import qualified XMonad.Actions.Submap          as Submap
import qualified XMonad.Actions.Warp            as Warp
import qualified XMonad.StackSet                as W
import qualified XMonad.Layout.Reflect          as Reflect
import qualified XMonad.Layout.MultiToggle      as MultiTog
import qualified XMonad.Prompt.Shell            as Prompt.Shell
import qualified XMonad.Prompt                  as Prompt
import qualified XMonad.Prompt.Window           as Window
import qualified XMonad.Actions.WorkspaceNames as WorkSpaceNames
import XMonad --hiding ((|||))
import XMonad.Actions.GridSelect
import XMonad.Hooks.ManageDocks
import XMonad.Layout.ToggleLayouts

import XMonad.Prompt.Man

import XMonad.Layout.ResizableTile
import XMonad.Actions.Navigation2D
import XMonad.Actions.MessageFeedback       -- pseudo conditional key bindings
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.SubLayouts             -- Layouts inside windows. Excellent.
import qualified Data.Map  as Map

import qualified Configuration as Config
import qualified StackOps
import qualified Shell

data Mpv = Mpv

instance Prompt.XPrompt Mpv where
  showXPrompt Mpv     = "MPV on: "
  completionToCommand _ = Shell.escape

--------------------------------------------------------------------------------
-- Main Key bindings
--------------------------------------------------------------------------------

-- windows with warp to window have them not to warp between windows, but
-- to always warp to the center of the current screen

prefixKey :: (KeyMask, KeySym)
prefixKey = (Config.modm, xK_x)

-- | @prefixMap@ is prefixMap for my xmonad keys
prefixMap :: X ()
prefixMap =
  Submap.submap . Map.fromList $
    [ ((0, xK_Return)    , Prompt.Shell.shellPrompt Config.promptConfig)
    , ((0, xK_m)         , Shell.safePrompt Mpv "mpv" Config.promptConfig)
    , ((shiftMask, xK_w) , Window.windowPrompt Config.promptConfig Window.Goto Window.wsWindows)
    , ((0, xK_g)         , groupMap)
    , ((0, xK_u)         , uploadCurrent)
    , ((shiftMask, xK_u) , uploadSection)
    ]

-- | @groupMap@ is the group submap
groupMap :: X ()
groupMap = Submap.submap . Map.fromList $
    [ ((0, xK_r)         , WorkSpaceNames.renameWorkspace Config.promptConfig)
    ]

stringMap :: [(String, X ())]
stringMap =
  [ ("M-g", goToSelected def)   -- Display window selection grid.
  , ("M-f",   sendMessage ToggleStruts <+> sendMessage ToggleLayout)
  , ("M-S-f", sendMessage ToggleLayout)
  , ("M-d",
     -- spawn "dmenu_run -nb \"#101010\" -nf \"#999999\" -sb \"#191919\" -sf \"#ff6699\""
     Prompt.Shell.shellPrompt Config.promptConfig
    )
  , ("M-n", spawn "passmenu  -nb \"#101010\" -nf \"#999999\" -sb \"#191919\" -sf \"#ff6699\"")
  , ("M-S-q", kill)
  , ("M-S-C-;", io exitSuccess) -- quit xmonad
  , ("M-t", withFocused (windows . toggleFloat))
  ] <> myAddWorkspace Config.wsExtra
    <> myAddWorkspace (zip Config.myWorkspaces (fmap (: []) ['1'..'9']))
    <> monitorKeys
--  <> zipM' "M-"   "Navigate screen" arrowKeys dirs screenGo True
    <> zipM' "M-"   "Navigate window" arrowKeys dirs windowGo True
    <> zipM' "C-S-" "Move window"     arrowKeys dirs windowSwap True
    <> imageUploading
    <> extraSwapKeys
    <> audioKeys
    <> resizing
    <> windowModification

maskMap :: [((KeyMask, KeySym), X ())]
maskMap
  = [ (prefixKey, prefixMap)
    , ((Config.modm, xK_equal ),
       WorkSpaceNames.workspaceNamePrompt
          Config.promptConfig
          (\str -> windows (W.view str) >> warpToMidWindow))
    , ((Config.modm, xK_F1), manPrompt Config.promptConfig)
    , ((Config.modm, xK_b), sendMessage ToggleStruts)
    , ((Config.modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
    , ((Config.modm, xK_c), toSubl NextLayout)
    , ((Config.modm, xK_q)
        -- %! Restart xmonad
      , spawn "if type xmonad; then xmonad --recompile && xmonad --restart; \
              \ else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    , ((Config.modm, xK_s     ), asks config >>= Submap.submap . defaultSublMap)
    , ((Config.modm, xK_Return), windows W.swapMaster)
    , ((Config.modm, xK_BackSpace), GroupNav.nextMatch GroupNav.History (return True) >> warpToMidWindow)
    ] <> mergingSubLayouts
      <> focusMaskMap

--------------------------------------------------------------------------------
-- Named Key bindings
--------------------------------------------------------------------------------

focusMaskMap :: [((KeyMask, KeySym), X ())]
focusMaskMap =
  -- %! Swap the focused window with the next window
  [ ((Config.modm, xK_j), Boring.focusDown)
  -- %! Swap the focused window with the previous window
  , ((Config.modm, xK_k), Boring.focusUp)
  ]

-- Merge w/ sublayout
mergingSubLayouts :: [((KeyMask, KeySym), X ())]
mergingSubLayouts =
  [ ((Config.modm .|. controlMask, xK_j), StackOps.mergeFun Boring.focusDown)
  , ((Config.modm .|. controlMask, xK_k), StackOps.mergeFun Boring.focusUp)
  , ((Config.modm .|. controlMask, xK_h), sendMessage       (pullGroup L))
  , ((Config.modm .|. controlMask, xK_l), sendMessage       (pullGroup R))
  ]

extraSwapKeys :: [(String, X ())]
extraSwapKeys =
  [ ("M-;", windows W.swapDown) -- sawp tabs down
  , ("M-'", windows W.swapUp)   -- swap tabs up
  ]

-- monitor keybindings
monitorKeys :: [(String, X ())]
monitorKeys
  =  fmap (\(k,sc) -> ("M-S-" <> [k], appplyOnWorkSpace W.shift sc))      screenAssoc
  <> fmap (\(k,sc) -> ("M-C-" <> [k], appplyOnWorkSpace W.greedyView sc)) screenAssoc
  <> fmap (\(k,sc) -> ("M-"   <> [k], appplyOnWorkSpace W.view sc >> warpToMidWindow))
          screenAssoc

audioKeys :: [(String, X ())]
audioKeys =
  [ ("<XF86AudioPlay>", spawn "~/scripts/mpcPausePlay.sh")
  , ("<XF86AudioStop>", spawn "mpc rand")
  , ("<XF86AudioNext>", spawn "mpc next")
  , ("<XF86AudioPrev>", spawn "mpc prev")
  , ("<XF86TouchPadToggle>"   , spawn "~/shellscript/toggletouchpad.sh")
  , ("<XF86MonBrightnessUp>"  , spawn "xbacklight -steps 1 -time 0 -inc 5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -steps 1 -time 0 -dec 5")

  , ("<XF86AudioMute>"        , spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("<XF86AudioLowerVolume>" , spawn "pactl set-sink-volume @DEFAULT_SINK@ '-5%'")
  , ("<XF86AudioRaiseVolume>" , spawn "pactl set-sink-volume @DEFAULT_SINK@ '+5%'")
  ]

uploadCurrent :: X ()
uploadCurrent =
  spawn "~/scripts/maim/current.sh"

uploadSection :: X ()
uploadSection =
  spawn "~/scripts/maim/section.sh"

imageUploading :: [(String, X ())]
imageUploading =
  [
  --   ("C-S-1", uploadCurrent)
  -- , ("C-S-2", uploadSection)
  -- , ("C-S-3", spawn "puush -a")

  -- , ("C-S-5", spawn "puush -d")
  -- , ("C-S-1", spawn "puush -e")
  ]

resizing :: [(String, X ())]
resizing =
  [ ("M-["    , tryMsgR (ExpandTowards L) (Shrink))
  , ("M-]"    , tryMsgR (ExpandTowards R) (Expand))
  , ("M-S-["  , tryMsgR (ExpandTowards U) (MirrorShrink))
  , ("M-S-]"  , tryMsgR (ExpandTowards D) (MirrorExpand))
  , ("M-C-["  , tryMsgR (ShrinkFrom R) (Shrink))
  , ("M-C-]"  , tryMsgR (ShrinkFrom L) (Expand))
  , ("M-C-S-[", tryMsgR (ShrinkFrom D) (MirrorShrink))
  , ("M-C-S-]", tryMsgR (ShrinkFrom U) (MirrorExpand)) ]

windowModification :: [(String, X ())]
windowModification =
  [ ("M-z M-r", sendMessage Rotate)                             -- rotate
  , ("M-v"    , sendMessage (MultiTog.Toggle Reflect.REFLECTX)) -- reflection on flex
  , ("M-S-v"  , sendMessage (MultiTog.Toggle Reflect.REFLECTY)) -- reflect on y
  ]

------------------------------------------------------------
-- Screen helper functions
------------------------------------------------------------

screenAssoc :: [(Char, ScreenId)]
screenAssoc = zip "wer" Config.screenOrdering

appplyOnWorkSpace :: (WorkspaceId -> WindowSet -> WindowSet) -> ScreenId -> X ()
appplyOnWorkSpace f scr =
  screenWorkspace scr >>= flip whenJust (windows . f)

myAddWorkspace :: [(String, String)] -> [(String, X ())]
myAddWorkspace ws
  =  fmap (\(w, k) -> ("M-"   <> k, windows (W.view w))) ws
  <> fmap (\(w, k) -> ("M-S-" <> k, windows (W.shift w))) ws
  <> fmap (\(w, k) -> ("M-C-" <> k, windows (W.greedyView w))) ws


-- This warps to the middle screen, changing the focus, so this is not ideal
warpToMidScren :: ScreenId -> X ()
warpToMidScren x = Warp.warpToScreen x 0.5 0.5

-- this on the other hand is ideal given we move to the proper window first
warpToMidWindow :: X ()
warpToMidWindow = Warp.warpToWindow 0.5 0.5

dirs :: [Direction2D]
dirs = [D, U, L, R]

arrowKeys :: [String]
arrowKeys = ["<D>","<U>","<L>","<R>"]

dirKeys :: [String]
dirKeys   = ["j","k","h","l"]

zipM :: [a] -> p -> [[a]] -> [t] -> (t -> b) -> [([a], b)]
zipM  m _nm ks as f = zipWith (\k d -> (m ++ k, f d))   ks as

zipM' :: [a] -> p -> [[a]] -> [t1] -> (t1 -> t2 -> b) -> t2 -> [([a], b)]
zipM' m _nm ks as f b = zipWith (\k d -> (m ++ k, f d b)) ks as

tryMsgR :: (Message a, Message b) => a -> b -> X ()
tryMsgR x y =
  sequence_ [tryMessageWithNoRefreshToCurrent x y, refresh]

toggleFloat :: Ord a => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
toggleFloat w s
  | Map.member w (W.floating s) = W.sink w s
  | otherwise                   = W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s
