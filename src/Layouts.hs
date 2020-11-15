{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Layouts where

import qualified XMonad.Layout.BoringWindows  as Boring
import qualified XMonad.Layout.Renamed        as Renamed
import qualified XMonad.Layout.LayoutModifier as LayoutModifier
import qualified XMonad.Layout.NoBorders      as NoBorders
import qualified XMonad.Layout.Circle         as Circle
import qualified XMonad.Layout.Grid           as Grid
import qualified XMonad.Layout.IM             as IM
import qualified XMonad.Hooks.ManageDocks     as ManageDocks
import qualified XMonad.Layout.FixedColumn    as FixedCoulmn
import qualified XMonad.Layout.PerWorkspace   as PerWorkspace
import qualified XMonad.Layout.Reflect        as Reflect
import qualified XMonad.Layout.Gaps           as Gaps
import qualified XMonad.Layout.MultiToggle    as MultiTog
import XMonad
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Accordion
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Hidden
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing                -- this makes smart space around windows
import XMonad.Layout.SubLayouts             -- Layouts inside windows. Excellent.
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

import qualified Configuration as Config


--------------------------------------------------------------------------------
-- Main Layout for configuration
--------------------------------------------------------------------------------

myLayout
  = ManageDocks.avoidStruts
  $ NoBorders.smartBorders
  $ MultiTog.mkToggle (MultiTog.single Reflect.REFLECTY)
  $ MultiTog.mkToggle (MultiTog.single Reflect.REFLECTX)
  $ toggleLayouts (Boring.boringWindows Full) workspaceLayouts

workspaceLayouts
  = PerWorkspace.onWorkspace Config.w2 webLayouts
  $ PerWorkspace.onWorkspace Config.w6 (tabs ||| defaultLayouts)
  $ PerWorkspace.onWorkspace Config.w10 myLayout'
  $ defaultLayouts
  where
    _chatLayout    = myGaps Grid.Grid ||| defaultLayouts
    _codeLayouts   = fixedLayout ||| tiledLayout ||| Mirror tiledLayout

    webLayouts     = Reflect.reflectHoriz flex   ||| defaultLayouts

    defaultLayouts = flex               ||| threeCol |||
                     tilePipeLine       ||| tabs     |||
                     Mirror tilePipeLine

    tilePipeLine = defaultLayoutPipeline tiledLayout
    _showWorkspaceName  = showWName' Config.myShowWNameTheme

--------------------------------------------------------------------------------
-- Layouts
--------------------------------------------------------------------------------

-- A standard tiled layout, with a master pane and a secondary pane off to
-- the side.  The master pane typically holds one window; the secondary
-- pane holds the rest.  Copied from standard xmonad.hs template config.
tiledLayout :: ResizableTall a
tiledLayout = ResizableTall nmaster delta ratio []
  where
    nmaster = 1      -- The default number of windows in the master pane.
    ratio   = 1/2    -- Default proportion of screen occupied by master pane.
    delta   = 3/100  -- Percent of screen to increment by when resizing panes.


-- all our layouts must have lazy

-- Inspired by:
--   http://kitenet.net/~joey/blog/entry/xmonad_layouts_for_netbooks/

flex =
  defaultLayoutPipeline
  $ subLayout [] (Simplest ||| uniformGaps Accordion)
  $ ifWider Config.smallMonResWidth wideLayouts standardLayouts
     where
       wideLayouts = myGaps
                   $ uniformGaps
                   $ tiledLayout
                   -- $ trimSuffixed 1 "Wide BSP" (hiddenWindows emptyBSP)
                 ||| suffixed "Wide 3Col" (ThreeColMid 1 (1/20) (1/2))
       standardLayouts =
         myGaps
         $ uniformGaps
         $ tiledLayout
         -- $ trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP
         ||| suffixed "Std 2/3" (ResizableTall 1 (1/20) (2/3) [])
         ||| suffixed "Std 1/2" (ResizableTall 1 (1/20) (1/2) [])

threeCol = named "Unflexed"
             $ addTopBar
             $ Boring.boringAuto
             $ myGaps
             $ uniformGaps
             $ ThreeColMid 1 (1/10) (1/2)

addTopBar = noFrillsDeco shrinkText Config.topBarTheme

tabs = named "Tabs"
     $ defaultLayoutTile
     $ Simplest

twoDLayout = trimNamed 10 "test"
           $ defaultLayoutPipeline
           $ subLayout [0,1] (Simplest ||| (uniformGaps $ Accordion))
           $ subLayout [0,1] (Simplest ||| Accordion ||| simpleTabbed)
           $ tiledLayout ||| Full

uniformGaps = uniformSpacing Config.gap

defaultLayoutPipeline x = defaultPred Boring.boringAuto x

defaultLayoutTile x = defaultPred Boring.boringWindows x

defaultPred boringFunc x =
  windowNavigation
  $ boringFunc
  $ addTabs shrinkText Config.myTabTheme
  $ x

-- An 80-column fixed layout for Emacs and terminals.  The master
-- pane will resize so that the contained window is 80 columns wide.
fixedLayout :: FixedCoulmn.FixedColumn a
fixedLayout = FixedCoulmn.FixedColumn 1 20 80 10

------------------------------------------------------------
-- Legacy Layouts
------------------------------------------------------------

-- Currently unused
-- A layout for instant messaging.  Devote 1/6th of the screen to
-- the Buddy List, and arrange other windows in a grid.
imLayout :: LayoutModifier.ModifiedLayout
             Gaps.Gaps
             (LayoutModifier.ModifiedLayout IM.AddRoster Grid.Grid)
             a
imLayout = myGaps
         $ IM.withIM (1/6) (IM.Or (IM.Title "Liste de contacts")
                                  (IM.Title "Buddy List"))
           Grid.Grid

-- Another IM layout, for use with Skype.
skypeLayout :: LayoutModifier.ModifiedLayout IM.AddRoster Grid.Grid a
skypeLayout = IM.withIM (1/6) skypeMainWindow Grid.Grid

skypeMainWindow :: IM.Property
skypeMainWindow = (IM.And (IM.Resource "skype")
                          (IM.Not (IM.Or (IM.Title "Transferts de fichiers")
                                  (IM.Role "ConversationsWindow"))))
-- avoidStruts is what allows xmobar and taffybar to stay on the screen

------------------------------------------------------------
-- Experimental Layouts
------------------------------------------------------------

myLayout' =
  windowNavigation
  $ addTabs shrinkText Config.myTabTheme
  $ subLayout [0,1,2] (Tall 1 0.2 0.5 ||| Simplest ||| Circle.Circle)
  $ tiledLayout ||| Full

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

trimNamed w n =
  Renamed.renamed [ Renamed.CutWordsLeft w
                  , Renamed.PrependWords n]

------------------------------------------------------------
-- Gaps
------------------------------------------------------------

sGap :: Int
sGap = quot Config.gap 2

myGaps :: l a -> LayoutModifier.ModifiedLayout Gaps.Gaps l a
myGaps =
  Gaps.gaps [(U, Config.gap), (D, Config.gap), (L, Config.gap), (R, Config.gap)]

mySmallGaps :: l a -> LayoutModifier.ModifiedLayout Gaps.Gaps l a
mySmallGaps =
  Gaps.gaps [(U, sGap), (D, sGap), (L, sGap), (R, sGap)]

myBigGaps :: l a -> LayoutModifier.ModifiedLayout Gaps.Gaps l a
myBigGaps =
  Gaps.gaps [ (U, Config.gap * 2)
            , (D, Config.gap * 2)
            , (L, Config.gap * 2)
            , (R, Config.gap * 2)
            ]

------------------------------------------------------------
-- Naming Windows
------------------------------------------------------------

named n = Renamed.renamed [(Renamed.Replace n)]

suffixed n  = Renamed.renamed [(Renamed.AppendWords n)]

trimSuffixed w n = Renamed.renamed [ Renamed.CutWordsRight w
                                   , Renamed.AppendWords n]
------------------------------------------------------------
-- Spacing
------------------------------------------------------------

uniformSpacing :: Int -> l a -> LayoutModifier.ModifiedLayout Spacing l a
uniformSpacing i = spacingRaw True (Border 0 0 0 0) False (Border i' i' i' i') True
    where i' = fromIntegral i
