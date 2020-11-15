{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Layouts where

import qualified XMonad.Layout.BoringWindows        as Boring
import qualified XMonad.Layout.Renamed              as Renamed
import qualified XMonad.Layout.LayoutModifier       as LayoutModifier
import qualified XMonad.Layout.NoBorders            as NoBorders
import qualified XMonad.Layout.Circle               as Circle
import qualified XMonad.Layout.Grid                 as Grid
import qualified XMonad.Layout.IM                   as IM
import qualified XMonad.Hooks.ManageDocks           as ManageDocks
import qualified XMonad.Layout.FixedColumn          as FixedCoulmn
import qualified XMonad.Layout.PerWorkspace         as PerWorkspace
import qualified XMonad.Layout.Reflect              as Reflect
import qualified XMonad.Layout.Gaps                 as Gaps
import qualified XMonad.Layout.MultiToggle          as MultiTog
import qualified XMonad.Layout.Tabbed               as Tabbed
import qualified XMonad.Layout.ToggleLayouts        as TogLayout
import qualified XMonad.Layout.Accordion            as Accordion
import qualified XMonad.Layout.ResizableTile        as Resize
import qualified XMonad.Layout.NoFrillsDecoration   as NoFrills
import qualified XMonad.Layout.PerScreen            as PerScreen -- Check screen width & adjust layouts
import qualified XMonad.Layout.Spacing              as Spacing   -- this makes smart space around windows
import qualified XMonad.Layout.SubLayouts           as SubLayout -- Layouts inside windows. Excellent.
import qualified XMonad.Layout.ShowWName            as WName
import qualified XMonad.Layout.ThreeColumns         as ThreeColumn
import qualified XMonad.Layout.Hidden               as Hidden
import qualified XMonad.Layout.BinarySpacePartition as BSP
import qualified XMonad.Layout.Simplest             as Simplest
import qualified XMonad.Layout.WindowNavigation     as WindowNavigation

import qualified Configuration as Config
import XMonad

--------------------------------------------------------------------------------
-- Main Layout for configuration
--------------------------------------------------------------------------------

hook
  = ManageDocks.avoidStruts
  $ NoBorders.smartBorders
  $ MultiTog.mkToggle (MultiTog.single Reflect.REFLECTY)
  $ MultiTog.mkToggle (MultiTog.single Reflect.REFLECTX)
  $ TogLayout.toggleLayouts (Boring.boringWindows Full) workspaceLayouts

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
    _showWorkspaceName = WName.showWName' Config.myShowWNameTheme

--------------------------------------------------------------------------------
-- Layouts
--------------------------------------------------------------------------------

-- A standard tiled layout, with a master pane and a secondary pane off to
-- the side.  The master pane typically holds one window; the secondary
-- pane holds the rest.  Copied from standard xmonad.hs template config.
tiledLayout :: Resize.ResizableTall a
tiledLayout = Resize.ResizableTall nmaster delta ratio []
  where
    nmaster = 1      -- The default number of windows in the master pane.
    ratio   = 1/2    -- Default proportion of screen occupied by master pane.
    delta   = 3/100  -- Percent of screen to increment by when resizing panes.


-- all our layouts must have lazy

-- Inspired by:
--   http://kitenet.net/~joey/blog/entry/xmonad_layouts_for_netbooks/

flex =
  defaultLayoutPipeline
  $ SubLayout.subLayout [] (Simplest.Simplest ||| uniformGaps Accordion.Accordion)
  $ PerScreen.ifWider Config.smallMonResWidth wideLayouts standardLayouts
     where
       wideLayouts = myGaps
                   $ uniformGaps
                   $ tiledLayout
                 ||| suffixed "Wide 3Col" (ThreeColumn.ThreeColMid 1 (1/20) (1/2))
       standardLayouts =
         myGaps
         $ uniformGaps
         $ tiledLayout
         ||| suffixed "Std 2/3" (Resize.ResizableTall 1 (1/20) (2/3) [])
         ||| suffixed "Std 1/2" (Resize.ResizableTall 1 (1/20) (1/2) [])

threeCol = named "Unflexed"
             $ addTopBar
             $ Boring.boringAuto
             $ myGaps
             $ uniformGaps
             $ ThreeColumn.ThreeColMid 1 (1/10) (1/2)

addTopBar = NoFrills.noFrillsDeco NoFrills.shrinkText Config.topBarTheme

tabs = named "Tabs"
     $ defaultLayoutTile
     $ Simplest.Simplest

twoDLayout
  = trimNamed 10 "test"
  $ defaultLayoutPipeline
  $ SubLayout.subLayout
      [0,1]
      (Simplest.Simplest ||| (uniformGaps $ Accordion.Accordion))
  $ SubLayout.subLayout
      [0,1]
      (Simplest.Simplest ||| Accordion.Accordion ||| Tabbed.simpleTabbed)
  $ tiledLayout ||| Full

uniformGaps = uniformSpacing Config.gap

defaultLayoutPipeline x = defaultPred Boring.boringAuto x

defaultLayoutTile x = defaultPred Boring.boringWindows x

defaultPred boringFunc x =
  WindowNavigation.windowNavigation
  $ boringFunc
  $ Tabbed.addTabs NoFrills.shrinkText Config.myTabTheme
  $ x

-- An 80-column fixed layout for Emacs and terminals.  The master
-- pane will resize so that the contained window is 80 columns wide.
fixedLayout :: FixedCoulmn.FixedColumn a
fixedLayout = FixedCoulmn.FixedColumn 1 20 80 10

------------------------------------------------------------
-- Legacy Layouts
------------------------------------------------------------

hiddenBSP = trimSuffixed 1 "Wide BSP" $ Hidden.hiddenWindows BSP.emptyBSP

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
  WindowNavigation.windowNavigation
  $ Tabbed.addTabs NoFrills.shrinkText Config.myTabTheme
  $ SubLayout.subLayout
      [0,1,2]
      (Tall 1 0.2 0.5 ||| Simplest.Simplest ||| Circle.Circle)
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
  Gaps.gaps [ (WindowNavigation.U, Config.gap)
            , (WindowNavigation.D, Config.gap)
            , (WindowNavigation.L, Config.gap)
            , (WindowNavigation.R, Config.gap)]

mySmallGaps :: l a -> LayoutModifier.ModifiedLayout Gaps.Gaps l a
mySmallGaps =
  Gaps.gaps [ (WindowNavigation.U, sGap)
            , (WindowNavigation.D, sGap)
            , (WindowNavigation.L, sGap)
            , (WindowNavigation.R, sGap)]

myBigGaps :: l a -> LayoutModifier.ModifiedLayout Gaps.Gaps l a
myBigGaps =
  Gaps.gaps [ (WindowNavigation.U, Config.gap * 2)
            , (WindowNavigation.D, Config.gap * 2)
            , (WindowNavigation.L, Config.gap * 2)
            , (WindowNavigation.R, Config.gap * 2)
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

uniformSpacing :: Int -> l a -> LayoutModifier.ModifiedLayout Spacing.Spacing l a
uniformSpacing i =
  Spacing.spacingRaw True (same 0) False (same (fromIntegral i)) True
  where
    same x = Spacing.Border x x x x
