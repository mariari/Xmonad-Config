{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Layouts where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.FixedColumn as FixedCoulmn
import XMonad.Layout.Grid as Grid
import XMonad.Layout.IM as IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Accordion
import qualified XMonad.Layout.Renamed as Renamed
import qualified XMonad.Layout.LayoutModifier as LayoutModifier
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Gaps as Gaps
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


-- A standard tiled layout, with a master pane and a secondary pane off to
-- the side.  The master pane typically holds one window; the secondary
-- pane holds the rest.  Copied from standard xmonad.hs template config.
tiledLayout :: Tall a
tiledLayout = Tall nmaster delta ratio
  where
    nmaster = 1      -- The default number of windows in the master pane.
    ratio   = 1/2    -- Default proportion of screen occupied by master pane.
    delta   = 3/100  -- Percent of screen to increment by when resizing panes.


-- Inspired by:
--   http://kitenet.net/~joey/blog/entry/xmonad_layouts_for_netbooks/
workspaceLayouts = onWorkspace Config.w2 webLayouts
                 $ onWorkspace Config.w5 twoDLayout
                 $ onWorkspace Config.w6 (tabs ||| defaultLayouts)
                 -- $ onWorkspace Config.w3 chatLayout
                 $ defaultLayouts
  where
    _chatLayout    = myGaps Grid ||| defaultLayouts
    _codeLayouts   = fixedLayout ||| tiledLayout ||| Mirror tiledLayout
    webLayouts     = reflectHoriz flex ||| reflectHoriz tiledLayout  ||| defaultLayouts
    defaultLayouts = flex               ||| threeCol |||
                     tiledLayout        ||| tabs     |||
                     Mirror tiledLayout

    smallMonResWidth    = 1920
    _showWorkspaceName  = showWName' Config.myShowWNameTheme

    named n             = Renamed.renamed [(Renamed.Replace n)]
    trimNamed w n       = Renamed.renamed [(Renamed.CutWordsLeft w),
                                           (Renamed.PrependWords n)]
    suffixed n          = Renamed.renamed [(Renamed.AppendWords n)]
    trimSuffixed w n    = Renamed.renamed [(Renamed.CutWordsRight w),
                                           (Renamed.AppendWords n)]

    addTopBar           = noFrillsDeco shrinkText Config.topBarTheme

    gap                 = Config.gap
    mySpacing           = uniformSpacing gap

    threeCol = named "Unflexed"
             $ avoidStruts
             $ addTopBar
             $ myGaps
             $ mySpacing
             $ ThreeColMid 1 (1/10) (1/2)

    tabs = named "Tabs"
         $ avoidStruts
         $ addTabs shrinkText Config.myTabTheme
         $ Simplest


    twoDLayout = trimNamed 10 "test"
               $ windowNavigation
               $ subLayout [0,1] (Simplest ||| (mySpacing $ Accordion))
               $ subLayout [0,1] (Simplest ||| Accordion ||| simpleTabbed)
               $ tiledLayout ||| Full
    -- from old config
    flex = trimNamed 5 "Flex"
              -- don't forget: even though we are using X.A.Navigation2D
              -- we need windowNavigation for merging to sublayouts
              $ windowNavigation
              $ addTabs shrinkText Config.myTabTheme
              $ subLayout [] (Simplest ||| mySpacing Accordion)
              $ subLayout [] (Simplest ||| Accordion)
              $ ifWider smallMonResWidth wideLayouts standardLayouts
              where
                  wideLayouts = myGaps
                              $ mySpacing
                              $ trimSuffixed 1 "Wide BSP" (hiddenWindows emptyBSP)
                            ||| suffixed "Wide 3Col" (ThreeColMid 1 (1/20) (1/2))
                  standardLayouts =
                    myGaps
                      $ mySpacing
                      $ trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP
                    ||| suffixed "Std 2/3" (ResizableTall 1 (1/20) (2/3) [])
                    ||| suffixed "Std 1/2" (ResizableTall 1 (1/20) (1/2) [])
--                     floatLayout ||| simpleTabbed
--    floatLayout = windowArrange simpleFloat


-- An 80-column fixed layout for Emacs and terminals.  The master
-- pane will resize so that the contained window is 80 columns wide.
fixedLayout :: FixedCoulmn.FixedColumn a
fixedLayout = FixedColumn 1 20 80 10

-- A layout for instant messaging.  Devote 1/6th of the screen to
 -- the Buddy List, and arrange other windows in a grid.
imLayout :: LayoutModifier.ModifiedLayout
             Gaps.Gaps
             (LayoutModifier.ModifiedLayout IM.AddRoster Grid.Grid)
             a
imLayout = myGaps
         $ withIM (1/6) (Or (Title "Liste de contacts")
                            (Title "Buddy List"))
           Grid

-- Another IM layout, for use with Skype.
skypeLayout :: LayoutModifier.ModifiedLayout IM.AddRoster Grid.Grid a
skypeLayout = withIM (1/6) skypeMainWindow Grid

skypeMainWindow :: Property
skypeMainWindow = (And (Resource "skype")
                       (Not (Or (Title "Transferts de fichiers")
                                (Role "ConversationsWindow"))))
-- avoidStruts is what allows xmobar and taffybar to stay on the screen

myLayout = avoidStruts $ smartBorders $ toggleLayouts Full workspaceLayouts

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

sGap :: Int
sGap = quot Config.gap 2

myGaps :: l a -> LayoutModifier.ModifiedLayout Gaps.Gaps l a
myGaps =
  gaps [(U, Config.gap), (D, Config.gap), (L, Config.gap), (R, Config.gap)]

mySmallGaps :: l a -> LayoutModifier.ModifiedLayout Gaps.Gaps l a
mySmallGaps =
  gaps [(U, sGap),(D, sGap),(L, sGap),(R, sGap)]

myBigGaps :: l a -> LayoutModifier.ModifiedLayout Gaps.Gaps l a
myBigGaps =
  gaps [(U, Config.gap*2),(D, Config.gap*2),(L, Config.gap*2),(R, Config.gap*2)]


--    floatLayout = windowArrange simpleFloat

uniformSpacing :: Int -> l a -> LayoutModifier.ModifiedLayout Spacing l a
uniformSpacing i = spacingRaw True (Border 0 0 0 0) False (Border i' i' i' i') True
    where i' = fromIntegral i

