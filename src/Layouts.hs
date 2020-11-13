{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Layouts where


-- import Data.Monoid
import XMonad --hiding ((|||))
import XMonad.Hooks.ManageDocks
import XMonad.Layout.FixedColumn
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Accordion
import XMonad.Layout.Renamed

import XMonad.Layout.ResizableTile


--import XMonad.Layout hiding ( (|||) )       -- ||| from X.L.LayoutCombinators
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
--import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing                -- this makes smart space around windows
import XMonad.Layout.SubLayouts             -- Layouts inside windows. Excellent.
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

import Configuration


-- A standard tiled layout, with a master pane and a secondary pane off to
-- the side.  The master pane typically holds one window; the secondary
-- pane holds the rest.  Copied from standard xmonad.hs template config.
tiledLayout :: Tall a
tiledLayout = Tall nmaster delta ratio
  where
    nmaster = 1      -- The default number of windows in the master pane.
    ratio   = 1/2    -- Default proportion of screen occupied by master pane.
    delta   = 3/100  -- Percent of screen to increment by when resizing panes.
-- foo =
-- Inspired by:
--   http://kitenet.net/~joey/blog/entry/xmonad_layouts_for_netbooks/
workspaceLayouts =
  onWorkspace w2 webLayouts
  $ onWorkspace w3 chatLayout
  -- $ onWorkspace w1 skypeLayouts
  $ onWorkspace w5 twoDLayout
  $ defaultLayouts
  where
    chatLayout     = myGaps Grid ||| defaultLayouts
    codeLayouts    = fixedLayout ||| tiledLayout ||| Mirror tiledLayout
    webLayouts     = reflectHoriz flex ||| reflectHoriz tiledLayout  ||| defaultLayouts
    skypeLayouts   = skypeLayout
    defaultLayouts = flex               ||| threeCol |||
                     tiledLayout        ||| tabs     |||
                     Mirror tiledLayout

    smallMonResWidth    = 1920
    showWorkspaceName   = showWName' myShowWNameTheme

    named n             = renamed [(XMonad.Layout.Renamed.Replace n)]
    trimNamed w n       = renamed [(XMonad.Layout.Renamed.CutWordsLeft w),
                                   (XMonad.Layout.Renamed.PrependWords n)]
    suffixed n          = renamed [(XMonad.Layout.Renamed.AppendWords n)]
    trimSuffixed w n    = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
                                   (XMonad.Layout.Renamed.AppendWords n)]

    addTopBar           = noFrillsDeco shrinkText topBarTheme

    mySpacing           = spacing gap
    sGap                = quot gap 2
    myGaps              = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
    mySmallGaps         = gaps [(U, sGap),(D, sGap),(L, sGap),(R, sGap)]
    myBigGaps           = gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]

    threeCol = named "Unflexed"
             $ avoidStruts
             $ addTopBar
             $ myGaps
             $ mySpacing
             $ ThreeColMid 1 (1/10) (1/2)

    tabs = named "Tabs"
         $ avoidStruts
         $ addTopBar
         $ addTabs shrinkText myTabTheme
         $ Simplest


    twoDLayout = trimNamed 10 "test"
               $ windowNavigation
               $ subLayout [0,1] (Simplest ||| (mySpacing $ Accordion))
               -- $ subLayout [0,1] (Simplest ||| Accordion ||| simpleTabbed)
               $ (Tall 1 (3/100) (1/2))  ||| Full
    -- from old config
    flex = trimNamed 5 "Flex"
              -- don't forget: even though we are using X.A.Navigation2D
              -- we need windowNavigation for merging to sublayouts
              $ windowNavigation
              $ addTabs shrinkText myTabTheme
              $ subLayout [] (Simplest ||| (mySpacing $ Accordion))
              $ subLayout [] (Simplest ||| Accordion)
              $ ifWider smallMonResWidth wideLayouts standardLayouts
              where
                  wideLayouts = myGaps $ mySpacing
                      $ (trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP)
                    ||| (suffixed "Wide 3Col" $ ThreeColMid 1 (1/20) (1/2))
                  --  ||| fullTabs
                  standardLayouts = myGaps $ mySpacing
                      $ (trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP)
                    ||| (suffixed "Std 2/3" $ ResizableTall 1 (1/20) (2/3) [])
                    ||| (suffixed "Std 1/2" $ ResizableTall 1 (1/20) (1/2) [])
--                     floatLayout ||| simpleTabbed

    -- An 80-column fixed layout for Emacs and terminals.  The master
    -- pane will resize so that the contained window is 80 columns wide.
    fixedLayout = FixedColumn 1 20 80 10

    -- A layout for instant messaging.  Devote 1/6th of the screen to
    -- the Buddy List, and arrange other windows in a grid.
    imLayout = myGaps $ withIM (1/6) (Or (Title "Liste de contacts")
                                         (Title "Buddy List")) Grid

    -- Another IM layout, for use with Skype.
    skypeLayout = withIM (1/6) skypeMainWindow Grid
    skypeMainWindow = (And (Resource "skype")
                           (Not (Or (Title "Transferts de fichiers")
                                    (Role "ConversationsWindow"))))
--    floatLayout = windowArrange simpleFloat

-- avoidStruts is what allows xmobar and taffybar to stay on the screen

myLayout = avoidStruts $ smartBorders $ toggleLayouts Full workspaceLayouts
