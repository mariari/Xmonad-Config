module UrgencyHook where

import qualified XMonad.Util.NamedWindows as NamedWindow
import qualified XMonad.Hooks.UrgencyHook as Urgency
import qualified XMonad.StackSet          as W
import qualified XMonad.Util.Run          as Run
import XMonad
---------------------------------------------------------------------------
-- Urgency Hook
---------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotify = LibNotify deriving (Read, Show)

instance Urgency.UrgencyHook LibNotify where
  urgencyHook LibNotify w = do
    name     <- NamedWindow.getName w
    Just idx <- W.findTag w <$> gets windowset
    Run.safeSpawn "notify-send" [show name, "workspace " <> idx]

-- cf https://github.com/pjones/xmonadrc

