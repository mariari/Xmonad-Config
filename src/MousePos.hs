module MousePos where

import XMonad
import qualified XMonad.StackSet           as W
import qualified XMonad.Actions.Warp       as Warp
import qualified Control.Monad             as Monad
import qualified Data.Maybe                as Maybe
import qualified XMonad.Hooks.EwmhDesktops as Ewmh
import Control.Monad
import qualified Data.Monoid               as Monoid


-- queryPoint return names for prosperity
-- (sameRoot, child, currentWindow, rootX, rootY, winx, winy, mask)

-- ISSUE ∷ activates when floating a window in firefox or other such qt software
-- However if you just activate it on the proper handler it'll work fine

-- | @warpToCurrentScreen@ warps the mouse to the current monitor if
-- not already there. This is an alternative to updatePointer which
-- runs on every window change. Is an O(w) operation
warpToCurrentScreen :: X ()
warpToCurrentScreen = do
  XConf {display, theRoot, mouseFocused} <- ask
  drag <- gets dragging
  Monad.unless (mouseFocused || Maybe.isJust drag) $ do
    (_, _, currentWindow, _, _, _, _, _mod) <- io $ queryPointer display theRoot
    stack <- gets (W.stack . W.workspace . W.current . windowset)
    case stack of
      Just W.Stack {focus, up, down}
        | currentWindow `notElem` focus : up <> down ->
          Warp.warpToWindow 0.5 0.5
      Just W.Stack {} -> pure ()
      Nothing         -> pure ()

ewmhEventHook :: Event -> X Monoid.All
ewmhEventHook event
  = Ewmh.ewmhDesktopsEventHook event
  <* case event of
      ClientMessageEvent {ev_message_type = mt} -> do
        aw <- getAtom "_NET_ACTIVE_WINDOW"
        when (mt == aw) warpToCurrentScreen
      _ -> pure ()
