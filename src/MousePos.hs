module MousePos where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Warp as Warp
import qualified Control.Monad as Monad

-- queryPoint return names for prosperity
-- (sameRoot, child, currentWindow, rootX, rootY, winx, winy, mask)

-- | @warpToCurrentScreen@ warps the mouse to the current monitor if
-- not already there. This is an alternative to updatePointer which
-- runs on every window change. Is an O(w) operation
warpToCurrentScreen :: X ()
warpToCurrentScreen = do
  XConf {display, theRoot, mouseFocused} <- ask
  Monad.unless mouseFocused $ do
    (_, _, currentWindow, _, _, _, _, _) <- io $ queryPointer display theRoot
    stack <- gets (W.stack . W.workspace . W.current . windowset)
    case stack of
      Just W.Stack {focus, up, down}
        | currentWindow `notElem` focus : up <> down ->
          Warp.warpToWindow 0.5 0.5
      Just W.Stack {} -> pure ()
      Nothing         -> pure ()
