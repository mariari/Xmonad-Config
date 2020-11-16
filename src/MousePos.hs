module MousePos where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Warp as Warp

-- an alternative to updatePointer

-- O(n) where n is the number of windows on the current screen
warpToCurrentScreen :: X ()
warpToCurrentScreen = do
  dpy  <- asks display
  root <- asks theRoot
  (_sameRoot, _child, currentWindow, _rootX, _rootY,_winx, _winy, _mask)
      <- io $ queryPointer dpy root
  ws <- gets windowset
  mouseIsMoving <- asks mouseFocused
  case W.stack $ W.workspace $ W.current ws of
    Just W.Stack {focus, up, down}
      | not mouseIsMoving && currentWindow `notElem` focus : up <> down ->
        Warp.warpToWindow 0.5 0.5
    _ -> pure ()
