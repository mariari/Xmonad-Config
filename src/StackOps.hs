{-# LANGUAGE FlexibleContexts #-}
module StackOps where

import XMonad
import qualified XMonad.StackSet          as W
import qualified XMonad.Layout.SubLayouts as SubLayout
import XMonad.Actions.Warp


peekUp :: W.Stack a -> Maybe a
peekUp W.Stack {up, down} =
  case up of
    u : _ -> Just u
    [] ->
      case reverse down of
        d : _ -> Just d
        []    -> Nothing

peekDown :: W.Stack a -> Maybe a
peekDown = peekUp . reverseStack

-- the True way to merge layouts. works with any focus function
mergeFun :: X a -> X ()
mergeFun movementFunction =
  onWindow $ \currFrame -> do
    let curr = W.focus currFrame
    state <- get
    -- temporary run the movement function
    _ <- movementFunction
    onWindowFail (put state) $ \g -> do
      let windowWeWant = W.focus g
      -- undo any changes we've made
      put state
      sendMessage (SubLayout.Merge curr windowWeWant)


reverseStack :: W.Stack a -> W.Stack a
reverseStack (W.Stack t ls rs) = W.Stack t rs ls

-- much more limited as we have to make our own peek function
-- meaning we can't do anything interesting if the mode has complicated logic
mergeGroup :: Typeable a => (W.Stack a -> Maybe a) -> W.Stack a -> X ()
mergeGroup peekF w@W.Stack {focus} =
  case peekF w of
    Just lo -> do
      sendMessage (SubLayout.Merge focus lo)
    Nothing -> pure ()

onWindowFail ::
  MonadState XState m => m b -> (W.Stack Window -> m b) -> m b
onWindowFail g f = do
  stack <- gets (W.stack . W.workspace . W.current . windowset)
  maybe g f stack
  -- case stack of
  --   Just x  -> f x
  --   Nothing -> g

-- Uses low level xmonad primitives
-- should figure out if there is a std lib function that does it
onWindow :: (W.Stack Window -> X ()) -> X ()
onWindow = onWindowFail (pure ())


warpToCurrentScreen :: X ()
warpToCurrentScreen = do
  dpy <- asks display
  root <- asks theRoot
  (_sameRoot,_,_currentWidnow, rootX, rootY,_,_,_) <- io $ queryPointer dpy root
  ws <- gets windowset
  warpToScreen (W.screen $ W.current ws) (fromIntegral rootX) (fromIntegral rootY)
  windows (const ws)
