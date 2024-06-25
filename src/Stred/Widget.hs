module Stred.Widget where

import Control.Monad.State.Strict (StateT)
import Graphics.Vty qualified as Vty

class Widget a where
  -- | Returns whether event was handled. If the event goes unhandled all the
  -- way to the toplevel, the application will terminate.
  handleEvent :: Vty.Event -> StateT a IO Bool

  render :: a -> Vty.Image
