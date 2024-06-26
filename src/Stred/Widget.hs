module Stred.Widget where

import Graphics.Vty qualified as Vty

class Widget a where
  -- | Nothing means the event was not handled. If the toplevel app does not
  -- handle an event, the system terminates.
  handleEvent :: Vty.Event -> a -> IO (Maybe a)

  render :: Bool -> a -> Vty.Image
