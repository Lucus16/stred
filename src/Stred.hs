module Stred where

import Control.Exception.Safe (Exception, finally, throw, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, get)
import Graphics.Vty (Vty)
import Graphics.Vty qualified as Vty
import Graphics.Vty.Image

class Widget a where
  -- | Returns whether event was handled. If the event goes unhandled all the
  -- way to the toplevel, the application will terminate.
  handleEvent :: Vty.Event -> StateT a IO Bool

  render :: a -> Vty.Image

data App = App

instance Widget App where
  handleEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) = pure False
  handleEvent _ = pure True

  render App = emptyImage

main :: Vty -> IO ()
main vty = evalStateT step App
  where
    step = do
      get >>= liftIO . Vty.update vty . Vty.picForImage . render
      handled <- liftIO (Vty.nextEvent vty) >>= handleEvent
      when handled step
