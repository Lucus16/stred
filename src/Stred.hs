module Stred where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Graphics.Vty (Vty)
import Graphics.Vty qualified as Vty
import Stred.LineEditor
import Stred.ListEditor
import Stred.Widget

newtype App a = App {top :: a}

instance (Widget a) => Widget (App a) where
  handleEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) _ = pure Nothing
  handleEvent ev (App a) = Just . App . fromMaybe a <$> handleEvent ev a
  render active (App w) = render active w

initialApp :: App (ListEditor LineEditor)
initialApp =
  App $ newListEditor $ replicate 5 newLineEditor

main :: Vty -> IO ()
main vty = step initialApp
  where
    step app = do
      liftIO $ Vty.update vty $ Vty.picForImage $ render True app
      liftIO (Vty.nextEvent vty) >>= flip handleEvent app >>= traverse_ step
