module Stred where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Graphics.Vty (Key (..), Vty)
import Graphics.Vty qualified as Vty
import Stred.Image
import Stred.JsonEditor
import Stred.Widget

newtype App a = App {top :: a}
  deriving newtype (Render, Editor)

instance (HandleEvent a) => HandleEvent (App a) where
  handleKey Ctrl (KChar 'q') _ = pure Nothing
  handleKey mods key (App a) = Just . App . fromMaybe a <$> handleKey mods key a

initialApp :: App JsonEditor
initialApp = newEditor

main :: Vty -> IO ()
main vty = step initialApp
  where
    step app = do
      event <- liftIO do
        (w, h) <- Vty.displayBounds out
        Vty.outputByteBuffer out $ renderImage $ crop w (h - 1) $ render True app
        Vty.nextEvent vty
      case event of
        Vty.EvKey key mods -> handleKey (toMods mods) key app >>= traverse_ step
        _ -> step app

    out = Vty.outputIface vty
