module Stred where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Graphics.Vty (Key (..), Vty)
import Graphics.Vty qualified as Vty
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
      liftIO $ Vty.update vty $ Vty.picForImage $ render True app
      liftIO (Vty.nextEvent vty) >>= \case
        Vty.EvKey key mods -> handleKey (toMods mods) key app >>= traverse_ step
        _ -> step app
