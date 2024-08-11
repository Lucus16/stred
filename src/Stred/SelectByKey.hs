module Stred.SelectByKey where

import Data.List (intersperse)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Graphics.Vty.Image ((<|>))
import Stred.Widget

data SelectByKey ed
  = SelectByKey (Map Char (Text, ed))
  | Selected ed

instance (HandleEvent ed) => HandleEvent (SelectByKey ed) where
  handleKey mods key (Selected s) = fmap Selected <$> handleKey mods key s
  handleKey NoMods (Vty.KChar key) (SelectByKey eds) =
    pure $ Selected . snd <$> Map.lookup key eds
  handleKey _ _ _ = pure Nothing

instance (Render ed) => Render (SelectByKey ed) where
  render active (Selected s) = render active s
  render active (SelectByKey s) =
    Vty.horizCat $
      intersperse (Vty.text Vty.defAttr " ") $
        renderOne <$> Map.assocs s
    where
      renderOne (key, (name, _))
        | key `Text.elem` name =
            Vty.text' Vty.defAttr (Text.takeWhile (/= key) name)
              <|> Vty.text' attr (Text.singleton key)
              <|> Vty.text' Vty.defAttr (Text.tail (Text.dropWhile (/= key) name))
        | otherwise =
            Vty.text' attr (Text.singleton key)
              <|> Vty.text' Vty.defAttr (":" <> name)
      attr
        | active = Vty.withStyle Vty.defAttr Vty.underline
        | otherwise = Vty.defAttr

  renderCollapsed _ = Vty.text Vty.defAttr "undefined"
