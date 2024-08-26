module Stred.SelectByKey where

import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Stred.Image
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
    case nonEmpty (Map.assocs s) of
      Nothing -> "(no options available)"
      Just assocs -> hcat $ NonEmpty.intersperse " " $ renderOne <$> assocs
    where
      renderOne (key, (name, _))
        | key `Text.elem` name =
            hcat
              [ raw (Text.takeWhile (/= key) name)
              , shortkeyStyle (raw (Text.singleton key))
              , raw (Text.tail (Text.dropWhile (/= key) name))
              ]
        | otherwise = hcat [shortkeyStyle (raw (Text.singleton key)), ":", raw name]
      shortkeyStyle
        | active = underline 15
        | otherwise = id

  renderCollapsed _ = "(choice)"
