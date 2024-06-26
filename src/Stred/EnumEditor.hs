module Stred.EnumEditor where

import Graphics.Vty (Key (..))
import Graphics.Vty qualified as Vty
import Stred.Widget

data EnumEditor a = EnumEditor
  { contents :: a
  , cursor :: a
  }

instance (Bounded a, Enum a, Ord a, Show a) => Widget (EnumEditor a) where
  handleEvent (Vty.EvKey key []) original@EnumEditor{contents, cursor} = pure case key of
    KChar ' ' -> pure original{contents = cursor}
    KLeft
      | cursor == minBound -> pure original
      | otherwise -> pure original{cursor = pred cursor}
    KRight
      | cursor == maxBound -> pure original
      | otherwise -> pure original{cursor = succ cursor}
    KHome -> pure original{cursor = minBound}
    KEnd -> pure original{cursor = maxBound}
    _ -> Nothing
  handleEvent _ _ = pure Nothing

  render active EnumEditor{contents, cursor} =
    Vty.horizCat $ map renderItem [minBound .. maxBound]
    where
      pad x = " " <> x <> " "

      renderItem :: a -> Vty.Image
      renderItem x = Vty.string (contentsAttr (cursorAttr Vty.defAttr)) (pad (bullet <> show x))
        where
          bullet
            | x == contents = "◉ "
            | otherwise = "○ "

          contentsAttr
            | x == contents = flip Vty.withStyle Vty.bold
            | otherwise = id

          cursorAttr
            | x == cursor && active = flip Vty.withStyle Vty.reverseVideo
            | otherwise = id
