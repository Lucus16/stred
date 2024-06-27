module Stred.EnumEditor where

import Graphics.Vty (Key (..))
import Graphics.Vty qualified as Vty
import Stred.Widget

data EnumEditor a = EnumEditor
  { contents :: a
  , cursor :: a
  }

instance (Bounded a, Enum a, Eq a) => HandleEvent (EnumEditor a) where
  handleKey NoMods key original@EnumEditor{cursor} = pure case key of
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
  handleKey _ _ _ = pure Nothing

instance (Bounded a, Enum a, Eq a, Show a) => Render (EnumEditor a) where
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

instance (Bounded a, Enum a, Eq a, Show a) => Editor (EnumEditor a) where
  type Contents (EnumEditor a) = a
  newEditor = EnumEditor{contents = minBound, cursor = minBound}
  editorFromContents x = EnumEditor{contents = x, cursor = x}
  contentsFromEditor EnumEditor{contents} = Just contents
