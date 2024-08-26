module Stred.EnumEditor where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Graphics.Vty (Key (..))
import Stred.Image
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
    hcat $ fmap renderItem (NonEmpty.fromList [minBound .. maxBound])
    where
      renderItem :: a -> Sized Image
      renderItem x =
        style (contentsAttr (cursorAttr defaultStyle)) $
          hcat [" ", bullet, ishow x, " "]
        where
          bullet
            | x == contents = "◉ "
            | otherwise = "○ "

          contentsAttr
            | x == contents = \s -> s{bold = Just True}
            | otherwise = id

          cursorAttr
            | x == cursor && active = \s -> s{bgColor = Just 8}
            | otherwise = id

  renderCollapsed EnumEditor{contents} = raw (Text.pack (show contents) <> " ...")

instance (Bounded a, Enum a, Eq a, Show a) => Editor (EnumEditor a) where
  type Contents (EnumEditor a) = a
  newEditor = EnumEditor{contents = minBound, cursor = minBound}
  editorFromContents x = EnumEditor{contents = x, cursor = x}
  contentsFromEditor EnumEditor{contents} = Just contents
