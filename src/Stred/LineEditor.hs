module Stred.LineEditor where

import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty (Key (..))
import Graphics.Vty qualified as Vty
import Graphics.Vty.Image ((<|>))
import Stred.Widget

data LineEditor = LineEditor
  { contents :: Text
  , cursorPos :: Int
  }

instance HandleEvent LineEditor where
  handleKey NoMods key original@LineEditor{contents, cursorPos} = pure case key of
    KChar c -> pure original{contents = Text.take cursorPos contents <> Text.singleton c <> Text.drop cursorPos contents, cursorPos = cursorPos + 1}
    KPause
      | cursorPos == Text.length contents -> pure original
      | otherwise -> pure original{contents = Text.take cursorPos contents <> Text.drop (cursorPos + 1) contents}
    KDel
      | cursorPos == Text.length contents -> pure original
      | otherwise -> pure original{contents = Text.take cursorPos contents <> Text.drop (cursorPos + 1) contents}
    KBS
      | cursorPos == 0 -> pure original
      | otherwise -> pure original{contents = Text.take (cursorPos - 1) contents <> Text.drop cursorPos contents, cursorPos = cursorPos - 1}
    KLeft
      | cursorPos == 0 -> pure original
      | otherwise -> pure original{cursorPos = cursorPos - 1}
    KRight
      | cursorPos == Text.length contents -> pure original
      | otherwise -> pure original{cursorPos = cursorPos + 1}
    KHome -> pure original{cursorPos = 0}
    KEnd -> pure original{cursorPos = Text.length contents}
    _ -> Nothing
  handleKey _ _ _ = pure Nothing

instance Render LineEditor where
  render active LineEditor{contents, cursorPos}
    | cursorPos < Text.length contents =
        Vty.text' Vty.defAttr (Text.take cursorPos contents)
          <|> Vty.text' cursorAttr (Text.take 1 (Text.drop cursorPos contents))
          <|> Vty.text' Vty.defAttr (Text.drop (cursorPos + 1) contents)
    | otherwise =
        Vty.text' Vty.defAttr (Text.take cursorPos contents)
          <|> Vty.text' cursorAttr " "
    where
      cursorAttr
        | active = Vty.withStyle Vty.defAttr Vty.reverseVideo
        | otherwise = Vty.defAttr

  renderCollapsed LineEditor{contents}
    | Text.length contents < 40 = Vty.text' Vty.defAttr contents
    | otherwise = Vty.text' Vty.defAttr (Text.take 36 contents <> " ...")

instance Editor LineEditor where
  type Contents LineEditor = Text
  newEditor = LineEditor{contents = "", cursorPos = 0}
  editorFromContents contents = LineEditor{contents, cursorPos = 0}
  contentsFromEditor LineEditor{contents} = Just contents
