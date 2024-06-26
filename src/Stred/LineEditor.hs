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

newLineEditor :: LineEditor
newLineEditor =
  LineEditor
    { contents = ""
    , cursorPos = 0
    }

instance Widget LineEditor where
  handleEvent (Vty.EvKey key []) original@LineEditor{contents, cursorPos} = pure case key of
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
  handleEvent _ _ = pure Nothing

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
