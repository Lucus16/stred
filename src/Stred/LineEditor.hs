module Stred.LineEditor where

import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty (Key (..))
import Stred.Image
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
        hcat
          [ raw (Text.take cursorPos contents)
          , style cursorStyle (raw (Text.take 1 (Text.drop cursorPos contents)))
          , raw (Text.drop (cursorPos + 1) contents)
          ]
    | otherwise =
        hcat [raw (Text.take cursorPos contents), style cursorStyle " "]
    where
      cursorStyle
        | active = defaultStyle{bgColor = Just 15, fgColor = Just 0}
        | otherwise = defaultStyle

  renderCollapsed LineEditor{contents}
    | Text.length contents < 40 = raw contents
    | otherwise = raw (Text.take 36 contents <> " ...")

instance Editor LineEditor where
  type Contents LineEditor = Text
  newEditor = LineEditor{contents = "", cursorPos = 0}
  editorFromContents contents = LineEditor{contents, cursorPos = 0}
  contentsFromEditor LineEditor{contents} = Just contents
