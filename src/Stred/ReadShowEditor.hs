module Stred.ReadShowEditor where

import Data.Text qualified as Text
import Graphics.Vty (Key (..))
import Stred.LineEditor
import Stred.Widget
import Text.Read (readMaybe)

data ReadShowEditor a = ReadShowEditor
  { lastValid :: Maybe a
  , editor :: LineEditor
  }

updateEditor :: (Read a, Show a) => LineEditor -> ReadShowEditor a -> ReadShowEditor a
updateEditor editor s = case contentsFromEditor s of
  Nothing -> s{editor}
  Just x -> s{lastValid = Just x, editor}

instance (Read a, Show a) => HandleEvent (ReadShowEditor a) where
  handleKey mods key s =
    handleKey mods key (editor s) >>= \case
      Just ed' -> pure $ Just $ updateEditor ed' s
      Nothing -> case (mods, key) of
        (NoMods, KEnter) -> case contentsFromEditor s of
          Nothing -> pure $ Just s
          Just valid -> pure $ Just s{lastValid = Just valid, editor = editorFromContents $ Text.pack $ show valid}
        (NoMods, KEsc) -> case lastValid s of
          Nothing -> pure $ Just s
          Just valid -> pure $ Just s{editor = editorFromContents $ Text.pack $ show valid}
        _ -> pure Nothing

instance Render (ReadShowEditor a) where
  render active ReadShowEditor{editor} =
    render active editor

  renderCollapsed ReadShowEditor{editor} = renderCollapsed editor

instance (Read a, Show a) => Editor (ReadShowEditor a) where
  type Contents (ReadShowEditor a) = a
  newEditor = ReadShowEditor{lastValid = Nothing, editor = newEditor}
  editorFromContents x = ReadShowEditor{lastValid = Just x, editor = editorFromContents $ Text.pack $ show x}
  contentsFromEditor s = readMaybe . Text.unpack =<< contentsFromEditor (editor s)
