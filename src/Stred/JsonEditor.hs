module Stred.JsonEditor where

import Data.Aeson (Value (..))
import Data.Vector qualified as Vector
import Graphics.Vty (Key (..))
import Graphics.Vty qualified as Vty
import Stred.EnumEditor
import Stred.LineEditor
import Stred.ListEditor
import Stred.Widget

data JsonEditor
  = ChoosingEditor
  | JsonEditorNull
  | BoolEditor (EnumEditor Bool)
  | StringEditor LineEditor
  | ArrayEditor (ListEditor JsonEditor)

instance HandleEvent JsonEditor where
  handleKey mods key = \case
    JsonEditorNull -> pure Nothing
    BoolEditor e -> fmap BoolEditor <$> handleKey mods key e
    StringEditor e -> fmap StringEditor <$> handleKey mods key e
    ArrayEditor e -> fmap ArrayEditor <$> handleKey mods key e
    ChoosingEditor -> case (mods, key) of
      (NoMods, KChar 'n') -> pure $ Just JsonEditorNull
      (NoMods, KChar 'b') -> pure $ Just $ BoolEditor newEditor
      (NoMods, KChar 's') -> pure $ Just $ StringEditor newEditor
      (NoMods, KChar 'a') -> pure $ Just $ ArrayEditor newEditor
      _ -> pure Nothing

instance Render JsonEditor where
  render _ ChoosingEditor = Vty.text Vty.defAttr "Null Bool Float String Array Object"
  render _ JsonEditorNull = Vty.text Vty.defAttr "null"
  render active (BoolEditor ed) = render active ed
  render active (StringEditor ed) = render active ed
  render active (ArrayEditor ed) = render active ed

  renderCollapsed ChoosingEditor = Vty.text Vty.defAttr "⎵"
  renderCollapsed JsonEditorNull = Vty.text Vty.defAttr "null"
  renderCollapsed (BoolEditor ed) = renderCollapsed ed
  renderCollapsed (StringEditor ed) = renderCollapsed ed
  renderCollapsed (ArrayEditor ed) = renderCollapsed ed

instance Editor JsonEditor where
  type Contents JsonEditor = Value
  newEditor = JsonEditorNull
  editorFromContents Null = JsonEditorNull
  editorFromContents (Bool b) = BoolEditor (editorFromContents b)
  editorFromContents (String t) = StringEditor (editorFromContents t)
  editorFromContents (Array l) = ArrayEditor (editorFromContents (Vector.toList l))
  editorFromContents (Number n) = undefined
  editorFromContents (Object o) = undefined
  contentsFromEditor = \case
    ChoosingEditor -> Nothing
    JsonEditorNull -> Just Null
    BoolEditor e -> Bool <$> contentsFromEditor e
    StringEditor e -> String <$> contentsFromEditor e
    ArrayEditor e -> Array . Vector.fromList <$> contentsFromEditor e
