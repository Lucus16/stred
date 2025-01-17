module Stred.JsonEditor where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map qualified as Map
import Data.Scientific (Scientific)
import Data.Vector qualified as Vector
import Graphics.Vty (Key (..))
import Stred.EnumEditor
import Stred.Image
import Stred.LineEditor
import Stred.ListEditor
import Stred.Prelude
import Stred.ReadShowEditor
import Stred.RecordEditor
import Stred.SelectByKey
import Stred.Widget

newtype JsonEditor = JsonEditor (SelectByKey JsonEditor1)
  deriving newtype (Render)

jsonEditor :: JsonEditor
jsonEditor =
  JsonEditor $
    SelectByKey $
      Map.fromList
        [ ('z', ("null", JsonEditorNull))
        , ('f', ("false", BoolEditor (editorFromContents False)))
        , ('t', ("true", BoolEditor (editorFromContents True)))
        , ('n', ("number", NumberEditor (editorFromContents 0)))
        , ('s', ("string", StringEditor newEditor))
        , ('a', ("array", ArrayEditor newEditor))
        , ('o', ("object", RecordEditor newEditor))
        ]

data JsonEditor1
  = JsonEditorNull
  | BoolEditor (EnumEditor Bool)
  | NumberEditor (ReadShowEditor Scientific)
  | StringEditor LineEditor
  | ArrayEditor (ListEditor JsonEditor)
  | RecordEditor (RecordEditor JsonEditor)

instance HandleEvent JsonEditor1 where
  handleKey mods key = \case
    JsonEditorNull -> pure Nothing
    BoolEditor e -> fmap BoolEditor <$> handleKey mods key e
    NumberEditor e -> fmap NumberEditor <$> handleKey mods key e
    StringEditor e -> fmap StringEditor <$> handleKey mods key e
    ArrayEditor e -> fmap ArrayEditor <$> handleKey mods key e
    RecordEditor e -> fmap RecordEditor <$> handleKey mods key e

  handleUnfocus = \case
    JsonEditorNull -> pure JsonEditorNull
    BoolEditor e -> BoolEditor <$> handleUnfocus e
    NumberEditor e -> NumberEditor <$> handleUnfocus e
    StringEditor e -> StringEditor <$> handleUnfocus e
    ArrayEditor e -> ArrayEditor <$> handleUnfocus e
    RecordEditor e -> RecordEditor <$> handleUnfocus e

instance HandleEvent JsonEditor where
  handleKey mods key (JsonEditor s) = do
    handleKey mods key s >>= \case
      Just s' -> pure $ Just $ JsonEditor s'
      Nothing -> case (mods, key) of
        (NoMods, KChar 'r') -> pure $ Just jsonEditor
        (Ctrl, KChar 'r') -> pure $ Just jsonEditor
        _ -> pure Nothing

  handleUnfocus (JsonEditor e) = JsonEditor <$> handleUnfocus e

quoteBar :: Sized Image
quoteBar = "┃ "

instance Render JsonEditor1 where
  render _ JsonEditorNull = "null"
  render active (BoolEditor ed) = render active ed
  render active (NumberEditor ed) = render active ed
  render active (StringEditor ed) = hcat [quoteBar, render active ed]
  render active (ArrayEditor ed) = render active ed
  render active (RecordEditor ed) = render active ed

  renderCollapsed JsonEditorNull = "null"
  renderCollapsed (BoolEditor ed) = renderCollapsed ed
  renderCollapsed (NumberEditor ed) = renderCollapsed ed
  renderCollapsed (StringEditor ed) = hcat [quoteBar, renderCollapsed ed]
  renderCollapsed (ArrayEditor ed) = renderCollapsed ed
  renderCollapsed (RecordEditor ed) = renderCollapsed ed

instance Editor JsonEditor where
  type Contents JsonEditor = Value
  newEditor = jsonEditor
  editorFromContents =
    JsonEditor . Selected . \case
      Null -> JsonEditorNull
      Bool b -> BoolEditor (editorFromContents b)
      String s -> StringEditor (editorFromContents s)
      Array a -> ArrayEditor (editorFromContents (Vector.toList a))
      Number n -> NumberEditor (editorFromContents n)
      Object o -> RecordEditor (editorFromContents (KeyMap.toMapText o))
  contentsFromEditor (JsonEditor (SelectByKey _)) = Nothing
  contentsFromEditor (JsonEditor (Selected s)) = case s of
    JsonEditorNull -> Just Null
    BoolEditor e -> Bool <$> contentsFromEditor e
    NumberEditor e -> Number <$> contentsFromEditor e
    StringEditor e -> String <$> contentsFromEditor e
    ArrayEditor e -> Array . Vector.fromList <$> contentsFromEditor e
    RecordEditor e -> Object . KeyMap.fromMapText <$> contentsFromEditor e
