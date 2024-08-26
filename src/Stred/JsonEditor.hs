module Stred.JsonEditor where

import Data.Aeson (Value (..))
import Data.Map qualified as Map
import Data.Scientific (Scientific)
import Data.Vector qualified as Vector
import Graphics.Vty (Key (..))
import Stred.EnumEditor
import Stred.Image
import Stred.LineEditor
import Stred.ListEditor
import Stred.ReadShowEditor
import Stred.SelectByKey
import Stred.Widget

newtype JsonEditor = JsonEditor (SelectByKey JsonEditor1)
  deriving newtype (Render)

jsonEditor :: JsonEditor
jsonEditor =
  JsonEditor $
    SelectByKey $
      Map.fromList
        [ ('n', ("null", JsonEditorNull))
        , ('b', ("bool", BoolEditor newEditor))
        , ('f', ("number", NumberEditor newEditor))
        , ('s', ("string", StringEditor newEditor))
        , ('a', ("array", ArrayEditor newEditor))
        ]

data JsonEditor1
  = JsonEditorNull
  | BoolEditor (EnumEditor Bool)
  | NumberEditor (ReadShowEditor Scientific)
  | StringEditor LineEditor
  | ArrayEditor (ListEditor JsonEditor)

instance HandleEvent JsonEditor1 where
  handleKey mods key = \case
    JsonEditorNull -> pure Nothing
    BoolEditor e -> fmap BoolEditor <$> handleKey mods key e
    NumberEditor e -> fmap NumberEditor <$> handleKey mods key e
    StringEditor e -> fmap StringEditor <$> handleKey mods key e
    ArrayEditor e -> fmap ArrayEditor <$> handleKey mods key e

instance HandleEvent JsonEditor where
  handleKey mods key (JsonEditor s) = do
    handleKey mods key s >>= \case
      Just s' -> pure $ Just $ JsonEditor s'
      Nothing -> case (mods, key) of
        (NoMods, KChar 'r') -> pure $ Just jsonEditor
        (Ctrl, KChar 'r') -> pure $ Just jsonEditor
        _ -> pure Nothing

quoteBar :: Sized Image
quoteBar = "┃ "

instance Render JsonEditor1 where
  render _ JsonEditorNull = "null"
  render active (BoolEditor ed) = render active ed
  render active (NumberEditor ed) = render active ed
  render active (StringEditor ed) = hcat [quoteBar, render active ed]
  render active (ArrayEditor ed) = render active ed

  renderCollapsed JsonEditorNull = "null"
  renderCollapsed (BoolEditor ed) = renderCollapsed ed
  renderCollapsed (NumberEditor ed) = renderCollapsed ed
  renderCollapsed (StringEditor ed) = hcat [quoteBar, renderCollapsed ed]
  renderCollapsed (ArrayEditor ed) = renderCollapsed ed

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
      Object o -> undefined
  contentsFromEditor (JsonEditor (SelectByKey _)) = Nothing
  contentsFromEditor (JsonEditor (Selected s)) = case s of
    JsonEditorNull -> Just Null
    BoolEditor e -> Bool <$> contentsFromEditor e
    NumberEditor e -> Number <$> contentsFromEditor e
    StringEditor e -> String <$> contentsFromEditor e
    ArrayEditor e -> Array . Vector.fromList <$> contentsFromEditor e
