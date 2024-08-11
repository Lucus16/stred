module Stred.JsonEditor where

import Data.Aeson (Value (..))
import Data.Map qualified as Map
import Data.Vector qualified as Vector
import Graphics.Vty qualified as Vty
import Stred.EnumEditor
import Stred.LineEditor
import Stred.ListEditor
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
        , ('s', ("string", StringEditor newEditor))
        , ('a', ("array", ArrayEditor newEditor))
        ]

data JsonEditor1
  = JsonEditorNull
  | BoolEditor (EnumEditor Bool)
  | StringEditor LineEditor
  | ArrayEditor (ListEditor JsonEditor)

instance HandleEvent JsonEditor1 where
  handleKey mods key = \case
    JsonEditorNull -> pure Nothing
    BoolEditor e -> fmap BoolEditor <$> handleKey mods key e
    StringEditor e -> fmap StringEditor <$> handleKey mods key e
    ArrayEditor e -> fmap ArrayEditor <$> handleKey mods key e

instance HandleEvent JsonEditor where
  handleKey mods key (JsonEditor s) = do
    handleKey mods key s >>= \case
      Just s' -> pure $ Just $ JsonEditor s'
      Nothing -> case (mods, key) of
        (NoMods, Vty.KChar 'r') -> pure $ Just jsonEditor
        (Ctrl, Vty.KChar 'r') -> pure $ Just jsonEditor
        _ -> pure Nothing

instance Render JsonEditor1 where
  render _ JsonEditorNull = Vty.text Vty.defAttr "null"
  render active (BoolEditor ed) = render active ed
  render active (StringEditor ed) = render active ed
  render active (ArrayEditor ed) = render active ed

  renderCollapsed JsonEditorNull = Vty.text Vty.defAttr "null"
  renderCollapsed (BoolEditor ed) = renderCollapsed ed
  renderCollapsed (StringEditor ed) = renderCollapsed ed
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
      Number n -> undefined
      Object o -> undefined
  contentsFromEditor (JsonEditor (SelectByKey _)) = Nothing
  contentsFromEditor (JsonEditor (Selected s)) = case s of
    JsonEditorNull -> Just Null
    BoolEditor e -> Bool <$> contentsFromEditor e
    StringEditor e -> String <$> contentsFromEditor e
    ArrayEditor e -> Array . Vector.fromList <$> contentsFromEditor e
