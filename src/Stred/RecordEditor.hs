module Stred.RecordEditor where

-- Keep insertion order
-- Allow duplicate keys
-- Show all instances of duplicate keys in red
-- Select a key-value pair or edit either a key or a value

-- up/down switch between fields
-- j,left,escape defocus record
-- ;,right,enter edit selected value
-- rn edit selected name

import Data.Map qualified as Map
import Data.Tape qualified as Tape
import Graphics.Vty (Key (..))
import Stred.Image
import Stred.LineEditor
import Stred.Prelude
import Stred.Widget

data Field ed = Field {fKey :: LineEditor, fValue :: ed}

data RecordEditor ed
  = Empty
  | EditingKey (Tape (Field ed))
  | EditingValue (Tape (Field ed))
  | Navigating (Tape (Field ed))

instance (Editor ed) => HandleEvent (RecordEditor ed) where
  handleKey NoMods key Empty = case key of
    KChar 'O' -> pure $ Just $ EditingKey $ Tape.singleton $ Field newEditor newEditor
    KChar 'o' -> pure $ Just $ EditingKey $ Tape.singleton $ Field newEditor newEditor
    _ -> pure Nothing
  handleKey NoMods key (Navigating xs) = case key of
    KUp -> pure $ Just $ Navigating $ try Tape.previous xs
    KChar 'l' -> pure $ Just $ Navigating $ try Tape.previous xs
    KDown -> pure $ Just $ Navigating $ try Tape.next xs
    KChar 'k' -> pure $ Just $ Navigating $ try Tape.next xs
    KChar 'n' -> pure $ Just $ EditingKey xs
    KChar 'd' -> pure $ Just $ Navigating $ Tape.pushBefore (Tape.peek xs) xs
    KChar 'g' -> pure $ Just $ Navigating $ Tape.first xs
    KChar 'G' -> pure $ Just $ Navigating $ Tape.last xs
    KHome -> pure $ Just $ Navigating $ Tape.first xs
    KEnd -> pure $ Just $ Navigating $ Tape.last xs
    KEnter -> pure $ Just $ EditingValue xs
    KChar ';' -> pure $ Just $ EditingValue xs
    KRight -> pure $ Just $ EditingValue xs
    KBS -> pure $ Just $ maybe Empty Navigating $ Tape.discard xs
    KChar 'x' -> pure $ Just $ maybe Empty Navigating $ Tape.discard xs
    KDel -> pure $ Just $ maybe Empty Navigating $ Tape.discard xs
    KPause -> pure $ Just $ maybe Empty Navigating $ Tape.discard xs
    KChar 'O' -> pure $ Just $ EditingKey $ Tape.pushBefore (Field newEditor newEditor) xs
    KChar 'o' -> pure $ Just $ EditingKey $ Tape.pushAfter (Field newEditor newEditor) xs
    KChar 's' -> pure $ Just $ EditingValue $ Tape.mapCurrent (\f -> f{fValue = newEditor}) xs
    _ -> pure Nothing
  handleKey mods key (EditingKey xs) =
    handleKey mods key (fKey (Tape.peek xs)) >>= \case
      Just k' -> pure $ Just $ EditingKey $ Tape.mapCurrent (\f -> f{fKey = k'}) xs
      Nothing -> case (mods, key) of
        (NoMods, KEsc) -> do
          k' <- handleUnfocus (fKey (Tape.peek xs))
          pure $ Just $ Navigating $ Tape.mapCurrent (\f -> f{fKey = k'}) xs
        (NoMods, KChar '\t') -> do
          k' <- handleUnfocus (fKey (Tape.peek xs))
          pure $ Just $ EditingValue $ Tape.mapCurrent (\f -> f{fKey = k'}) xs
        _ -> pure Nothing
  handleKey mods key (EditingValue xs) =
    handleKey mods key (fValue (Tape.peek xs)) >>= \case
      Just v' -> pure $ Just $ EditingValue $ Tape.mapCurrent (\f -> f{fValue = v'}) xs
      Nothing -> case (mods, key) of
        (NoMods, KEsc) -> do
          v' <- handleUnfocus (fValue (Tape.peek xs))
          pure $ Just $ Navigating $ Tape.mapCurrent (\f -> f{fValue = v'}) xs
        (NoMods, KChar '\t') -> do
          v' <- handleUnfocus (fValue (Tape.peek xs))
          pure $ Just $ EditingKey $ Tape.mapCurrent (\f -> f{fValue = v'}) xs
        _ -> pure Nothing
  handleKey _ _ _ = pure Nothing

instance (Render ed) => Render (RecordEditor ed) where
  render active = \case
    Empty -> applyWhen active (bg 8) "(empty record)"
    Navigating xs -> vcat $ Tape.toNonEmptyWith renderInactive renderActive xs
    EditingKey xs -> vcat $ Tape.toNonEmptyWith renderInactive renderEditingKey xs
    EditingValue xs -> vcat $ Tape.toNonEmptyWith renderInactive renderEditingValue xs
    where
      renderInactive (Field key value) = hcat [render False key, ": ", render False value]
      renderActive = applyWhen active (bg 8) . renderInactive
      renderEditingKey (Field key value) = hcat [render active key, ": ", render False value]
      renderEditingValue (Field key value) = hcat [render False key, ": ", render active value]

  renderCollapsed = \case
    Empty -> "(empty record)"
    Navigating xs -> hcat ["(record of ", ishow (Tape.size xs), " items)"]
    EditingKey xs -> hcat ["(record of ", ishow (Tape.size xs), " items)"]
    EditingValue xs -> hcat ["(record of ", ishow (Tape.size xs), " items)"]

instance (Editor ed) => Editor (RecordEditor ed) where
  type Contents (RecordEditor ed) = Map Text (Contents ed)
  newEditor = Empty
  editorFromContents kvs = maybe Empty Navigating $ Tape.fromList $ mkField <$> Map.toList kvs
    where
      mkField :: (Text, Contents ed) -> Field ed
      mkField (key, value) = Field (editorFromContents key) (editorFromContents value)
  contentsFromEditor re = do
    kvs <-
      Map.fromList <$> for fields \(Field key value) -> do
        k <- contentsFromEditor key
        v <- contentsFromEditor value
        pure (k, v)
    guard $ Map.size kvs == length fields
    pure kvs
    where
      fields = case re of
        Empty -> []
        Navigating xs -> Tape.toList xs
        EditingKey xs -> Tape.toList xs
        EditingValue xs -> Tape.toList xs
