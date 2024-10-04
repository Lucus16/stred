module Stred.ListEditor where

import Data.Tape qualified as Tape
import Graphics.Vty (Key (..))
import Stred.Image
import Stred.Prelude
import Stred.Widget

data ListEditor ed
  = Empty
  | Editing (Tape ed)
  | Navigating (Tape ed)

instance (Editor ed) => HandleEvent (ListEditor ed) where
  handleKey NoMods key Empty = case key of
    KChar 'O' -> pure $ Just $ Editing $ Tape.singleton newEditor
    KChar 'o' -> pure $ Just $ Editing $ Tape.singleton newEditor
    _ -> pure Nothing
  handleKey mods key (Editing xs) =
    handleKey mods key (Tape.peek xs) >>= \case
      Just cur' -> pure $ Just $ Editing $ Tape.replace cur' xs
      Nothing -> case (mods, key) of
        (NoMods, KEsc) -> do
          cur' <- handleUnfocus (Tape.peek xs)
          pure $ Just $ Navigating $ Tape.replace cur' xs
        (NoMods, KLeft) -> do
          cur' <- handleUnfocus (Tape.peek xs)
          pure $ Just $ Navigating $ Tape.replace cur' xs
        _ -> pure Nothing
  handleKey NoMods key (Navigating xs) =
    case key of
      KUp -> pure $ Just $ Navigating $ try Tape.previous xs
      KDown -> pure $ Just $ Navigating $ try Tape.next xs
      KChar 'd' -> pure $ Just $ Navigating $ Tape.pushBefore (Tape.peek xs) xs
      KChar 'g' -> pure $ Just $ Navigating $ Tape.first xs
      KChar 'G' -> pure $ Just $ Navigating $ Tape.last xs
      KHome -> pure $ Just $ Navigating $ Tape.first xs
      KEnd -> pure $ Just $ Navigating $ Tape.last xs
      KEnter -> pure $ Just $ Editing xs
      KRight -> pure $ Just $ Editing xs
      KBS -> pure $ Just $ maybe Empty Navigating $ Tape.discard xs
      KChar 'x' -> pure $ Just $ maybe Empty Navigating $ Tape.discard xs
      KDel -> pure $ Just $ maybe Empty Navigating $ Tape.discard xs
      KPause -> pure $ Just $ maybe Empty Navigating $ Tape.discard xs
      KChar 'O' -> pure $ Just $ Editing $ Tape.pushBefore newEditor xs
      KChar 'o' -> pure $ Just $ Editing $ Tape.pushAfter newEditor xs
      KChar 's' -> pure $ Just $ Editing $ Tape.replace newEditor xs
      _ -> pure Nothing
  handleKey _ _ _ = pure Nothing

instance (Render ed) => Render (ListEditor ed) where
  render active = \case
    Empty -> applyWhen active (bg 8) "(empty list)"
    Navigating xs -> vcat $ Tape.toNonEmptyWith renderInactive renderSelected xs
    Editing xs -> vcat $ Tape.toNonEmptyWith renderInactive renderEditing xs
    where
      renderInactive editor = hcat [bullet, render False editor]
      renderEditing editor = hcat [bullet, render active editor]
      renderSelected editor = hcat [selectedBullet, applyWhen active (bg 8) $ render False editor]
      bullet = "• "
      selectedBullet
        | active = "➤ "
        | otherwise = bullet

  renderCollapsed = \case
    Empty -> raw "(empty list)"
    Navigating xs -> hcat ["(list of ", ishow (Tape.size xs), " toNonEmpty)"]
    Editing xs -> hcat ["(list of ", ishow (Tape.size xs), " toNonEmpty)"]

instance (Editor ed) => Editor (ListEditor ed) where
  type Contents (ListEditor ed) = [Contents ed]
  newEditor = Empty
  editorFromContents xs = maybe Empty Navigating $ Tape.fromList $ map editorFromContents xs
  contentsFromEditor Empty = Just []
  contentsFromEditor (Editing xs) = traverse contentsFromEditor $ Tape.toList xs
  contentsFromEditor (Navigating xs) = traverse contentsFromEditor $ Tape.toList xs
