module Stred.ListEditor where

import Graphics.Vty (Key (..))
import Graphics.Vty qualified as Vty
import Graphics.Vty.Image ((<->), (<|>))
import Stred.Widget

data ListEditor ed
  = Empty
  | Editing [ed] ed [ed]
  | Navigating [ed] ed [ed]

instance (Editor ed) => HandleEvent (ListEditor ed) where
  handleKey NoMods key Empty = case key of
    KChar 'O' -> pure $ Just $ Editing [] newEditor []
    KChar 'o' -> pure $ Just $ Editing [] newEditor []
    _ -> pure Nothing
  handleKey mods key (Editing before cur after) =
    handleKey mods key cur >>= \case
      Just cur' -> pure $ Just $ Editing before cur' after
      Nothing -> case (mods, key) of
        (NoMods, KEsc) -> pure $ Just $ Navigating before cur after
        (NoMods, KLeft) -> pure $ Just $ Navigating before cur after
        _ -> pure Nothing
  handleKey NoMods key original@(Navigating before cur after) = case key of
    KUp -> case before of
      [] -> pure $ Just original
      x : before' -> pure $ Just $ Navigating before' x (cur : after)
    KDown -> case after of
      [] -> pure $ Just original
      x : after' -> pure $ Just $ Navigating (cur : before) x after'
    KChar 'd' -> pure $ Just $ Navigating before cur (cur : after)
    KEnter -> pure $ Just $ Editing before cur after
    KRight -> pure $ Just $ Editing before cur after
    KBS -> delete
    KChar 'x' -> delete
    KDel -> delete
    KPause -> delete
    KChar 'O' -> pure $ Just $ Editing before newEditor (cur : after)
    KChar 'o' -> pure $ Just $ Editing (cur : before) newEditor after
    KChar 'r' -> pure $ Just $ Editing before newEditor after
    _ -> pure Nothing
    where
      delete =
        case after of
          [] ->
            case before of
              [] -> pure $ Just Empty
              cur' : before' -> pure $ Just $ Navigating before' cur' []
          cur' : after' -> pure $ Just $ Navigating before cur' after'
  handleKey _ _ _ = pure Nothing

instance (Render ed) => Render (ListEditor ed) where
  render active = \case
    Empty -> Vty.text' activeAttr "(empty list)"
    Navigating before cur after ->
      Vty.vertCat (map renderInactive (reverse before))
        <-> (currentBullet <|> render False cur)
        <-> Vty.vertCat (map renderInactive after)
    Editing before cur after ->
      Vty.vertCat (map renderInactive (reverse before))
        <-> (bullet <|> render active cur)
        <-> Vty.vertCat (map renderInactive after)
    where
      renderInactive editor = bullet <|> render False editor
      activeAttr
        | active = Vty.withStyle Vty.defAttr Vty.reverseVideo
        | otherwise = Vty.defAttr
      bullet = Vty.text' Vty.defAttr "• "
      currentBullet
        | active = Vty.text' Vty.defAttr "➤ "
        | otherwise = bullet

  renderCollapsed = \case
    Empty -> Vty.text' Vty.defAttr "(empty list)"
    Navigating before _ after ->
      Vty.string Vty.defAttr $
        "(list of " <> show (length before + 1 + length after) <> " items)"
    Editing before _ after ->
      Vty.string Vty.defAttr $
        "(list of " <> show (length before + 1 + length after) <> " items)"

instance (Editor ed) => Editor (ListEditor ed) where
  type Contents (ListEditor ed) = [Contents ed]
  newEditor = Empty
  editorFromContents xs = case map editorFromContents xs of
    [] -> Empty
    (ed : eds) -> Navigating [] ed eds
  contentsFromEditor Empty = Just []
  contentsFromEditor (Editing before cur after) =
    traverse contentsFromEditor $ reverse before <> [cur] <> after
  contentsFromEditor (Navigating before cur after) =
    traverse contentsFromEditor $ reverse before <> [cur] <> after
