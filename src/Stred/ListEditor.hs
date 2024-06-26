module Stred.ListEditor where

import Graphics.Vty (Key (..))
import Graphics.Vty qualified as Vty
import Graphics.Vty.Image ((<->), (<|>))
import Stred.Widget

data ListEditor ed
  = Empty
  | Editing [ed] ed [ed]
  | Navigating [ed] ed [ed]

newListEditor :: [ed] -> ListEditor ed
newListEditor = \case
  [] -> Empty
  (x : xs) -> Navigating [] x xs

instance (Widget ed) => Widget (ListEditor ed) where
  handleEvent ev@(Vty.EvKey key []) (Editing before cur after) =
    handleEvent ev cur >>= \case
      Just cur' -> pure $ Just $ Editing before cur' after
      Nothing -> case key of
        KEsc -> pure $ Just $ Navigating before cur after
        KLeft -> pure $ Just $ Navigating before cur after
        _ -> pure Nothing
  handleEvent (Vty.EvKey key []) original@(Navigating before cur after) = case key of
    KUp -> case before of
      [] -> pure $ Just original
      x : before' -> pure $ Just $ Navigating before' x (cur : after)
    KDown -> case after of
      [] -> pure $ Just original
      x : after' -> pure $ Just $ Navigating (cur : before) x after'
    KEnter -> pure $ Just $ Editing before cur after
    KRight -> pure $ Just $ Editing before cur after
    KBS -> delete
    KChar 'x' -> delete
    KDel -> delete
    KPause -> delete
    _ -> pure Nothing
    where
      delete =
        case after of
          [] ->
            case before of
              [] -> pure $ Just Empty
              cur' : before' -> pure $ Just $ Navigating before' cur' []
          cur' : after' -> pure $ Just $ Navigating before cur' after'
  handleEvent _ _ = pure Nothing

  render active = \case
    Empty -> Vty.text' activeAttr "(empty list)"
    Navigating before cur after ->
      Vty.vertCat (map renderInactive (reverse before))
        <-> (bullet <|> render False cur)
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
