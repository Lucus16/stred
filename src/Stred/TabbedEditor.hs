module Stred.TabbedEditor where

import Data.List (intersperse)
import Data.Text (Text)
import Graphics.Vty (Key (..))
import Graphics.Vty qualified as Vty
import Graphics.Vty.Image ((<->))
import Stred.Widget

data Tab a = Tab
  { name :: Text
  , contents :: SomeEditor a
  }

data TabbedEditor a
  = Navigating [Tab a] (Tab a) [Tab a]
  | Editing [Tab a] (Tab a) [Tab a]

instance (Bounded a, Enum a, Eq a) => HandleEvent (TabbedEditor a) where
  handleKey mods key (Editing before cur after) =
    handleKey mods key (contents cur) >>= \case
      Just cur' -> pure $ Just $ Editing before cur{contents = cur'} after
      Nothing -> case (mods, key) of
        (NoMods, KEsc) -> pure $ Just $ Navigating before cur after
        _ -> pure Nothing
  handleKey NoMods key original@(Navigating before cur after) = case key of
    KLeft -> case before of
      [] -> pure $ Just original
      x : before' -> pure $ Just $ Navigating before' x (cur : after)
    KRight -> case after of
      [] -> pure $ Just original
      x : after' -> pure $ Just $ Navigating (cur : before) x after'
    _ -> pure Nothing
  handleKey _ _ _ = pure Nothing

instance Render (TabbedEditor a) where
  render active (Navigating before cur after) =
    Vty.horizCat (intersperse space tabs) <-> render active (contents cur)
    where
      space = Vty.text' Vty.defAttr " "
      tabs =
        map (Vty.text' Vty.defAttr . name) (reverse before)
          <> [Vty.text' (Vty.withStyle Vty.defAttr Vty.reverseVideo) (name cur)]
          <> map (Vty.text' Vty.defAttr . name) after
  render active (Editing before cur after) =
    Vty.horizCat (intersperse space tabs) <-> render active (contents cur)
    where
      space = Vty.text' Vty.defAttr " "
      tabs =
        map (Vty.text' Vty.defAttr . name) (reverse before)
          <> [Vty.text' (Vty.withStyle Vty.defAttr Vty.reverseVideo) (name cur)]
          <> map (Vty.text' Vty.defAttr . name) after
