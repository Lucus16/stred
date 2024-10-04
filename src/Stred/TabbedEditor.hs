module Stred.TabbedEditor
  ( Tab (..)
  , TabbedEditor (..)
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Tape qualified as Tape
import Graphics.Vty (Key (..))
import Stred.Image
import Stred.Prelude
import Stred.Widget

data Tab a = Tab
  { name :: Text
  , body :: SomeEditor a
  }

data TabbedEditor a
  = Navigating (Tape (Tab a))
  | Editing (Tape (Tab a))

instance (Bounded a, Enum a, Eq a) => HandleEvent (TabbedEditor a) where
  handleKey mods key (Editing xs) =
    handleKey mods key (body (Tape.peek xs)) >>= \case
      Just cur' -> pure $ Just $ Editing $ Tape.mapCurrent (\tab -> tab{body = cur'}) xs
      Nothing -> case (mods, key) of
        (NoMods, KEsc) -> pure $ Just $ Navigating xs
        _ -> pure Nothing
  handleKey NoMods key (Navigating xs) = case key of
    KLeft -> pure $ Just $ Navigating $ try Tape.previous xs
    KRight -> pure $ Just $ Navigating $ try Tape.next xs
    _ -> pure Nothing
  handleKey _ _ _ = pure Nothing

instance Render (TabbedEditor a) where
  render active (Navigating xs) =
    vcat
      [ hcat (NonEmpty.intersperse " " tabs)
      , render False (body (Tape.peek xs))
      ]
    where
      tabs = Tape.toNonEmptyWith (raw . name) (tabStyle . raw . name) xs
      tabStyle
        | active = bg 15 . fg 0
        | otherwise = bold
  render active (Editing xs) =
    vcat
      [ hcat (NonEmpty.intersperse " " tabs)
      , render active (body (Tape.peek xs))
      ]
    where
      tabs = Tape.toNonEmptyWith (raw . name) (selectedTabStyle . raw . name) xs

  renderCollapsed (Navigating xs) =
    raw (name (Tape.peek xs) <> " ...")
  renderCollapsed (Editing xs) =
    raw (name (Tape.peek xs) <> " ...")

selectedTabStyle :: Sized Image -> Sized Image
selectedTabStyle = bg 7 . fg 0
