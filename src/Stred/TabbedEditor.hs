module Stred.TabbedEditor
  ( Tab (..)
  , TabbedEditor (..)
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Graphics.Vty (Key (..))
import Stred.Image
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
    vcat
      [ hcat (NonEmpty.intersperse " " tabs)
      , render False (contents cur)
      ]
    where
      tabs =
        NonEmpty.prependList (map (raw . name) (reverse before)) $
          tabStyle (raw (name cur)) :| map (raw . name) after
      tabStyle
        | active = bg 15 . fg 0
        | otherwise = bold
  render active (Editing before cur after) =
    vcat
      [ hcat (NonEmpty.intersperse " " tabs)
      , render active (contents cur)
      ]
    where
      tabs =
        NonEmpty.prependList (map (raw . name) (reverse before)) $
          selectedTabStyle (raw (name cur))
            :| map (raw . name) after

  renderCollapsed (Navigating _ cur _) =
    raw (name cur <> " ...")
  renderCollapsed (Editing _ cur _) =
    raw (name cur <> " ...")

selectedTabStyle :: Sized Image -> Sized Image
selectedTabStyle = bg 7 . fg 0
