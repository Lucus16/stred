module Stred.Widget where

import Data.Fix (Fix (..))
import Graphics.Vty.Input.Events (Key, Modifier (..))
import Stred.Image

data Mods = Mods {ctrl :: Bool, alt :: Bool, shift :: Bool}

pattern NoMods, Ctrl, Alt, Shift, CtrlAlt, CtrlShift, AltShift, CtrlAltShift :: Mods
pattern NoMods = Mods False False False
pattern Ctrl = Mods True False False
pattern Alt = Mods False True False
pattern Shift = Mods False False True
pattern CtrlAlt = Mods True True False
pattern CtrlShift = Mods True False True
pattern AltShift = Mods False True True
pattern CtrlAltShift = Mods True True True

toMods :: [Modifier] -> Mods
toMods = go NoMods
  where
    go mods [] = mods
    go mods (MCtrl : more) = go mods{ctrl = True} more
    go mods (MMeta : more) = go mods{alt = True} more
    go mods (MAlt : more) = go mods{alt = True} more
    go mods (MShift : more) = go mods{shift = True} more

class HandleEvent a where
  -- | Nothing means the event was not handled. If the toplevel app does not
  -- handle an event, the system terminates.
  handleKey :: Mods -> Key -> a -> IO (Maybe a)

  handleUnfocus :: a -> IO a
  handleUnfocus = pure

class Render a where
  render :: Bool -> a -> Sized Image
  renderCollapsed :: a -> Sized Image

class (HandleEvent a, Render a) => Editor a where
  type Contents a
  newEditor :: a
  editorFromContents :: Contents a -> a
  contentsFromEditor :: a -> Maybe (Contents a)

data SomeEditor a where
  SomeEditor :: (Editor ed, Contents ed ~ a) => ed -> SomeEditor a

instance HandleEvent (SomeEditor a) where
  handleKey mods key (SomeEditor ed) = fmap SomeEditor <$> handleKey mods key ed

instance Render (SomeEditor a) where
  render active (SomeEditor ed) = render active ed
  renderCollapsed (SomeEditor ed) = renderCollapsed ed

class Editable a where
  type DefaultEditor a
  defaultEditor :: (Editor (DefaultEditor a), Contents (DefaultEditor a) ~ a) => a -> DefaultEditor a

class HandleEvent1 f where
  handleKey1 :: (Mods -> Key -> a -> IO (Maybe a)) -> Mods -> Key -> f a -> IO (Maybe (f a))

instance (HandleEvent1 f) => HandleEvent (Fix f) where
  handleKey mods key = fmap (fmap Fix) . handleKey1 handleKey mods key . unFix
