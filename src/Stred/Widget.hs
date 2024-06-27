module Stred.Widget where

import Graphics.Vty qualified as Vty
import Graphics.Vty.Input.Events (Key, Modifier)

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
    go mods (Vty.MCtrl : more) = go mods{ctrl = True} more
    go mods (Vty.MMeta : more) = go mods{alt = True} more
    go mods (Vty.MAlt : more) = go mods{alt = True} more
    go mods (Vty.MShift : more) = go mods{shift = True} more

class HandleEvent a where
  -- | Nothing means the event was not handled. If the toplevel app does not
  -- handle an event, the system terminates.
  handleKey :: Mods -> Key -> a -> IO (Maybe a)

class Render a where
  render :: Bool -> a -> Vty.Image

class (HandleEvent a, Render a) => Editor a where
  type Contents a
  newEditor :: a
  editorFromContents :: Contents a -> a
  contentsFromEditor :: a -> Maybe (Contents a)
