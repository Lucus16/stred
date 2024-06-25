{-# LANGUAGE TemplateHaskell #-}

module Stred where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (evalStateT, get)
import Graphics.Vty (Vty)
import Graphics.Vty qualified as Vty
import Optics
import Stred.LineEditor
import Stred.Widget

newtype App a = App {top :: a}

makeFieldLabelsNoPrefix ''App

instance (Widget a) => Widget (App a) where
  handleEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) = pure False
  handleEvent ev = do
    void $ zoom #top $ handleEvent ev
    pure True

  render (App w) = render w

initialApp :: App LineEditor
initialApp =
  App $
    LineEditor
      { contents = ""
      , cursorPos = 0
      }

main :: Vty -> IO ()
main vty = evalStateT step initialApp
  where
    step = do
      get >>= liftIO . Vty.update vty . Vty.picForImage . render
      handled <- liftIO (Vty.nextEvent vty) >>= handleEvent
      when handled step
