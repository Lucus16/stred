{-# LANGUAGE TemplateHaskell #-}

module Stred.EnumEditor where

import Control.Monad (unless)
import Control.Monad.State.Strict (get)
import Graphics.Vty qualified as Vty
import Optics
import Optics.State.Operators ((%=), (.=))
import Stred.Widget

data EnumEditor a = EnumEditor
  { contents :: a
  , cursor :: a
  }

makeFieldLabelsNoPrefix ''EnumEditor

instance (Bounded a, Enum a, Ord a, Show a) => Widget (EnumEditor a) where
  handleEvent (Vty.EvKey (Vty.KChar ' ') []) = do
    EnumEditor{cursor} <- get
    #contents .= cursor
    pure True
  handleEvent (Vty.EvKey Vty.KLeft []) = do
    EnumEditor{cursor} <- get
    unless (cursor == minBound) do
      #cursor %= pred
    pure True
  handleEvent (Vty.EvKey Vty.KRight []) = do
    EnumEditor{cursor} <- get
    unless (cursor == maxBound) do
      #cursor %= succ
    pure True
  handleEvent (Vty.EvKey Vty.KHome []) = do
    #cursor .= minBound
    pure True
  handleEvent (Vty.EvKey Vty.KEnd []) = do
    #cursor .= maxBound
    pure True
  handleEvent _ = pure False

  render EnumEditor{contents, cursor} =
    Vty.horizCat $ map renderItem [minBound .. maxBound]
    where
      pad x = " " <> x <> " "

      renderItem :: a -> Vty.Image
      renderItem x = Vty.string (contentsAttr (cursorAttr Vty.defAttr)) (pad (bullet <> show x))
        where
          bullet
            | x == contents = "◉ "
            | otherwise = "○ "

          contentsAttr
            | x == contents = flip Vty.withStyle Vty.bold
            | otherwise = id

          cursorAttr
            | x == cursor = flip Vty.withStyle Vty.reverseVideo
            | otherwise = id
