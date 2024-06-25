{-# LANGUAGE TemplateHaskell #-}

module Stred.LineEditor where

import Control.Monad (unless)
import Control.Monad.State.Strict (get)
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Graphics.Vty.Image ((<|>))
import Optics
import Optics.State.Operators ((%=), (.=))
import Stred.Widget

data LineEditor = LineEditor
  { contents :: Text
  , cursorPos :: Int
  }

makeFieldLabelsNoPrefix ''LineEditor

instance Widget LineEditor where
  handleEvent (Vty.EvKey (Vty.KChar c) []) = do
    LineEditor{contents, cursorPos} <- get
    #contents .= Text.take cursorPos contents <> Text.singleton c <> Text.drop cursorPos contents
    #cursorPos %= (+ 1)
    pure True
  handleEvent (Vty.EvKey Vty.KPause []) = do
    LineEditor{contents, cursorPos} <- get
    unless (cursorPos == Text.length contents) do
      #contents .= Text.take cursorPos contents <> Text.drop (cursorPos + 1) contents
    pure True
  handleEvent (Vty.EvKey Vty.KDel []) = do
    LineEditor{contents, cursorPos} <- get
    unless (cursorPos == Text.length contents) do
      #contents .= Text.take cursorPos contents <> Text.drop (cursorPos + 1) contents
    pure True
  handleEvent (Vty.EvKey Vty.KBS []) = do
    LineEditor{contents, cursorPos} <- get
    unless (cursorPos == 0) do
      #contents .= Text.take (cursorPos - 1) contents <> Text.drop cursorPos contents
      #cursorPos %= subtract 1
    pure True
  handleEvent (Vty.EvKey Vty.KLeft []) = do
    LineEditor{cursorPos} <- get
    unless (cursorPos == 0) do
      #cursorPos %= subtract 1
    pure True
  handleEvent (Vty.EvKey Vty.KRight []) = do
    LineEditor{contents, cursorPos} <- get
    unless (cursorPos == Text.length contents) do
      #cursorPos %= (+ 1)
    pure True
  handleEvent (Vty.EvKey Vty.KHome []) = do
    #cursorPos .= 0
    pure True
  handleEvent (Vty.EvKey Vty.KEnd []) = do
    LineEditor{contents} <- get
    #cursorPos .= Text.length contents
    pure True
  handleEvent _ = pure False

  render LineEditor{contents, cursorPos}
    | cursorPos < Text.length contents =
        Vty.text' Vty.defAttr (Text.take cursorPos contents)
          <|> Vty.text' cursorAttr (Text.take 1 (Text.drop cursorPos contents))
          <|> Vty.text' Vty.defAttr (Text.drop (cursorPos + 1) contents)
    | otherwise =
        Vty.text' Vty.defAttr (Text.take cursorPos contents)
          <|> Vty.text' cursorAttr " "
    where
      cursorAttr = Vty.withStyle Vty.defAttr Vty.reverseVideo
