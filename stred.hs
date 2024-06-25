module Main where

import Control.Exception.Safe (bracket)
import Graphics.Vty (Vty (shutdown))
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Platform.Unix (mkVty)
import Stred qualified

main :: IO ()
main = bracket (mkVty defaultConfig) shutdown Stred.main
