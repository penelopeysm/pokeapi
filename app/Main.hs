module Main where

import Control.Monad ((>=>))
import Data.Text (Text)
import Pokeapi

main :: IO ()
main = do
  mapM_ (getHiddenAbility >=> print) ["Togekiss"]
  mapM_ (getAbilities >=> print) ["Togekiss"]
