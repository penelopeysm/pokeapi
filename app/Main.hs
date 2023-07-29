module Main where

import Control.Monad ((>=>))
import Data.Text (Text)
import Pokeapi

main :: IO ()
main = do
  mapM_ (haSpecies >=> print) ["Togekiss", "Finizen", "Iron-Bundle", "Toxtricity"]
