{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Pokeapi

main :: IO ()
main = do
  resourceList <- gets @Pokemon (Just 50) Nothing
  forM_ resourceList $ \res -> do
    pkmn <- resolve res
    T.putStrLn $ pokemonName pkmn
