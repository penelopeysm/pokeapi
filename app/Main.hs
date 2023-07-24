module Main where

import Control.Monad ((>=>))
import Data.Text (Text)
import Pokeapi

getHA :: Text -> IO (Maybe Text)
getHA p = do
  pkmn <- pokemon p
  case filter paIsHidden (pokemonAbilities pkmn) of
    [] -> pure Nothing
    (x : _) -> do
      abty <- ability $ name (paAbility x)
      let englishName = filter (\n -> name (nameLanguage n) == "en") (abilityNames abty)
      pure $ case englishName of
        [] -> Nothing
        (n : _) -> Just $ nameName n

main :: IO ()
main = do
  mapM_ (getHA >=> print) ["Tatsugiri"]
