{-# LANGUAGE DataKinds #-}

module Pokeapi
  ( pokemon,
    ability,
    getAbilities,
    getHiddenAbility,
    module Pokeapi.Types,
  )
where

import Control.Exception (catch, throwIO, try)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (eitherDecode)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import Pokeapi.Types

-- * Higher-level functions

-- | Get the (correctly-hyphenated / capitalised) name of an ability in a given
-- language. Use "en" for English.
-- TODO: Use a proper ADT for language
getAbilityNameInLang :: Text -> Text -> IO (Maybe Text)
getAbilityNameInLang abty lang = do
  abty' <- ability abty
  let names = abilityNames abty'
  case filter (\n -> name (nameLanguage n) == lang) names of
    [] -> pure Nothing
    (n : _) -> pure $ Just (nameName n)

-- | Get a list of all possible abilities of a Pokemon. The Bool indicates
-- whether the ability is a hidden ability (True corresponds to a HA).
getAbilities :: Text -> IO [(Text, Bool)]
getAbilities p = do
  let getEngNameAndHidden :: PokemonAbility -> IO (Maybe (Text, Bool))
      getEngNameAndHidden abty = do
        let abilityName = name (paAbility abty)
        englishName <- getAbilityNameInLang abilityName "en"
        let isHidden = paIsHidden abty
        case englishName of
          Nothing -> throwIO $ PokeException $ "No English name found for ability '" <> abilityName <> "'. (This should not happen.)"
          Just n -> do
            pure $ Just (n, isHidden)
  pkmn <- pokemon p
  let abilities = pokemonAbilities pkmn
  case abilities of
    [] -> throwIO $ PokeException $ "No abilities found for Pokemon '" <> p <> "'. (This should not happen.)"
    _ -> do
      mapM
        ( \a -> do
            let abilityName = name (paAbility a)
            englishName <- getAbilityNameInLang abilityName "en"
            case englishName of
              Nothing -> throwIO $ PokeException $ "No English name found for ability '" <> name (paAbility a) <> "'. (This should not happen.)"
              Just n -> pure (n, paIsHidden a)
        )
        abilities

-- | Get the hidden ability of a Pokemon, if it exists.
getHiddenAbility :: Text -> IO (Maybe Text)
getHiddenAbility p = do
  pkmn <- pokemon p
  case filter paIsHidden (pokemonAbilities pkmn) of
    [] -> pure Nothing
    (x : _) -> do
      let abilityName = name (paAbility x)
      englishName <- getAbilityNameInLang abilityName "en"
      case englishName of
        Nothing -> throwIO $ PokeException $ "No English name found for ability '" <> abilityName <> "'."
        Just n -> pure $ Just n

-- * Actual endpoints

--
-- TODO: Caching. How do we do this?

api :: Url 'Https
api = https "pokeapi.co" /: "api" /: "v2"

ua :: Option 'Https
ua = header "user-agent" "pokeapi-haskell v0.1.0.0 github:penelopeysm/pokeapi"

pokemon :: Text -> IO Pokemon
pokemon name = do
  body <- runReq' $ do
    let uri = api /: "pokemon" /: T.toLower name
    resp <- req GET uri NoReqBody lbsResponse ua
    pure (responseBody resp)
  case eitherDecode body of
    Left err -> error err
    Right pokemon -> return pokemon

ability :: Text -> IO Ability
ability name = do
  body <- runReq' $ do
    let uri = api /: "ability" /: T.toLower name
    resp <- req GET uri NoReqBody lbsResponse ua
    pure (responseBody resp)
  case eitherDecode body of
    Left err -> error err
    Right ability -> return ability

-- | Run a Req action, catching any HTTP exceptions and rethrowing them as
-- 'PokeHttpException's.
runReq' :: (MonadIO m) => Req a -> m a
runReq' req =
  liftIO $
    catch
      (runReq defaultHttpConfig req)
      (\(e :: HttpException) -> throwIO (PokeHttpException e))
