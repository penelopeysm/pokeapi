{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Pokeapi where

import Control.Exception (Exception)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Req

data PokeException
  = PokeException Text
  | PokeHttpException HttpException
  | PokeJsonException Text
  deriving (Show)

instance Exception PokeException

api :: Url 'Https
api = https "pokeapi.co" /: "api" /: "v2"

ua :: Option 'Https
ua = header "user-agent" "pokeapi-haskell v0.1.0.0 github:penelopeysm/pokeapi"

lower1 :: String -> String
lower1 [] = []
lower1 (x : xs) = toLower x : xs

flm :: Int -> String -> String
flm n = camelTo2 '_' . lower1 . drop n

-- * Pokemon

pokemon :: Text -> IO Pokemon
pokemon name = do
  body <- runReq defaultHttpConfig $ do
    let uri = api /: "pokemon" /: T.toLower name
    resp <- req GET uri NoReqBody lbsResponse ua
    pure (responseBody resp)
  case eitherDecode body of
    Left err -> error err
    Right pokemon -> return pokemon

data Pokemon = Pokemon
  { pokemonId :: Int,
    pokemonName :: Text,
    pokemonBaseExperience :: Int,
    pokemonHeight :: Int,
    pokemonIsDefault :: Bool,
    pokemonOrder :: Int,
    pokemonWeight :: Int,
    pokemonAbilities :: [PokemonAbility],
    pokemonForms :: [NamedAPIResource ProxyPokemonForm],
    pokemonGameIndices :: [VersionGameIndex],
    pokemonHeldItems :: [PokemonHeldItem],
    pokemonLocationAreaEncounters :: Text,
    pokemonMoves :: [PokemonMove],
    pokemonPastTypes :: [PokemonTypePast],
    pokemonSprites :: PokemonSprites,
    pokemonSpecies :: NamedAPIResource ProxyPokemonSpecies,
    pokemonStats :: [PokemonStat],
    pokemonTypes :: [PokemonType]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Pokemon where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 7}

data PokemonAbility = PokemonAbility
  { paIsHidden :: Bool,
    paSlot :: Int,
    paAbility :: NamedAPIResource ProxyAbility
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonAbility where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data PokemonType = PokemonType
  { ptSlot :: Int,
    ptType :: NamedAPIResource ProxyType
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonType where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data PokemonFormType = PokemonFormType
  { pftSlot :: Int,
    pftType :: NamedAPIResource ProxyType
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonFormType where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data PokemonTypePast = PokemonTypePast
  { ptpGeneration :: NamedAPIResource ProxyGeneration,
    ptpTypes :: [PokemonType]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonTypePast where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data PokemonHeldItem = PokemonHeldItem
  { phiItem :: NamedAPIResource ProxyItem,
    phiVersionDetails :: [PokemonHeldItemVersion]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonHeldItem where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data PokemonHeldItemVersion = PokemonHeldItemVersion
  { phivVersion :: NamedAPIResource ProxyVersion,
    phivRarity :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonHeldItemVersion where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

data PokemonMove = PokemonMove
  { pmMove :: NamedAPIResource ProxyMove,
    pmVersionGroupDetails :: [PokemonMoveVersion]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonMove where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data PokemonMoveVersion = PokemonMoveVersion
  { pmvMoveLearnMethod :: NamedAPIResource ProxyMoveLearnMethod,
    pmvVersionGroup :: NamedAPIResource ProxyVersionGroup,
    pmvLevelLearnedAt :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonMoveVersion where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data PokemonStat = PokemonStat
  { psStat :: NamedAPIResource ProxyStat,
    psEffort :: Int,
    psBaseStat :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonStat where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data PokemonSprites = PokemonSprites
  { psFrontDefault :: Text,
    psFrontShiny :: Text,
    psFrontFemale :: Maybe Text,
    psFrontShinyFemale :: Maybe Text,
    psBackDefault :: Text,
    psBackShiny :: Text,
    psBackFemale :: Maybe Text,
    psBackShinyFemale :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonSprites where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data VersionGameIndex = VersionGameIndex
  { vgiGameIndex :: Int,
    vgiVersion :: NamedAPIResource ProxyVersion
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON VersionGameIndex where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- * Abilities

ability :: Text -> IO Ability
ability name = do
  body <- runReq defaultHttpConfig $ do
    let uri = api /: "ability" /: T.toLower name
    resp <- req GET uri NoReqBody lbsResponse ua
    pure (responseBody resp)
  case eitherDecode body of
    Left err -> error err
    Right ability -> return ability

data Ability = Ability
  { abilityId :: Int,
    abilityName :: Text,
    abilityIsMainSeries :: Bool,
    abilityGeneration :: NamedAPIResource ProxyGeneration,
    abilityNames :: [Name],
    abilityEffectEntries :: [VerboseEffect],
    abilityEffectChanges :: [AbilityEffectChange],
    abilityFlavorTextEntries :: [AbilityFlavorText],
    abilityPokemon :: [AbilityPokemon]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Ability where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 7}

data AbilityEffectChange = AbilityEffectChange
  { aecEffectEntries :: [Effect],
    aecVersionGroup :: NamedAPIResource ProxyVersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AbilityEffectChange where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data Name = Name
  { nameName :: Text,
    nameLanguage :: NamedAPIResource ProxyLanguage
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Name where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

data Effect = Effect
  { effectEffect :: Text,
    effectLanguage :: NamedAPIResource ProxyLanguage
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Effect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 6}

data VerboseEffect = VerboseEffect
  { veEffect :: Text,
    veShortEffect :: Text,
    veLanguage :: NamedAPIResource ProxyLanguage
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON VerboseEffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data AbilityFlavorText = AbilityFlavorText
  { aftFlavorText :: Text,
    aftLanguage :: NamedAPIResource ProxyLanguage,
    aftVersionGroup :: NamedAPIResource ProxyVersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AbilityFlavorText where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data AbilityPokemon = AbilityPokemon
  { apIsHidden :: Bool,
    apSlot :: Int,
    apPokemon :: NamedAPIResource ProxyPokemon
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AbilityPokemon where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data NamedAPIResource a = NamedAPIResource
  { name :: Text,
    url :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (NamedAPIResource a) where
  parseJSON = genericParseJSON defaultOptions

-- * Proxy types for NamedAPIResources

data ProxyPokemon

data ProxyLanguage

data ProxyVersionGroup

data ProxyGeneration

data ProxyAbility

data ProxyType

data ProxyItem

data ProxyVersion

data ProxyMove

data ProxyMoveLearnMethod

data ProxyStat

data ProxyPokemonForm

data ProxyPokemonSpecies
