{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Pokeapi
-- Description : Complete interface
-- Copyright   : (c) Penelope Yong 2023
-- License     : MIT
-- Maintainer  : penelopeysm@gmail.com
-- Stability   : Stable
--
-- This module provides all the functions to directly interact with the [v2
-- REST API of PokeAPI](https://pokeapi.co/docs/v2). The GraphQL API is not
-- supported.
--
-- Generally, you should only need to use the following functions. Example usage
-- is provided in the documentation for each.
--
--  * 'get': queries an endpoint by ID or name
--  * 'gets': queries an endpoint to obtain a list of results
--  * 'resolve': lets you get an actual value of @a@ from either a
--  @'NamedAPIResource' a@ or @'APIResource' a@
--  * the various record field selectors (which directly map to the PokeAPI
--  schema; see <https://pokeapi.co/docs/v2> for documentation)
module Pokeapi
  ( -- * Exceptions
    PokeException (..),

    -- * Core typeclasses
    PokeApi (get),
    PokeApiListable (gets),
    Resolvable (resolve, url),
    ResolvableList,

    -- * Berries

    -- ** Berries
    Berry (..),
    BerryFlavorMap (..),

    -- ** Berry Firmnesses
    BerryFirmness (..),

    -- ** Berry Flavors
    BerryFlavor (..),
    FlavorBerryMap (..),

    -- * Contests

    -- ** Contest Types
    ContestType (..),
    ContestName (..),

    -- ** Contest Effects
    ContestEffect (..),

    -- ** Super Contest Effects
    SuperContestEffect (..),

    -- * Encounters

    -- ** Encounter Methods
    EncounterMethod (..),

    -- ** Encounter Conditions
    EncounterCondition (..),

    -- ** Encounter Condition Values
    EncounterConditionValue (..),

    -- * Evolution

    -- ** Evolution Chains
    EvolutionChain (..),
    ChainLink (..),
    EvolutionDetail (..),

    -- ** Evolution Triggers
    EvolutionTrigger (..),

    -- * Games

    -- ** Generations
    Generation (..),

    -- ** Pokedexes
    Pokedex (..),
    PokemonEntry (..),

    -- ** Versions
    Version (..),

    -- ** Version Groups
    VersionGroup (..),

    -- * Items

    -- ** Items
    Item (..),
    ItemSprites (..),
    ItemHolderPokemon (..),
    ItemHolderPokemonVersionDetail (..),

    -- ** Item Attributes
    ItemAttribute (..),

    -- ** Item Categories
    ItemCategory (..),

    -- ** Item Fling Effects
    ItemFlingEffect (..),

    -- ** Item Pockets
    ItemPocket (..),

    -- * Locations

    -- ** Locations
    Location (..),

    -- ** Location Areas
    LocationArea (..),
    EncounterMethodRate (..),
    EncounterVersionDetails (..),
    PokemonEncounter (..),

    -- ** Pal Park Areas
    PalParkArea (..),
    PalParkEncounterSpecies (..),

    -- ** Regions
    Region (..),

    -- * Machines
    Machine (..),

    -- * Moves

    -- ** Moves
    Move (..),
    ContestComboSets (..),
    ContestComboDetail (..),
    MoveFlavorText (..),
    MoveMetaData (..),
    MoveStatChange (..),
    PastMoveStatValues (..),

    -- ** Move Ailments
    MoveAilment (..),

    -- ** Move Battle Styles
    MoveBattleStyle (..),

    -- ** Move Categories
    MoveCategory (..),

    -- ** Move Damage Classes
    MoveDamageClass (..),

    -- ** Move Learn Methods
    MoveLearnMethod (..),

    -- ** Move Targets
    MoveTarget (..),

    -- * Pokemon

    -- ** Abilities
    Ability (..),
    AbilityEffectChange (..),
    AbilityFlavorText (..),
    AbilityPokemon (..),

    -- ** Characteristics
    Characteristic (..),

    -- ** Egg Groups
    EggGroup (..),

    -- ** Genders
    Gender (..),
    PokemonSpeciesGender (..),

    -- ** Growth Rates
    GrowthRate (..),
    GrowthRateExperienceLevel (..),

    -- ** Natures
    Nature (..),
    NatureStatChange (..),
    MoveBattleStylePreference (..),

    -- ** Pokeathlon Stats
    PokeathlonStat (..),
    NaturePokeathlonStatAffectSets (..),
    NaturePokeathlonStatAffect (..),

    -- ** Pokemon
    Pokemon (..),
    PokemonAbility (..),
    PokemonType (..),
    PokemonFormType (..),
    PokemonTypePast (..),
    PokemonHeldItem (..),
    PokemonHeldItemVersion (..),
    PokemonMove (..),
    PokemonMoveVersion (..),
    PokemonStat (..),
    PokemonSprites (..),

    -- ** Pokemon Location Areas
    PokemonLocationArea (..),
    LocationAreaEncounter (..),

    -- ** Pokemon Colors
    PokemonColor (..),

    -- ** Pokemon Forms
    PokemonForm (..),
    PokemonFormSprites (..),

    -- ** Pokemon Habitats
    PokemonHabitat (..),

    -- ** Pokemon Shapes
    PokemonShape (..),
    AwesomeName (..),

    -- ** Pokemon Species
    PokemonSpecies (..),
    Genus (..),
    PokemonSpeciesDexEntry (..),
    PalParkEncounterArea (..),
    PokemonSpeciesVariety (..),

    -- ** Stats
    Stat (..),
    MoveStatAffectSets (..),
    MoveStatAffect (..),
    NatureStatAffectSets (..),

    -- ** Types
    Type (..),
    TypePokemon (..),
    TypeRelations (..),
    TypeRelationsPast (..),

    -- * Utility

    -- ** Languages
    Language (..),

    -- ** API Resources
    NamedAPIResource (..),
    NamedAPIResourceList (..),
    APIResource (..),
    APIResourceList (..),

    -- ** Common Models
    Description (..),
    Effect (..),
    Encounter (..),
    FlavorText (..),
    GenerationGameIndex (..),
    MachineVersionDetail (..),
    Name (..),
    VerboseEffect (..),
    VersionEncounterDetail (..),
    VersionGameIndex (..),
    VersionGroupFlavorText (..),
  )
where

import Control.Exception (Exception (..), catch, throwIO, try)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (Void)
import GHC.Generics
import Network.HTTP.Req
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.URI (mkURI)

-- | API base URL
apiv2 :: Url 'Https
apiv2 = https "pokeapi.co" /: "api" /: "v2"

-- | User-agent header
ua :: Option 'Https
ua = header "user-agent" "pokeapi-haskell v0.1.0.0 github:penelopeysm/pokeapi"

-- * Exceptions

-- | Exceptions that can be thrown by this library.
data PokeException
  = -- | Exceptions arising from the API itself. You should never see these.
    PokeException Text
  | -- | Exceptions arising from HTTP requests. You will get this if you pass an invalid parameter, for example.
    PokeHttpException HttpException
  | -- | Exceptions arising from JSON decoding. If you see one of these, it is a bug in this library.
    PokeJsonException Text
  deriving (Show)

instance Exception PokeException

-- * Helper functions

lower1 :: String -> String
lower1 [] = []
lower1 (x : xs) = toLower x : xs

flm :: Int -> String -> String
flm n = camelTo2 '_' . lower1 . drop n

-- | Obtain PokeAPI data.
--
-- First, if POKEAPI_NO_CACHE is not set, queries the cache to see whether a
-- cached response is present. If there is one, simply decodes and returns that.
--
-- If there is no cached value, or if decoding the file throws an error,
-- executes a HTTP request to PokeAPI, catching any HTTP exceptions and
-- rethrowing them as 'PokeHttpException's. The response body is then decoded as
-- JSON, and any Aeson exceptions are rethrown as 'PokeJsonException's. Finally,
-- the JSON is cached (again if POKEAPI_NO_CACHE is not set) and returned.
request :: (FromJSON a) => Url 'Https -> Option 'Https -> IO a
request url headers = do
  -- Check whether caching is disabled
  noCache <- isJust <$> lookupEnv "POKEAPI_NO_CACHE"
  -- Generate a file name (which doesn't have too many special characters)
  let cacheFile =
        T.unpack $
          (T.replace "/" "_" . T.replace "https://pokeapi.co/api/v2/" "" . renderUrl $ url)
            <> T.concat (map (\(k, v) -> "__" <> k <> maybe "" ("_" <>) v) (queryParamToList headers))
  maybeCacheDir <- lookupEnv "POKEAPI_CACHE_LOCATION"
  cacheDir <- case maybeCacheDir of
    Nothing -> do
      homeDir <- getHomeDirectory
      pure $ homeDir </> ".pokeapi_cache"
    Just cd -> pure cd
  let cacheFilePath = cacheDir </> cacheFile

  -- Check cache
  maybeCachedValue :: Maybe a <-
    if noCache
      then pure Nothing
      else do
        fileContents <- catch (Just <$> LBS.readFile cacheFilePath) (\(_ :: IOError) -> pure Nothing)
        pure $ case fileContents of
          Nothing -> Nothing -- File doesn't exist or is otherwise unreadable
          Just cts -> case eitherDecode cts of
            Left err -> Nothing -- File doesn't contain correct JSON
            Right res -> Just res
  case maybeCachedValue of
    Just cachedValue -> pure cachedValue
    Nothing -> do
      -- Run HTTP request
      eitherBody <- try $ runReq defaultHttpConfig $ do
        resp <- req GET url NoReqBody lbsResponse (ua <> headers)
        pure (responseBody resp)
      case eitherBody of
        Left e -> throwIO (PokeHttpException e)
        Right body -> do
          -- Cache
          unless noCache $ do
            createDirectoryIfMissing True cacheDir
            LBS.writeFile cacheFilePath body
          -- Decode
          case eitherDecode body of
            Left err -> throwIO $ PokeJsonException $ T.pack err <> " when accessing URL: " <> renderUrl url
            Right res -> pure res

-- | A typeclass for resources in PokeAPI.
class (FromJSON a) => PokeApi a where
  -- | Retrieve an instance of the resource from the URL.
  getFromUrl' :: Text -> IO a
  getFromUrl' u = do
    mbUrl <- useHttpsURI <$> mkURI u
    case mbUrl of
      Nothing -> throwIO $ PokeException $ "Invalid HTTPS URL: " <> u
      Just (url, _) -> getFromUrl url

  -- | Retrieve an instance of the resource from the URL.
  getFromUrl :: Url 'Https -> IO a
  getFromUrl url = request url mempty

  -- | Retrieve an instance of the resource from the identifier (either a
  -- numeric ID or a name). This, along with 'gets', is the primary function for
  -- querying the API.
  --
  -- Each endpoint in PokeAPI is associated with a specific type, and can be
  -- accessed via this function. For example, to access the @\/pokemon\/togekiss@
  -- endpoint, you would do:
  --
  -- @
  -- togekiss <- 'get' "togekiss"
  -- -- do something with togekiss
  -- @
  --
  -- Generally, whatever you then do with @togekiss@ (e.g. accessing its fields)
  -- will allow GHC to infer the polymorphic type @a@. If you need to specify
  -- the type, you can either use a type annotation:
  --
  -- @
  -- (togekiss :: 'Pokemon') <- 'get' "togekiss"
  -- @
  --
  -- or use the [@TypeApplications@](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_applications.html)
  -- extension:
  --
  -- @
  -- {-\# LANGUAGE TypeApplications \#-}
  -- togekiss <- 'get' \@'Pokemon' "togekiss"
  -- @
  get ::
    -- | The identifier of the resource, which can either be a numeric ID or a
    -- name.
    Text ->
    IO a

-- | A typeclass for resources in PokeAPI which can be queried in a list-like
-- fashion.
--
-- Most endpoints in PokeAPI can be queried without passing an explicit ID or
-- name; see <https://pokeapi.co/docs/v2#resource-listspagination-section>. (The
-- only exception is the @\/pokemon\/{id or name}\/encounters@ endpoint, which
-- corresponds to the 'PokemonLocationArea' type.)
--
-- Instead of a single value of @a@, this would then return a list of @t a@,
-- where (depending on the endpoint, i.e., the exact type @a@) @t@ is either
-- 'NamedAPIResource' or 'APIResource'. This relationship is expressed as a
-- [functional
-- dependency](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/functional_dependencies.html)
-- in the typeclass. (The @tlist@ type is a technical detail to do with parsing
-- the JSON returned by PokeAPI; you don't need to worry about it.)
--
-- The resulting @{Named}APIResource a@ values are not fully populated; they
-- only contain a URL pointing to the resource @a@, and optionally, a name. To
-- retrieve the full resource, you can use the 'resolve' function; both
-- @APIResource@ and @NamedAPIResource@ are instances of the 'Resolvable'
-- typeclass, which enables this behaviour. Note that each call to 'resolve' is
-- one API call, so you should be careful about only requesting the resources
-- you need, and implement rate limiting as you see fit.
--
-- See 'gets' for a specific usage example.
class
  (PokeApi a, FromJSON (t a), FromJSON (tlist a), ResolvableList tlist t) =>
  PokeApiListable a tlist t
    | a -> tlist t
  where
  -- | Retrieve a list of resources from the URL.
  getsFromUrl :: Maybe Int -> Maybe Int -> Url 'Https -> IO [t a]
  getsFromUrl lim off url = do
    let limitHeader = maybe mempty ("limit" =:) lim
        offsetHeader = maybe mempty ("offset" =:) off
    -- Need type annotation here to specify that list has the specific tlist
    -- given in the instance definition
    (list :: tlist a) <- request url (limitHeader <> offsetHeader)
    pure $ results list

  -- | Retrieve a list of resources. For example, to get the first 50 Pokemon:
  --
  -- @
  -- {-\# LANGUAGE TypeApplications \#-}
  -- pkmns <- 'gets' @'Pokemon' (Just 50) Nothing
  -- @
  -- (See 'get' for details on type annotations.)
  --
  -- Here, @pkmns@ has the type @['NamedAPIResource' 'Pokemon']@. If you want a
  -- full list of @['Pokemon']@, you need to then use 'resolve':
  --
  -- @
  -- {-\# LANGUAGE TypeApplications \#-}
  -- pkmns <- 'gets' @'Pokemon' (Just 50) Nothing
  -- pkmns' <- mapM 'resolve' pkmns
  -- @
  gets ::
    -- | Limit (i.e. how many resources to return). If @Nothing@, defaults to 20.
    --
    -- To obtain a list of all resources, you can put a very large number here (e.g. @Just 100000@).
    Maybe Int ->
    -- | Offset (i.e. the index of the first resource to return). If @Nothing@, defaults to 0.
    Maybe Int ->
    -- | Returned resources (as a list of either 'NamedAPIResource's or 'APIResource's).
    IO [t a]

-- | A class for resources in PokeAPI (either 'APIResource's or
-- 'NamedAPIResource's) which are not fully resolved, but can be.
class Resolvable t where
  -- | Get the URL pointing to a resource.
  url :: t a -> Text

  -- | PokeAPI often does not return completely filled-in types as part of the
  -- data, choosing instead to return @NamedAPIResource a@ or @APIResource a@.
  --
  -- This function lets gets you the actual @a@'s. For example, if you want to
  -- get Togekiss's (first) ability, you could do:
  --
  -- @
  -- togekiss <- 'get' "togekiss"
  -- let firstAbility = 'paAbility' . head . 'pokemonAbilities' $ togekiss
  -- @
  --
  -- However, here, @firstAbility@ doesn't actually contain much information
  -- about the actual ability:
  --
  -- @
  -- >>> firstAbility
  -- 'NamedAPIResource' {'name' = "hustle", 'narUrl' = "https:\/\/pokeapi.co\/api\/v2\/ability\/55\/"}
  -- @
  --
  -- To get the full ability, you can use 'resolve':
  --
  -- @
  -- hustle <- 'resolve' firstAbility
  -- @
  --
  -- Now, @hustle@ is of type 'Ability', and contains all the information you
  -- might ever want about Hustle (probably).
  resolve :: (PokeApiListable a tlist t) => t a -> IO a
  resolve = getFromUrl' . url

-- | A class for lists of resources in PokeAPI. You should not have to interact
-- with this class.
class (Resolvable t) => ResolvableList tlist t | tlist -> t where
  -- | Extract the actual list of @t a@'s from its wrapper.
  results :: (FromJSON a) => tlist a -> [t a]

-- * Berries

-- ** Berries

-- | <https://pokeapi.co/docs/v2#berries>
data Berry = Berry
  { berryId :: Int,
    berryName :: Text,
    berryGrowthTime :: Int,
    berryMaxHarvest :: Int,
    berryNaturalGiftPower :: Int,
    berrySize :: Int,
    berrySmoothness :: Int,
    berrySoilDryness :: Int,
    berryFirmness :: NamedAPIResource BerryFirmness,
    berryFlavors :: [BerryFlavorMap],
    berryItem :: NamedAPIResource Item,
    berryNaturalGiftType :: NamedAPIResource Type
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Berry where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 5}

-- | [@\/berry\/{id or name}@](https://pokeapi.co/docs/v2#berries)
instance PokeApi Berry where
  get iden = getFromUrl $ apiv2 /: "berry" /: T.toLower iden

-- | [@\/berry@](https://pokeapi.co/docs/v2#berries)
instance PokeApiListable Berry NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "berry"

-- | <https://pokeapi.co/docs/v2#berryflavormap>
data BerryFlavorMap = BerryFlavorMap
  { bfmPotency :: Int,
    bfmFlavor :: NamedAPIResource BerryFlavor
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON BerryFlavorMap where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- ** Berry Firmnesses

-- | <https://pokeapi.co/docs/v2#berry-firmnesses>
data BerryFirmness = BerryFirmness
  { bfmId :: Int,
    bfmName :: Text,
    bfmBerries :: [NamedAPIResource Berry],
    bfmNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON BerryFirmness where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | [@\/berry-firmness\/{id or name}@](https://pokeapi.co/docs/v2#berry-firmnesses)
instance PokeApi BerryFirmness where
  get iden = getFromUrl $ apiv2 /: "berry-firmness" /: T.toLower iden

-- | [@\/berry-firmness@](https://pokeapi.co/docs/v2#berry-firmnesses)
instance PokeApiListable BerryFirmness NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "berry-firmness"

-- ** Berry Flavors

-- | <https://pokeapi.co/docs/v2#berry-flavors>
data BerryFlavor = BerryFlavor
  { bfId :: Int,
    bfName :: Text,
    bfBerries :: [FlavorBerryMap],
    bfContestType :: NamedAPIResource ContestType,
    bfNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON BerryFlavor where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/berry-flavor\/{id or name}@](https://pokeapi.co/docs/v2#berry-flavors)
instance PokeApi BerryFlavor where
  get iden = getFromUrl $ apiv2 /: "berry-flavor" /: T.toLower iden

-- | [@\/berry-flavor@](https://pokeapi.co/docs/v2#berry-flavors)
instance PokeApiListable BerryFlavor NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "berry-flavor"

-- | <https://pokeapi.co/docs/v2#flavorberrymap>
data FlavorBerryMap = FlavorBerryMap
  { fbmPotency :: Int,
    fbmBerry :: NamedAPIResource Berry
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON FlavorBerryMap where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- * Contests

-- ** Contest Types

-- | <https://pokeapi.co/docs/v2#contest-types>
data ContestType = ContestType
  { ctId :: Int,
    ctName :: Text,
    ctBerryFlavor :: NamedAPIResource BerryFlavor,
    ctNames :: [ContestName]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContestType where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/contest-type\/{id or name}@](https://pokeapi.co/docs/v2#contest-types)
instance PokeApi ContestType where
  get iden = getFromUrl $ apiv2 /: "contest-type" /: T.toLower iden

-- | [@\/contest-type@](https://pokeapi.co/docs/v2#contest-types)
instance PokeApiListable ContestType NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "contest-type"

-- | <https://pokeapi.co/docs/v2#contestname>
data ContestName = ContestName
  { cnName :: Text,
    cnColor :: Text,
    cnLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContestName where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Contest Effects

-- | <https://pokeapi.co/docs/v2#contest-effects>
data ContestEffect = ContestEffect
  { ceId :: Int,
    ceAppeal :: Int,
    ceJam :: Int,
    ceEffectEntries :: [Effect],
    ceFlavorTextEntries :: [FlavorText]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContestEffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/contest-effect\/{id}@](https://pokeapi.co/docs/v2#contest-effects)
instance PokeApi ContestEffect where
  get iden = getFromUrl $ apiv2 /: "contest-effect" /: T.toLower iden

-- | [@\/contest-effect@](https://pokeapi.co/docs/v2#contest-effects)
instance PokeApiListable ContestEffect APIResourceList APIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "contest-effect"

-- ** Super Contest Effects

-- | <https://pokeapi.co/docs/v2#super-contest-effects>
data SuperContestEffect = SuperContestEffect
  { sceId :: Int,
    sceAppeal :: Int,
    sceFlavorTextEntries :: [FlavorText],
    sceMoves :: [NamedAPIResource Move]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SuperContestEffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | [@\/super-contest-effect\/{id}@](https://pokeapi.co/docs/v2#super-contest-effects)
instance PokeApi SuperContestEffect where
  get iden = getFromUrl $ apiv2 /: "super-contest-effect" /: T.toLower iden

-- | [@\/super-contest-effect@](https://pokeapi.co/docs/v2#super-contest-effects)
instance PokeApiListable SuperContestEffect APIResourceList APIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "super-contest-effect"

-- * Encounters

-- ** Encounter Methods

-- | <https://pokeapi.co/docs/v2#encounter-methods>
data EncounterMethod = EncounterMethod
  { emId :: Int,
    emName :: Text,
    emOrder :: Int,
    emNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EncounterMethod where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/encounter-method\/{id or name}@](https://pokeapi.co/docs/v2#encounter-methods)
instance PokeApi EncounterMethod where
  get iden = getFromUrl $ apiv2 /: "encounter-method" /: T.toLower iden

-- | [@\/encounter-method@](https://pokeapi.co/docs/v2#encounter-methods)
instance PokeApiListable EncounterMethod NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "encounter-method"

-- ** Encounter Conditions

-- | <https://pokeapi.co/docs/v2#encounter-conditions>
--
-- Note the @ecd@ prefix for the record fields.
data EncounterCondition = EncounterCondition
  { ecdId :: Int,
    ecdName :: Text,
    ecdNames :: [Name],
    ecdValues :: [NamedAPIResource EncounterConditionValue]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EncounterCondition where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | [@\/encounter-condition\/{id or name}@](https://pokeapi.co/docs/v2#encounter-conditions)
instance PokeApi EncounterCondition where
  get iden = getFromUrl $ apiv2 /: "encounter-condition" /: T.toLower iden

-- | [@\/encounter-condition@](https://pokeapi.co/docs/v2#encounter-conditions)
instance PokeApiListable EncounterCondition NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "encounter-condition"

-- ** Encounter Condition Values

-- | <https://pokeapi.co/docs/v2#encounter-condition-values>
data EncounterConditionValue = EncounterConditionValue
  { ecvId :: Int,
    ecvName :: Text,
    ecvCondition :: NamedAPIResource EncounterCondition,
    ecvNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EncounterConditionValue where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | [@\/encounter-condition-value\/{id or name}@](https://pokeapi.co/docs/v2#encounter-condition-values)
instance PokeApi EncounterConditionValue where
  get iden = getFromUrl $ apiv2 /: "encounter-condition-value" /: T.toLower iden

-- | [@\/encounter-condition-value@](https://pokeapi.co/docs/v2#encounter-condition-values)
instance PokeApiListable EncounterConditionValue NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "encounter-condition-value"

-- * Evolution

-- ** Evolution Chains

-- | <https://pokeapi.co/docs/v2#evolution-chains>
data EvolutionChain = EvolutionChain
  { ecId :: Int,
    ecBabyTriggerItem :: Maybe (NamedAPIResource Item), -- Maybe verified
    ecChain :: ChainLink
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EvolutionChain where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/evolution-chain\/{id}@](https://pokeapi.co/docs/v2#evolution-chains)
instance PokeApi EvolutionChain where
  get iden = getFromUrl $ apiv2 /: "evolution-chain" /: T.toLower iden

-- | [@\/evolution-chain@](https://pokeapi.co/docs/v2#evolution-chains)
instance PokeApiListable EvolutionChain APIResourceList APIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "evolution-chain"

-- | <https://pokeapi.co/docs/v2#chainlink>
data ChainLink = ChainLink
  { clIsBaby :: Bool,
    clSpecies :: NamedAPIResource PokemonSpecies,
    clEvolutionDetails :: [EvolutionDetail],
    clEvolvesTo :: [ChainLink]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ChainLink where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | <https://pokeapi.co/docs/v2#evolutiondetail>
data EvolutionDetail = EvolutionDetail
  { edItem :: Maybe (NamedAPIResource Item), -- Maybe verified
    edTrigger :: NamedAPIResource EvolutionTrigger,
    edGender :: Maybe Int, -- Maybe verified
    edHeldItem :: Maybe (NamedAPIResource Item), -- Maybe verified
    edKnownMove :: Maybe (NamedAPIResource Move), -- Maybe verified
    edKnownMoveType :: Maybe (NamedAPIResource Type), -- Maybe verified
    edLocation :: Maybe (NamedAPIResource Location), -- Maybe verified
    edMinLevel :: Maybe Int,
    edMinHappiness :: Maybe Int, -- Maybe verified
    edMinBeauty :: Maybe Int, -- Maybe verified
    edMinAffection :: Maybe Int, -- Maybe verified
    edNeedsOverworldRain :: Bool,
    edPartySpecies :: Maybe (NamedAPIResource PokemonSpecies), -- Maybe verified
    edPartyType :: Maybe (NamedAPIResource Type), -- Maybe verified
    edRelativePhysicalStats :: Maybe Int, -- Maybe verified
    edTimeOfDay :: Text,
    edTradeSpecies :: Maybe (NamedAPIResource PokemonSpecies), -- Maybe verified
    edTurnUpsideDown :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EvolutionDetail where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Evolution Triggers

-- | <https://pokeapi.co/docs/v2#evolution-triggers>
data EvolutionTrigger = EvolutionTrigger
  { etId :: Int,
    etName :: Text,
    etNames :: [Name],
    etPokemonSpecies :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EvolutionTrigger where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/evolution-trigger\/{id or name}@](https://pokeapi.co/docs/v2#evolution-triggers)
instance PokeApi EvolutionTrigger where
  get iden = getFromUrl $ apiv2 /: "evolution-trigger" /: T.toLower iden

-- | [@\/evolution-trigger@](https://pokeapi.co/docs/v2#evolution-triggers)
instance PokeApiListable EvolutionTrigger NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "evolution-trigger"

-- * Games

-- ** Generations

-- | <https://pokeapi.co/docs/v2#generations>
data Generation = Generation
  { generationId :: Int,
    generationName :: Text,
    generationAbilities :: [NamedAPIResource Ability],
    generationNames :: [Name],
    generationMainRegion :: NamedAPIResource Region,
    generationMoves :: [NamedAPIResource Move],
    generationPokemonSpecies :: [NamedAPIResource PokemonSpecies],
    generationTypes :: [NamedAPIResource Type],
    generationVersionGroups :: [NamedAPIResource VersionGroup]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Generation where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 10}

-- | [@\/generation\/{id or name}@](https://pokeapi.co/docs/v2#generations)
instance PokeApi Generation where
  get iden = getFromUrl $ apiv2 /: "generation" /: T.toLower iden

-- | [@\/generation@](https://pokeapi.co/docs/v2#generations)
instance PokeApiListable Generation NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "generation"

-- ** Pokedexes

-- | <https://pokeapi.co/docs/v2#pokedexes>
data Pokedex = Pokedex
  { pokedexId :: Int,
    pokedexName :: Text,
    pokedexIsMainSeries :: Bool,
    pokedexDescriptions :: [Description],
    pokedexNames :: [Name],
    pokedexPokemonEntries :: [PokemonEntry],
    -- | @Nothing@ for Pokemon Conquest gallery
    pokedexRegion :: Maybe (NamedAPIResource Region),
    pokedexVersionGroups :: [NamedAPIResource VersionGroup]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Pokedex where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 7}

-- | [@\/pokedex\/{id or name}@](https://pokeapi.co/docs/v2#pokedexes)
instance PokeApi Pokedex where
  get iden = getFromUrl $ apiv2 /: "pokedex" /: T.toLower iden

-- | [@\/pokedex@](https://pokeapi.co/docs/v2#pokedexes)
instance PokeApiListable Pokedex NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokedex"

-- | <https://pokeapi.co/docs/v2#pokemonentry>
data PokemonEntry = PokemonEntry
  { peEntryNumber :: Int,
    pePokemonSpecies :: NamedAPIResource PokemonSpecies
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonEntry where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Version

-- | <https://pokeapi.co/docs/v2#version>
data Version = Version
  { versionId :: Int,
    versionName :: Text,
    versionNames :: [Name],
    versionVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Version where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 7}

-- | [@\/version\/{id or name}@](https://pokeapi.co/docs/v2#version)
instance PokeApi Version where
  get iden = getFromUrl $ apiv2 /: "version" /: T.toLower iden

-- | [@\/version@](https://pokeapi.co/docs/v2#version)
instance PokeApiListable Version NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "version"

-- ** Version Groups

-- | <https://pokeapi.co/docs/v2#version-groups>
data VersionGroup = VersionGroup
  { vgId :: Int,
    vgName :: Text,
    vgOrder :: Int,
    vgGeneration :: NamedAPIResource Generation,
    vgMoveLearnMethods :: [NamedAPIResource MoveLearnMethod],
    vgPokedexes :: [NamedAPIResource Pokedex],
    vgRegions :: [NamedAPIResource Region],
    vgVersions :: [NamedAPIResource Version]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON VersionGroup where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/version-group\/{id or name}@](https://pokeapi.co/docs/v2#version-groups)
instance PokeApi VersionGroup where
  get iden = getFromUrl $ apiv2 /: "version-group" /: T.toLower iden

-- | [@\/version-group@](https://pokeapi.co/docs/v2#version-groups)
instance PokeApiListable VersionGroup NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "version-group"

-- * Items

-- ** Item

-- | <https://pokeapi.co/docs/v2#item>
data Item = Item
  { itemId :: Int,
    itemName :: Text,
    itemCost :: Int,
    itemFlingPower :: Maybe Int, -- verified
    itemFlingEffect :: Maybe (NamedAPIResource ItemFlingEffect), -- verified
    itemAttributes :: [NamedAPIResource ItemAttribute],
    itemCategory :: NamedAPIResource ItemCategory,
    itemEffectEntries :: [VerboseEffect],
    itemFlavorTextEntries :: [VersionGroupFlavorText],
    itemGameIndices :: [GenerationGameIndex],
    itemNames :: [Name],
    itemSprites :: ItemSprites,
    itemHeldByPokemon :: [ItemHolderPokemon],
    itemBabyTriggerFor :: Maybe (APIResource EvolutionChain),
    itemMachines :: [MachineVersionDetail]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Item where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- | [@\/item\/{id or name}@](https://pokeapi.co/docs/v2#item)
instance PokeApi Item where
  get iden = getFromUrl $ apiv2 /: "item" /: T.toLower iden

-- | [@\/item@](https://pokeapi.co/docs/v2#item)
instance PokeApiListable Item NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "item"

-- | <https://pokeapi.co/docs/v2#itemsprites>
newtype ItemSprites = ItemSprites
  { isDefault :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemSprites where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | <https://pokeapi.co/docs/v2#itemholderpokemon>
data ItemHolderPokemon = ItemHolderPokemon
  { ihpPokemon :: NamedAPIResource Pokemon,
    ihpVersionDetails :: [ItemHolderPokemonVersionDetail]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemHolderPokemon where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#itemholderpokemonversiondetail>
data ItemHolderPokemonVersionDetail = ItemHolderPokemonVersionDetail
  { ihpvdRarity :: Int,
    ihpvdVersion :: NamedAPIResource Version
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemHolderPokemonVersionDetail where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 5}

-- ** Item Attributes

-- | <https://pokeapi.co/docs/v2#item-attributes>
data ItemAttribute = ItemAttribute
  { iaId :: Int,
    iaName :: Text,
    iaItems :: [NamedAPIResource Item],
    iaNames :: [Name],
    iaDescriptions :: [Description]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemAttribute where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/item-attribute\/{id or name}@](https://pokeapi.co/docs/v2#item-attributes)
instance PokeApi ItemAttribute where
  get iden = getFromUrl $ apiv2 /: "item-attribute" /: T.toLower iden

-- | [@\/item-attribute@](https://pokeapi.co/docs/v2#item-attributes)
instance PokeApiListable ItemAttribute NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "item-attribute"

-- ** Item Categories

-- | <https://pokeapi.co/docs/v2#item-categories>
data ItemCategory = ItemCategory
  { icId :: Int,
    icName :: Text,
    icItems :: [NamedAPIResource Item],
    icNames :: [Name],
    icPocket :: NamedAPIResource ItemPocket
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemCategory where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/item-category\/{id or name}@](https://pokeapi.co/docs/v2#item-categories)
instance PokeApi ItemCategory where
  get iden = getFromUrl $ apiv2 /: "item-category" /: T.toLower iden

-- | [@\/item-category@](https://pokeapi.co/docs/v2#item-categories)
instance PokeApiListable ItemCategory NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "item-category"

-- ** Item Fling Effects

-- | <https://pokeapi.co/docs/v2#item-fling-effects>
data ItemFlingEffect = ItemFlingEffect
  { ifeId :: Int,
    ifeName :: Text,
    ifeEffectEntries :: [Effect],
    ifeItems :: [NamedAPIResource Item]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemFlingEffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | [@\/item-fling-effect\/{id or name}@](https://pokeapi.co/docs/v2#item-fling-effects)
instance PokeApi ItemFlingEffect where
  get iden = getFromUrl $ apiv2 /: "item-fling-effect" /: T.toLower iden

-- | [@\/item-fling-effect@](https://pokeapi.co/docs/v2#item-fling-effects)
instance PokeApiListable ItemFlingEffect NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "item-fling-effect"

-- ** Item Pockets

-- | <https://pokeapi.co/docs/v2#item-pockets>
data ItemPocket = ItemPocket
  { ipId :: Int,
    ipName :: Text,
    ipCategories :: [NamedAPIResource ItemCategory],
    ipNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemPocket where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/item-pocket\/{id or name}@](https://pokeapi.co/docs/v2#item-pockets)
instance PokeApi ItemPocket where
  get iden = getFromUrl $ apiv2 /: "item-pocket" /: T.toLower iden

-- | [@\/item-pocket@](https://pokeapi.co/docs/v2#item-pockets)
instance PokeApiListable ItemPocket NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "item-pocket"

-- * Locations

-- ** Locations

-- | <https://pokeapi.co/docs/v2#locations>
data Location = Location
  { locationId :: Int,
    locationName :: Text,
    locationRegion :: Maybe (NamedAPIResource Region), -- verified
    locationNames :: [Name],
    locationGameIndices :: [GenerationGameIndex],
    locationAreas :: [NamedAPIResource LocationArea]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 8}

-- | [@\/location\/{id or name}@](https://pokeapi.co/docs/v2#locations)
instance PokeApi Location where
  get iden = getFromUrl $ apiv2 /: "location" /: T.toLower iden

-- | [@\/location@](https://pokeapi.co/docs/v2#locations)
instance PokeApiListable Location NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "location"

-- ** Location Areas

-- | <https://pokeapi.co/docs/v2#location-areas>
data LocationArea = LocationArea
  { laId :: Int,
    laName :: Text,
    laGameIndex :: Int,
    laEncounterMethodRates :: [EncounterMethodRate],
    laLocation :: NamedAPIResource Location,
    laNames :: [Name],
    laPokemonEncounters :: [PokemonEncounter]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON LocationArea where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/location-area\/{id or name}@](https://pokeapi.co/docs/v2#location-areas)
instance PokeApi LocationArea where
  get iden = getFromUrl $ apiv2 /: "location-area" /: T.toLower iden

-- | [@\/location-area@](https://pokeapi.co/docs/v2#location-areas)
instance PokeApiListable LocationArea NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "location-area"

-- | <https://pokeapi.co/docs/v2#encountermethodrate>
data EncounterMethodRate = EncounterMethodRate
  { emrEncounterMethod :: NamedAPIResource EncounterMethod,
    emrVersionDetails :: [EncounterVersionDetails]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EncounterMethodRate where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#encounterversiondetails>
data EncounterVersionDetails = EncounterVersionDetails
  { evdRate :: Int,
    evdVersion :: NamedAPIResource Version
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EncounterVersionDetails where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#pokemonencounter>
data PokemonEncounter = PokemonEncounter
  { pePokemon :: NamedAPIResource Pokemon,
    peVersionDetails :: [VersionEncounterDetail]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonEncounter where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Pal Park Areas

-- | <https://pokeapi.co/docs/v2#pal-park-areas>
data PalParkArea = PalParkArea
  { ppaId :: Int,
    ppaName :: Text,
    ppaNames :: [Name],
    ppaPokemonEncounters :: [PalParkEncounterSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PalParkArea where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | [@\/pal-park-area\/{id or name}@](https://pokeapi.co/docs/v2#pal-park-areas)
instance PokeApi PalParkArea where
  get iden = getFromUrl $ apiv2 /: "pal-park-area" /: T.toLower iden

-- | [@\/pal-park-area@](https://pokeapi.co/docs/v2#pal-park-areas)
instance PokeApiListable PalParkArea NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "pal-park-area"

-- | <https://pokeapi.co/docs/v2#palparkencounterspecies>
data PalParkEncounterSpecies = PalParkEncounterSpecies
  { ppesBaseScore :: Int,
    ppesRate :: Int,
    ppesPokemonSpecies :: NamedAPIResource PokemonSpecies
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PalParkEncounterSpecies where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Regions

-- | <https://pokeapi.co/docs/v2#regions>
data Region = Region
  { regionId :: Int,
    regionName :: Text,
    regionLocations :: [NamedAPIResource Location],
    regionNames :: [Name],
    -- | @Nothing@ for Hisui
    regionMainGeneration :: Maybe (NamedAPIResource Generation),
    regionPokedexes :: [NamedAPIResource Pokedex],
    regionVersionGroups :: [NamedAPIResource VersionGroup]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Region where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 6}

-- | [@\/region\/{id or name}@](https://pokeapi.co/docs/v2#regions)
instance PokeApi Region where
  get iden = getFromUrl $ apiv2 /: "region" /: T.toLower iden

-- | [@\/region@](https://pokeapi.co/docs/v2#regions)
instance PokeApiListable Region NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "region"

-- * Machines

-- | <https://pokeapi.co/docs/v2#machines>
data Machine = Machine
  { machineId :: Int,
    machineItem :: NamedAPIResource Item,
    machineMove :: NamedAPIResource Move,
    machineVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Machine where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 7}

-- | [@\/machine\/{id}@](https://pokeapi.co/docs/v2#machines)
instance PokeApi Machine where
  get iden = getFromUrl $ apiv2 /: "machine" /: T.toLower iden

-- | [@\/machine@](https://pokeapi.co/docs/v2#machines)
instance PokeApiListable Machine APIResourceList APIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "machine"

-- * Moves

-- ** Moves

-- | <https://pokeapi.co/docs/v2#moves>
data Move = Move
  { moveId :: Int,
    moveName :: Text,
    -- | @Nothing@ for moves that cannot miss
    moveAccuracy :: Maybe Int,
    moveEffectChance :: Maybe Int,
    -- | @Nothing@ for Shadow moves (from XD: Gale of Darkness)
    movePp :: Maybe Int,
    movePriority :: Int,
    -- | @Nothing@ for status moves
    movePower :: Maybe Int,
    moveContestCombos :: Maybe ContestComboSets,
    moveContestType :: Maybe (NamedAPIResource ContestType), -- verified
    moveContestEffect :: Maybe (APIResource ContestEffect), -- verified
    moveDamageClass :: NamedAPIResource MoveDamageClass,
    moveEffectEntries :: [VerboseEffect],
    moveEffectChanges :: [AbilityEffectChange],
    moveLearnedByPokemon :: [NamedAPIResource Pokemon],
    moveFlavorTextEntries :: [MoveFlavorText],
    moveGeneration :: NamedAPIResource Generation,
    moveMachines :: [MachineVersionDetail],
    moveMeta :: Maybe MoveMetaData, -- verified
    moveNames :: [Name],
    movePastValues :: [PastMoveStatValues],
    moveStatChanges :: [MoveStatChange],
    moveSuperContestEffect :: Maybe (APIResource SuperContestEffect), -- verified
    moveTarget :: NamedAPIResource MoveTarget,
    moveType :: NamedAPIResource Type
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Move where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- | [@\/move\/{id or name}@](https://pokeapi.co/docs/v2#moves)
instance PokeApi Move where
  get iden = getFromUrl $ apiv2 /: "move" /: T.toLower iden

-- | [@\/move@](https://pokeapi.co/docs/v2#moves)
instance PokeApiListable Move NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "move"

-- | <https://pokeapi.co/docs/v2#contestcombosets>
data ContestComboSets = ContestComboSets
  { ccsNormal :: ContestComboDetail,
    ccsSuper :: ContestComboDetail
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContestComboSets where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#contestcombodetail>
data ContestComboDetail = ContestComboDetail
  { ccdUseBefore :: Maybe [NamedAPIResource Move], -- verified
    ccdUseAfter :: Maybe [NamedAPIResource Move] -- verified
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContestComboDetail where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#moveflavortext>
data MoveFlavorText = MoveFlavorText
  { mftFlavorText :: Text,
    mftLanguage :: NamedAPIResource Language,
    mftVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveFlavorText where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#movemetadata>
data MoveMetaData = MoveMetaData
  { mmdAilment :: NamedAPIResource MoveAilment,
    mmdCategory :: NamedAPIResource MoveCategory,
    mmdMinHits :: Maybe Int, -- Maybe verified
    mmdMaxHits :: Maybe Int, -- Maybe verified
    mmdMinTurns :: Maybe Int, -- Maybe verified
    mmdMaxTurns :: Maybe Int, -- Maybe verified
    mmdDrain :: Int,
    mmdHealing :: Int,
    mmdCritRate :: Int,
    mmdAilmentChance :: Int,
    mmdFlinchChance :: Int,
    mmdStatChance :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveMetaData where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#movestatchange>
data MoveStatChange = MoveStatChange
  { mscChange :: Int,
    mscStat :: NamedAPIResource Stat
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveStatChange where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#pastmovestatvalues>
data PastMoveStatValues = PastMoveStatValues
  { pmsvAccuracy :: Maybe Int, -- verified
    pmsvEffectChance :: Maybe Int, -- verified
    pmsvPower :: Maybe Int, -- verified
    pmsvPp :: Maybe Int, -- verified
    pmsvEffectEntries :: [VerboseEffect],
    pmsvType :: Maybe (NamedAPIResource Type), -- verified
    pmsvVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PastMoveStatValues where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Move Ailments

-- | <https://pokeapi.co/docs/v2#move-ailments>
data MoveAilment = MoveAilment
  { maId :: Int,
    maName :: Text,
    maMoves :: [NamedAPIResource Move],
    maNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveAilment where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/move-ailment\/{id or name}@](https://pokeapi.co/docs/v2#move-ailments)
instance PokeApi MoveAilment where
  get iden = getFromUrl $ apiv2 /: "move-ailment" /: T.toLower iden

-- | [@\/move-ailment@](https://pokeapi.co/docs/v2#move-ailments)
instance PokeApiListable MoveAilment NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-ailment"

-- ** Move Battle Styles

-- | <https://pokeapi.co/docs/v2#move-battle-styles>
data MoveBattleStyle = MoveBattleStyle
  { mbsId :: Int,
    mbsName :: Text,
    mbsNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveBattleStyle where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | [@\/move-battle-style\/{id or name}@](https://pokeapi.co/docs/v2#move-battle-styles)
instance PokeApi MoveBattleStyle where
  get iden = getFromUrl $ apiv2 /: "move-battle-style" /: T.toLower iden

-- | [@\/move-battle-style@](https://pokeapi.co/docs/v2#move-battle-styles)
instance PokeApiListable MoveBattleStyle NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-battle-style"

-- ** Move Categories

-- | <https://pokeapi.co/docs/v2#move-categories>
data MoveCategory = MoveCategory
  { mcId :: Int,
    mcName :: Text,
    mcMoves :: [NamedAPIResource Move],
    mcDescriptions :: [Description]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveCategory where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/move-category\/{id or name}@](https://pokeapi.co/docs/v2#move-categories)
instance PokeApi MoveCategory where
  get iden = getFromUrl $ apiv2 /: "move-category" /: T.toLower iden

-- | [@\/move-category@](https://pokeapi.co/docs/v2#move-categories)
instance PokeApiListable MoveCategory NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-category"

-- ** Move Damage Classes

-- | <https://pokeapi.co/docs/v2#move-damage-classes>
data MoveDamageClass = MoveDamageClass
  { mdcId :: Int,
    mdcName :: Text,
    mdcDescriptions :: [Description],
    mdcMoves :: [NamedAPIResource Move],
    mdcNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveDamageClass where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | [@\/move-damage-class\/{id or name}@](https://pokeapi.co/docs/v2#move-damage-classes)
instance PokeApi MoveDamageClass where
  get iden = getFromUrl $ apiv2 /: "move-damage-class" /: T.toLower iden

-- | [@\/move-damage-class@](https://pokeapi.co/docs/v2#move-damage-classes)
instance PokeApiListable MoveDamageClass NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-damage-class"

-- ** Move Learn Methods

-- | <https://pokeapi.co/docs/v2#move-learn-methods>
data MoveLearnMethod = MoveLearnMethod
  { mlmId :: Int,
    mlmName :: Text,
    mlmDescriptions :: [Description],
    mlmNames :: [Name],
    mlmVersionGroups :: [NamedAPIResource VersionGroup]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveLearnMethod where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | [@\/move-learn-method\/{id or name}@](https://pokeapi.co/docs/v2#move-learn-methods)
instance PokeApi MoveLearnMethod where
  get iden = getFromUrl $ apiv2 /: "move-learn-method" /: T.toLower iden

-- | [@\/move-learn-method@](https://pokeapi.co/docs/v2#move-learn-methods)
instance PokeApiListable MoveLearnMethod NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-learn-method"

-- ** Move Targets

-- | <https://pokeapi.co/docs/v2#move-targets>
data MoveTarget = MoveTarget
  { mtId :: Int,
    mtName :: Text,
    mtDescriptions :: [Description],
    mtMoves :: [NamedAPIResource Move],
    mtNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveTarget where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/move-target\/{id or name}@](https://pokeapi.co/docs/v2#move-targets)
instance PokeApi MoveTarget where
  get iden = getFromUrl $ apiv2 /: "move-target" /: T.toLower iden

-- | [@\/move-target@](https://pokeapi.co/docs/v2#move-targets)
instance PokeApiListable MoveTarget NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-target"

-- * Pokemon

-- ** Abilities

-- | <https://pokeapi.co/docs/v2#abilities>
data Ability = Ability
  { abilityId :: Int,
    abilityName :: Text,
    abilityIsMainSeries :: Bool,
    abilityGeneration :: NamedAPIResource Generation,
    abilityNames :: [Name],
    abilityEffectEntries :: [VerboseEffect],
    abilityEffectChanges :: [AbilityEffectChange],
    abilityFlavorTextEntries :: [AbilityFlavorText],
    abilityPokemon :: [AbilityPokemon]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Ability where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 7}

-- | [@\/ability\/{id or name}@](https://pokeapi.co/docs/v2#abilities)
instance PokeApi Ability where
  get iden = getFromUrl $ apiv2 /: "ability" /: T.toLower iden

-- | [@\/ability@](https://pokeapi.co/docs/v2#abilities)
instance PokeApiListable Ability NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "ability"

-- | <https://pokeapi.co/docs/v2#abilityeffectchange>
data AbilityEffectChange = AbilityEffectChange
  { aecEffectEntries :: [Effect],
    aecVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AbilityEffectChange where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#abilityflavortext>
data AbilityFlavorText = AbilityFlavorText
  { aftFlavorText :: Text,
    aftLanguage :: NamedAPIResource Language,
    aftVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AbilityFlavorText where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#abilitypokemon>
data AbilityPokemon = AbilityPokemon
  { apIsHidden :: Bool,
    apSlot :: Int,
    apPokemon :: NamedAPIResource Pokemon
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AbilityPokemon where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Characteristics

-- | <https://pokeapi.co/docs/v2#characteristics>
data Characteristic = Characteristic
  { characteristicId :: Int,
    characteristicGeneModulo :: Int,
    characteristicPossibleValues :: [Int],
    characteristicHighestStat :: NamedAPIResource Stat,
    characteristicDescriptions :: [Description]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Characteristic where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 14}

-- | [@\/characteristic\/{id}@](https://pokeapi.co/docs/v2#characteristics)
instance PokeApi Characteristic where
  get iden = getFromUrl $ apiv2 /: "characteristic" /: T.toLower iden

-- | [@\/characteristic@](https://pokeapi.co/docs/v2#characteristics)
instance PokeApiListable Characteristic APIResourceList APIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "characteristic"

-- ** Egg Groups

-- | <https://pokeapi.co/docs/v2#egg-groups>
data EggGroup = EggGroup
  { eggGroupId :: Int,
    eggGroupName :: Text,
    eggGroupNames :: [Name],
    eggGroupPokemonSpecies :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EggGroup where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 8}

-- | [@\/egg-group\/{id or name}@](https://pokeapi.co/docs/v2#egg-groups)
instance PokeApi EggGroup where
  get iden = getFromUrl $ apiv2 /: "egg-group" /: T.toLower iden

-- | [@\/egg-group@](https://pokeapi.co/docs/v2#egg-groups)
instance PokeApiListable EggGroup NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "egg-group"

-- ** Genders

-- | <https://pokeapi.co/docs/v2#genders>
data Gender = Gender
  { genderId :: Int,
    genderName :: Text,
    genderPokemonSpeciesDetails :: [PokemonSpeciesGender],
    genderRequiredForEvolution :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Gender where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 6}

-- | [@\/gender\/{id or name}@](https://pokeapi.co/docs/v2#genders)
instance PokeApi Gender where
  get iden = getFromUrl $ apiv2 /: "gender" /: T.toLower iden

-- | [@\/gender@](https://pokeapi.co/docs/v2#genders)
instance PokeApiListable Gender NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "gender"

-- | <https://pokeapi.co/docs/v2#pokemonspeciesgender>
data PokemonSpeciesGender = PokemonSpeciesGender
  { psgRate :: Int,
    psgPokemonSpecies :: NamedAPIResource PokemonSpecies
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonSpeciesGender where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- ** Growth rates

-- | <https://pokeapi.co/docs/v2#growth-rates>
data GrowthRate = GrowthRate
  { grId :: Int,
    grName :: Text,
    grFormula :: Text,
    grDescriptions :: [Description],
    grLevels :: [GrowthRateExperienceLevel],
    grPokemonSpecies :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON GrowthRate where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/growth-rate\/{id or name}@](https://pokeapi.co/docs/v2#growth-rates)
instance PokeApi GrowthRate where
  get iden = getFromUrl $ apiv2 /: "growth-rate" /: T.toLower iden

-- | [@\/growth-rate@](https://pokeapi.co/docs/v2#growth-rates)
instance PokeApiListable GrowthRate NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "growth-rate"

-- | <https://pokeapi.co/docs/v2#growthrateexperiencelevel>
data GrowthRateExperienceLevel = GrowthRateExperienceLevel
  { grelLevel :: Int,
    grelExperience :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON GrowthRateExperienceLevel where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Natures

-- | <https://pokeapi.co/docs/v2#natures>
data Nature = Nature
  { natureId :: Int,
    natureName :: Text,
    natureDecreasedStat :: Maybe (NamedAPIResource Stat),
    natureIncreasedStat :: Maybe (NamedAPIResource Stat),
    natureHatesFlavor :: Maybe (NamedAPIResource BerryFlavor),
    natureLikesFlavor :: Maybe (NamedAPIResource BerryFlavor),
    naturePokeathlonStatChanges :: [NatureStatChange],
    natureMoveBattleStylePreferences :: [MoveBattleStylePreference],
    natureNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Nature where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 6}

-- | [@\/nature\/{id or name}@](https://pokeapi.co/docs/v2#natures)
instance PokeApi Nature where
  get iden = getFromUrl $ apiv2 /: "nature" /: T.toLower iden

-- | [@\/nature@](https://pokeapi.co/docs/v2#natures)
instance PokeApiListable Nature NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "nature"

-- | <https://pokeapi.co/docs/v2#naturestatchange>
data NatureStatChange = NatureStatChange
  { nscMaxChange :: Int,
    nscPokeathlonStat :: NamedAPIResource PokeathlonStat
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NatureStatChange where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#movebattlestylepreference>
data MoveBattleStylePreference = MoveBattleStylePreference
  { mbspLowHpPreference :: Int,
    mbspHighHpPreference :: Int,
    mbspMoveBattleStyle :: NamedAPIResource MoveBattleStyle
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveBattleStylePreference where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Pokeathlon Stats

-- | <https://pokeapi.co/docs/v2#pokeathlon-stats>
data PokeathlonStat = PokeathlonStat
  { pasId :: Int,
    pasName :: Text,
    pasNames :: [Name],
    pasAffectingNatures :: NaturePokeathlonStatAffectSets
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokeathlonStat where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | [@\/pokeathlon-stat\/{id or name}@](https://pokeapi.co/docs/v2#pokeathlon-stats)
instance PokeApi PokeathlonStat where
  get iden = getFromUrl $ apiv2 /: "pokeathlon-stat" /: T.toLower iden

-- | [@\/pokeathlon-stat@](https://pokeapi.co/docs/v2#pokeathlon-stats)
instance PokeApiListable PokeathlonStat NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokeathlon-stat"

-- | <https://pokeapi.co/docs/v2#naturepokeathlonstataffectsets>
data NaturePokeathlonStatAffectSets = NaturePokeathlonStatAffectSets
  { npsasIncrease :: [NaturePokeathlonStatAffect],
    npsasDecrease :: [NaturePokeathlonStatAffect]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NaturePokeathlonStatAffectSets where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 5}

-- | <https://pokeapi.co/docs/v2#naturepokeathlonstataffect>
data NaturePokeathlonStatAffect = NaturePokeathlonStatAffect
  { npsaMaxChange :: Int,
    npsaNature :: NamedAPIResource Nature
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NaturePokeathlonStatAffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Pokemon

-- | <https://pokeapi.co/docs/v2#pokemon>
data Pokemon = Pokemon
  { pokemonId :: Int,
    pokemonName :: Text,
    pokemonBaseExperience :: Maybe Int, -- verified Maybe, this is absent in Gen 9
    pokemonHeight :: Int,
    pokemonIsDefault :: Bool,
    pokemonOrder :: Int,
    pokemonWeight :: Int,
    pokemonAbilities :: [PokemonAbility],
    pokemonForms :: [NamedAPIResource PokemonForm],
    pokemonGameIndices :: [VersionGameIndex],
    pokemonHeldItems :: [PokemonHeldItem],
    pokemonLocationAreaEncounters :: Text,
    pokemonMoves :: [PokemonMove],
    pokemonPastTypes :: [PokemonTypePast],
    pokemonSprites :: PokemonSprites,
    pokemonSpecies :: NamedAPIResource PokemonSpecies,
    pokemonStats :: [PokemonStat],
    pokemonTypes :: [PokemonType]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Pokemon where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 7}

-- | [@\/pokemon\/{id or name}@](https://pokeapi.co/docs/v2#pokemon)
instance PokeApi Pokemon where
  get iden = getFromUrl $ apiv2 /: "pokemon" /: T.toLower iden

-- | [@\/pokemon@](https://pokeapi.co/docs/v2#pokemon)
instance PokeApiListable Pokemon NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon"

-- | <https://pokeapi.co/docs/v2#pokemonability>
data PokemonAbility = PokemonAbility
  { paIsHidden :: Bool,
    paSlot :: Int,
    paAbility :: NamedAPIResource Ability
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonAbility where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | <https://pokeapi.co/docs/v2#pokemontype>
data PokemonType = PokemonType
  { ptSlot :: Int,
    ptType :: NamedAPIResource Type
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonType where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | <https://pokeapi.co/docs/v2#pokemonformtype>
data PokemonFormType = PokemonFormType
  { pftSlot :: Int,
    pftType :: NamedAPIResource Type
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonFormType where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#pokemontypepast>
data PokemonTypePast = PokemonTypePast
  { ptpGeneration :: NamedAPIResource Generation,
    ptpTypes :: [PokemonType]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonTypePast where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#pokemonhelditem>
data PokemonHeldItem = PokemonHeldItem
  { phiItem :: NamedAPIResource Item,
    phiVersionDetails :: [PokemonHeldItemVersion]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonHeldItem where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#pokemonhelditemversion>
data PokemonHeldItemVersion = PokemonHeldItemVersion
  { phivVersion :: NamedAPIResource Version,
    phivRarity :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonHeldItemVersion where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- | <https://pokeapi.co/docs/v2#pokemonmove>
data PokemonMove = PokemonMove
  { pmMove :: NamedAPIResource Move,
    pmVersionGroupDetails :: [PokemonMoveVersion]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonMove where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | <https://pokeapi.co/docs/v2#pokemonmoveversion>
data PokemonMoveVersion = PokemonMoveVersion
  { pmvMoveLearnMethod :: NamedAPIResource MoveLearnMethod,
    pmvVersionGroup :: NamedAPIResource VersionGroup,
    pmvLevelLearnedAt :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonMoveVersion where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#pokemonstat>
data PokemonStat = PokemonStat
  { psStat :: NamedAPIResource Stat,
    psEffort :: Int,
    psBaseStat :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonStat where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | <https://pokeapi.co/docs/v2#pokemonsprites>
data PokemonSprites = PokemonSprites
  { psFrontDefault :: Maybe Text, -- verified
    psFrontShiny :: Maybe Text, -- verified
    psFrontFemale :: Maybe Text, -- verified
    psFrontShinyFemale :: Maybe Text, -- verified
    psBackDefault :: Maybe Text, -- verified
    psBackShiny :: Maybe Text, -- verified
    psBackFemale :: Maybe Text, -- verified
    psBackShinyFemale :: Maybe Text -- verified
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonSprites where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Pokemon Location Areas

-- | <https://pokeapi.co/docs/v2#pokemon-location-areas>
--
-- NOTE: This is the only endpoint which cannot be queried as a list.
newtype PokemonLocationArea = PokemonLocationArea
  {plaEncounters :: [LocationAreaEncounter]}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonLocationArea where
  parseJSON o = PokemonLocationArea <$> parseJSON o

-- | [@\/pokemon\/{id or name}\/encounters@](https://pokeapi.co/docs/v2#pokemon-location-areas)
instance PokeApi PokemonLocationArea where
  get iden = getFromUrl $ apiv2 /: "pokemon" /: T.toLower iden /: "encounters"

-- | <https://pokeapi.co/docs/v2#pokemon-location-areas>
data LocationAreaEncounter = LocationAreaEncounter
  { laeLocationArea :: NamedAPIResource LocationArea,
    laeVersionDetails :: [VersionEncounterDetail]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON LocationAreaEncounter where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- ** Pokemon Colors

-- | <https://pokeapi.co/docs/v2#pokemon-colors>
data PokemonColor = PokemonColor
  { pcId :: Int,
    pcName :: Text,
    pcNames :: [Name],
    pcPokemonSpecies :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonColor where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/pokemon-color\/{id or name}@](https://pokeapi.co/docs/v2#pokemon-colors)
instance PokeApi PokemonColor where
  get iden = getFromUrl $ apiv2 /: "pokemon-color" /: T.toLower iden

-- | [@\/pokemon-color@](https://pokeapi.co/docs/v2#pokemon-colors)
instance PokeApiListable PokemonColor NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon-color"

-- ** Pokemon Forms

-- | <https://pokeapi.co/docs/v2#pokemon-forms>
data PokemonForm = PokemonForm
  { pfId :: Int,
    pfName :: Text,
    pfOrder :: Int,
    pfFormOrder :: Int,
    pfIsDefault :: Bool,
    pfIsBattleOnly :: Bool,
    pfIsMega :: Bool,
    pfFormName :: Text,
    pfPokemon :: NamedAPIResource Pokemon,
    pfTypes :: [PokemonFormType],
    pfSprites :: PokemonFormSprites,
    pfVersionGroup :: NamedAPIResource VersionGroup,
    pfNames :: [Name],
    pfFormNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonForm where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/pokemon-form\/{id or name}@](https://pokeapi.co/docs/v2#pokemon-forms)
instance PokeApi PokemonForm where
  get iden = getFromUrl $ apiv2 /: "pokemon-form" /: T.toLower iden

-- | [@\/pokemon-form@](https://pokeapi.co/docs/v2#pokemon-forms)
instance PokeApiListable PokemonForm NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon-form"

-- | <https://pokeapi.co/docs/v2#pokemonformsprites>
data PokemonFormSprites = PokemonFormSprites
  { pfsFrontDefault :: Maybe Text,
    pfsFrontShiny :: Maybe Text,
    pfsBackDefault :: Maybe Text,
    pfsBackShiny :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonFormSprites where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- ** Pokemon Habitats

-- | <https://pokeapi.co/docs/v2#pokemon-habitats>
data PokemonHabitat = PokemonHabitat
  { phId :: Int,
    phName :: Text,
    phNames :: [Name],
    phPokemonSpecies :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonHabitat where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/pokemon-habitat\/{id or name}@](https://pokeapi.co/docs/v2#pokemon-habitats)
instance PokeApi PokemonHabitat where
  get iden = getFromUrl $ apiv2 /: "pokemon-habitat" /: T.toLower iden

-- | [@\/pokemon-habitat@](https://pokeapi.co/docs/v2#pokemon-habitats)
instance PokeApiListable PokemonHabitat NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon-habitat"

-- ** Pokemon Shapes

-- | <https://pokeapi.co/docs/v2#pokemon-shapes>
data PokemonShape = PokemonShape
  { pshId :: Int,
    pshName :: Text,
    pshAwesomeNames :: [AwesomeName],
    pshNames :: [Name],
    pshPokemonSpecies :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonShape where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | [@\/pokemon-shape\/{id or name}@](https://pokeapi.co/docs/v2#pokemon-shapes)
instance PokeApi PokemonShape where
  get iden = getFromUrl $ apiv2 /: "pokemon-shape" /: T.toLower iden

-- | [@\/pokemon-shape@](https://pokeapi.co/docs/v2#pokemon-shapes)
instance PokeApiListable PokemonShape NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon-shape"

-- | <https://pokeapi.co/docs/v2#awesomename>
data AwesomeName = AwesomeName
  { anAwesomeName :: Text,
    anLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AwesomeName where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Pokemon Species

-- | <https://pokeapi.co/docs/v2#pokemon-species>
data PokemonSpecies = PokemonSpecies
  { psId :: Int,
    psName :: Text,
    psOrder :: Int,
    psGenderRate :: Int,
    psCaptureRate :: Int,
    -- | @Nothing@ for PLA-exclusive evolutions
    psBaseHappiness :: Maybe Int,
    psIsBaby :: Bool,
    psIsLegendary :: Bool,
    psIsMythical :: Bool,
    -- | @Nothing@ for PLA-exclusive evolutions
    psHatchCounter :: Maybe Int,
    psHasGenderDifferences :: Bool,
    psFormsSwitchable :: Bool,
    psGrowthRate :: NamedAPIResource GrowthRate,
    psPokedexNumbers :: [PokemonSpeciesDexEntry],
    psEggGroups :: [NamedAPIResource EggGroup],
    psColor :: NamedAPIResource PokemonColor,
    psShape :: Maybe (NamedAPIResource PokemonShape),
    psEvolutionChain :: APIResource EvolutionChain,
    psHabitat :: Maybe (NamedAPIResource PokemonHabitat),
    psGeneration :: NamedAPIResource Generation,
    psNames :: [Name],
    psPalParkEncounters :: [PalParkEncounterArea],
    psFlavorTextEntries :: [FlavorText],
    psFormDescriptions :: [Description],
    psGenera :: [Genus],
    psVarieties :: [PokemonSpeciesVariety]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonSpecies where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | [@\/pokemon-species\/{id or name}@](https://pokeapi.co/docs/v2#pokemon-species)
instance PokeApi PokemonSpecies where
  get iden = getFromUrl $ apiv2 /: "pokemon-species" /: T.toLower iden

-- | [@\/pokemon-species@](https://pokeapi.co/docs/v2#pokemon-species)
instance PokeApiListable PokemonSpecies NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon-species"

-- | <https://pokeapi.co/docs/v2#genus>
data Genus = Genus
  { genusGenus :: Text,
    genusLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Genus where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 5}

-- | <https://pokeapi.co/docs/v2#pokemonspeciesdexentry>
data PokemonSpeciesDexEntry = PokemonSpeciesDexEntry
  { psdeEntryNumber :: Int,
    psdePokedex :: NamedAPIResource Pokedex
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonSpeciesDexEntry where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- | <https://pokeapi.co/docs/v2#palparkencounterarea>
data PalParkEncounterArea = PalParkEncounterArea
  { ppeaBaseScore :: Int,
    ppeaRate :: Int,
    ppeaArea :: NamedAPIResource PalParkArea
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PalParkEncounterArea where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- | <https://pokeapi.co/docs/v2#pokemonspeciesvariety>
data PokemonSpeciesVariety = PokemonSpeciesVariety
  { psvIsDefault :: Bool,
    psvPokemon :: NamedAPIResource Pokemon
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonSpeciesVariety where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- ** Stats

-- | <https://pokeapi.co/docs/v2#stats>
data Stat = Stat
  { statId :: Int,
    statName :: Text,
    statGameIndex :: Int,
    statIsBattleOnly :: Bool,
    statAffectingMoves :: MoveStatAffectSets,
    statAffectingNatures :: NatureStatAffectSets,
    statCharacteristics :: [APIResource Characteristic],
    statMoveDamageClass :: NamedAPIResource MoveDamageClass,
    statNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Stat where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- | [@\/stat\/{id or name}@](https://pokeapi.co/docs/v2#stats)
instance PokeApi Stat where
  get iden = getFromUrl $ apiv2 /: "stat" /: T.toLower iden

-- | [@\/stat@](https://pokeapi.co/docs/v2#stats)
instance PokeApiListable Stat NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "stat"

-- | <https://pokeapi.co/docs/v2#movestataffectsets>
data MoveStatAffectSets = MoveStatAffectSets
  { msasIncrease :: [MoveStatAffect],
    msasDecrease :: [MoveStatAffect]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveStatAffectSets where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- | <https://pokeapi.co/docs/v2#movestataffect>
data MoveStatAffect = MoveStatAffect
  { msaChange :: Int,
    msaMove :: NamedAPIResource Move
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveStatAffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#naturestataffectsets>
data NatureStatAffectSets = NatureStatAffectSets
  { nsasIncrease :: [NamedAPIResource Nature],
    nsasDecrease :: [NamedAPIResource Nature]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NatureStatAffectSets where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Types

-- | <https://pokeapi.co/docs/v2#types>
data Type = Type
  { typeId :: Int,
    typeName :: Text,
    typeDamageRelations :: TypeRelations,
    typePastDamageRelations :: [TypeRelationsPast],
    typeGameIndices :: [GenerationGameIndex],
    typeGeneration :: NamedAPIResource Generation,
    -- | @Nothing@ for Fairy (was introduced after physical/special split), Unknown (Curse in early generations), and Shadow
    typeMoveDamageClass :: Maybe (NamedAPIResource MoveDamageClass),
    typeNames :: [Name],
    typePokemon :: [TypePokemon],
    typeMoves :: [NamedAPIResource Move]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Type where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- | [@\/type\/{id or name}@](https://pokeapi.co/docs/v2#types)
instance PokeApi Type where
  get iden = getFromUrl $ apiv2 /: "type" /: T.toLower iden

-- | [@\/type@](https://pokeapi.co/docs/v2#types)
instance PokeApiListable Type NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "type"

-- | <https://pokeapi.co/docs/v2#typepokemon>
data TypePokemon = TypePokemon
  { tpSlot :: Int,
    tpPokemon :: NamedAPIResource Pokemon
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TypePokemon where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | <https://pokeapi.co/docs/v2#typerelations>
data TypeRelations = TypeRelations
  { trNoDamageTo :: [NamedAPIResource Type],
    trHalfDamageTo :: [NamedAPIResource Type],
    trDoubleDamageTo :: [NamedAPIResource Type],
    trNoDamageFrom :: [NamedAPIResource Type],
    trHalfDamageFrom :: [NamedAPIResource Type],
    trDoubleDamageFrom :: [NamedAPIResource Type]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TypeRelations where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | <https://pokeapi.co/docs/v2#typerelationspast>
data TypeRelationsPast = TypeRelationsPast
  { trpGeneration :: NamedAPIResource Generation,
    trpDamageRelations :: TypeRelations
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TypeRelationsPast where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- * Utility

-- ** Languages

-- | <https://pokeapi.co/docs/v2#languages>
data Language = Language
  { languageId :: Int,
    languageName :: Text,
    languageOfficial :: Bool,
    languageIso639 :: Text,
    languageIso3166 :: Text,
    languageNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Language where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 8}

-- | [@\/language\/{id or name}@](https://pokeapi.co/docs/v2#languages)
instance PokeApi Language where
  get iden = getFromUrl $ apiv2 /: "language" /: T.toLower iden

-- | [@\/language@](https://pokeapi.co/docs/v2#languages)
instance PokeApiListable Language NamedAPIResourceList NamedAPIResource where
  gets lim off = getsFromUrl lim off $ apiv2 /: "language"

-- ** (Named-)APIResources

-- | <https://pokeapi.co/docs/v2#namedapiresource>
data NamedAPIResource a = NamedAPIResource
  { name :: Text,
    -- | You can use 'url' instead of 'narUrl' if you really need to access this.
    narUrl :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (NamedAPIResource a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = \case "narUrl" -> "url"; "name" -> "name"; s' -> s'}

-- | <https://pokeapi.co/docs/v2#apiresource>
newtype APIResource a = APIResource
  { -- | You can use 'url' instead of 'arUrl' if you really need to access this.
    arUrl :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (APIResource a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

instance Resolvable NamedAPIResource where
  url = narUrl

instance Resolvable APIResource where
  url = arUrl

-- ** Lists of (Named-)APIResources (to deal with pagination)

-- | The 'gets' function returns @Resolvable t => [t a]@. However, the actual
-- API response is wrapped in an object, which this type models. You should not
-- need to use this type.
data NamedAPIResourceList a = NamedAPIResourceList
  { narlCount :: Int,
    narlNext :: Maybe Text,
    narlPrevious :: Maybe Text,
    narlResults :: [NamedAPIResource a]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (NamedAPIResourceList a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- | See 'NamedAPIResourceList'.
data APIResourceList a = APIResourceList
  { arlCount :: Int,
    arlNext :: Maybe Text,
    arlPrevious :: Maybe Text,
    arlResults :: [APIResource a]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (APIResourceList a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

instance ResolvableList NamedAPIResourceList NamedAPIResource where
  results = narlResults

instance ResolvableList APIResourceList APIResource where
  results = arlResults

-- ** Common Models

-- | <https://pokeapi.co/docs/v2#description>
data Description = Description
  { descriptionDescription :: Text,
    descriptionLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Description where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 11}

-- | <https://pokeapi.co/docs/v2#effect>
data Effect = Effect
  { effectEffect :: Text,
    effectLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Effect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 6}

-- | <https://pokeapi.co/docs/v2#encounter>
data Encounter = Encounter
  { encounterMinLevel :: Int,
    encounterMaxLevel :: Int,
    encounterConditionValues :: [NamedAPIResource EncounterConditionValue],
    encounterChance :: Int,
    encounterMethod :: NamedAPIResource EncounterMethod
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Encounter where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 9}

-- | <https://pokeapi.co/docs/v2#flavortext>
data FlavorText = FlavorText
  { ftFlavorText :: Text,
    ftLanguage :: NamedAPIResource Language,
    ftVersion :: Maybe (NamedAPIResource Version) -- verified
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON FlavorText where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | <https://pokeapi.co/docs/v2#generationgameindex>
data GenerationGameIndex = GenerationGameIndex
  { ggiGameIndex :: Int,
    ggiGeneration :: NamedAPIResource Generation
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON GenerationGameIndex where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#machineversiondetail>
data MachineVersionDetail = MachineVersionDetail
  { mvdMachine :: APIResource Machine,
    mvdVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MachineVersionDetail where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#name>
data Name = Name
  { nameName :: Text,
    nameLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Name where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- | <https://pokeapi.co/docs/v2#verboseeffect>
data VerboseEffect = VerboseEffect
  { veEffect :: Text,
    veShortEffect :: Text,
    veLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON VerboseEffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- | <https://pokeapi.co/docs/v2#versionencounterdetail>
data VersionEncounterDetail = VersionEncounterDetail
  { vedVersion :: NamedAPIResource Version,
    vedMaxChance :: Int,
    vedEncounterDetails :: [Encounter]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON VersionEncounterDetail where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#versiongameindex>
data VersionGameIndex = VersionGameIndex
  { vgiGameIndex :: Int,
    vgiVersion :: NamedAPIResource Version
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON VersionGameIndex where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- | <https://pokeapi.co/docs/v2#versiongroupflavortext>
data VersionGroupFlavorText = VersionGroupFlavorText
  { vgftText :: Text,
    vgftLanguage :: NamedAPIResource Language,
    vgftVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON VersionGroupFlavorText where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}
