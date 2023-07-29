{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Pokeapi.Types where

import Control.Exception (Exception (..), catch, throwIO, try)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import Network.HTTP.Req
import Text.URI (mkURI)

apiv2 :: Url 'Https
apiv2 = https "pokeapi.co" /: "api" /: "v2"

ua :: Option 'Https
ua = header "user-agent" "pokeapi-haskell v0.1.0.0 github:penelopeysm/pokeapi"

-- * Exceptions

data PokeException
  = PokeException Text
  | PokeHttpException HttpException
  | PokeJsonException Text
  deriving (Show)

instance Exception PokeException

-- * Helper functions

lower1 :: String -> String
lower1 [] = []
lower1 (x : xs) = toLower x : xs

flm :: Int -> String -> String
flm n = camelTo2 '_' . lower1 . drop n

-- | Run a Req action, catching any HTTP exceptions and rethrowing them as
-- 'PokeHttpException's.
runReq' :: Req a -> IO a
runReq' req = do
  resp <- try (runReq defaultHttpConfig req)
  case resp of
    Left e -> throwIO (PokeHttpException e)
    Right res -> pure res

lbsToText :: LBS.ByteString -> Text
lbsToText = T.decodeUtf8 . BS.concat . LBS.toChunks

-- | Decode a lazy ByteString response into a type, throwing any errors as
-- a wrapped 'PokeJsonException'. Takes a URL as an extra parameter, which is
-- used in the exception message.
decodeBody' :: (FromJSON a) => Url 'Https -> LBS.ByteString -> IO a
decodeBody' uri body = case eitherDecode body of
  Left err -> throwIO $ PokeJsonException $ T.pack err <> " when accessing URL: " <> T.pack (show uri)
  Right res -> pure res

-- | Extract the last component of a URL.
getLastUrlComponent :: Text -> Text
getLastUrlComponent = last . T.splitOn "/" . T.dropWhileEnd (== '/')

class (FromJSON a) => PokeApiResource a where
  -- | Retrieve an instance of the resource from the URL.
  getFromUrl' :: Text -> IO a
  getFromUrl' u = do
    mbUrl <- useHttpsURI <$> mkURI u
    case mbUrl of
      Nothing -> throwIO $ PokeException $ "Invalid HTTPS URL: " <> u
      Just (url, _) -> getFromUrl url

  -- | Retrieve an instance of the resource from the URL.
  getFromUrl :: Url 'Https -> IO a
  getFromUrl url = do
    body <- runReq' $ do
      resp <- req GET url NoReqBody lbsResponse ua
      pure (responseBody resp)
    decodeBody' url body

  -- | Retrieve a list of resources from the URL.
  getsFromUrl :: Maybe Int -> Maybe Int -> Url 'Https -> IO [NamedAPIResource a]
  getsFromUrl lim off url = do
    let limitHeader = maybe mempty ("limit" =:) lim
        offsetHeader = maybe mempty ("offset" =:) off
    body <- runReq' $ do
      resp <- req GET url NoReqBody lbsResponse (ua <> limitHeader <> offsetHeader)
      pure (responseBody resp)
    narlResults <$> decodeBody' url body

  -- | Retrieve an instance of the resource from the identifier (either a
  -- numeric ID or a name).
  get :: Text -> IO a

  -- | Retrieve a list of resources.
  gets ::
    -- | Limit (i.e. how many resources to return). Defaults to 20.
    Maybe Int ->
    -- | Offset (i.e. the index of the first resource to return). Defaults to 0.
    Maybe Int ->
    -- | Returned resources (in the form of 'NamedAPIResource's). To retrieve
    -- the full data for each resource, you can do:
    --
    --     namedResources <- gets 200 0  -- namedResources :: [NamedAPIResource a]
    --     resources <- mapM resolve namedResources -- resources :: [a]
    --
    -- Note that this will make a separate API call for each resource, so it
    -- may be slow. You should consider introducing a rate limit yourself.
    --
    -- To obtain a list of all resources, pass a very large number as the limit.
    IO [NamedAPIResource a]

-- * Berries

-- ** Berries

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

instance PokeApiResource Berry where
  get iden = getFromUrl $ apiv2 /: "berry" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "berry"

data BerryFlavorMap = BerryFlavorMap
  { bfmPotency :: Int,
    bfmFlavor :: NamedAPIResource BerryFlavor
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON BerryFlavorMap where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- * Berry Firmnesses

data BerryFirmness = BerryFirmness
  { bfmId :: Int,
    bfmName :: Text,
    bfmBerries :: [NamedAPIResource Berry],
    bfmNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON BerryFirmness where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

instance PokeApiResource BerryFirmness where
  get iden = getFromUrl $ apiv2 /: "berry-firmness" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "berry-firmness"

-- * Berry Flavors

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

instance PokeApiResource BerryFlavor where
  get iden = getFromUrl $ apiv2 /: "berry-flavor" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "berry-flavor"

data FlavorBerryMap = FlavorBerryMap
  { fbmPotency :: Int,
    fbmBerry :: NamedAPIResource Berry
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON FlavorBerryMap where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- * Contests

-- ** Contest Types

data ContestType = ContestType
  { ctId :: Int,
    ctName :: Text,
    ctBerryFlavor :: NamedAPIResource BerryFlavor,
    ctNames :: [ContestName]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContestType where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

instance PokeApiResource ContestType where
  get iden = getFromUrl $ apiv2 /: "contest-type" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "contest-type"

data ContestName = ContestName
  { cnName :: Text,
    cnColor :: Text,
    cnLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContestName where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Contest Effects

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

instance PokeApiResource ContestEffect where
  get iden = getFromUrl $ apiv2 /: "contest-effect" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "contest-effect"

-- ** Super Contest Effects

data SuperContestEffect = SuperContestEffect
  { sceId :: Int,
    sceAppeal :: Int,
    sceFlavorTextEntries :: [FlavorText],
    sceMoves :: [NamedAPIResource Move]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SuperContestEffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

instance PokeApiResource SuperContestEffect where
  get iden = getFromUrl $ apiv2 /: "super-contest-effect" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "super-contest-effect"

-- * Encounters

-- ** Encounter Methods

data EncounterMethod = EncounterMethod
  { emId :: Int,
    emName :: Text,
    emOrder :: Int,
    emNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EncounterMethod where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

instance PokeApiResource EncounterMethod where
  get iden = getFromUrl $ apiv2 /: "encounter-method" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "encounter-method"

-- ** Encounter Conditions

data EncounterCondition = EncounterCondition
  { ecId :: Int,
    ecName :: Text,
    ecNames :: [Name],
    ecValues :: [NamedAPIResource EncounterConditionValue]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EncounterCondition where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

instance PokeApiResource EncounterCondition where
  get iden = getFromUrl $ apiv2 /: "encounter-condition" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "encounter-condition"

-- ** Encounter Condition Values

data EncounterConditionValue = EncounterConditionValue
  { ecvId :: Int,
    ecvName :: Text,
    ecvCondition :: NamedAPIResource EncounterCondition,
    ecvNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EncounterConditionValue where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

instance PokeApiResource EncounterConditionValue where
  get iden = getFromUrl $ apiv2 /: "encounter-condition-value" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "encounter-condition-value"

-- * Evolution

-- ** Evolution Chains

data EvolutionChain = EvolutionChain
  { echId :: Int,
    echBabyTriggerItem :: Maybe (NamedAPIResource Item), -- Maybe verified
    echChain :: ChainLink
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EvolutionChain where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

instance PokeApiResource EvolutionChain where
  get iden = getFromUrl $ apiv2 /: "evolution-chain" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "evolution-chain"

data ChainLink = ChainLink
  { clIsBaby :: Bool,
    clSpecies :: NamedAPIResource PokemonSpecies,
    clEvolutionDetails :: [EvolutionDetail],
    clEvolvesTo :: [ChainLink]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ChainLink where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

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

data EvolutionTrigger = EvolutionTrigger
  { etId :: Int,
    etName :: Text,
    etNames :: [Name],
    etPokemonSpecies :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EvolutionTrigger where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

instance PokeApiResource EvolutionTrigger where
  get iden = getFromUrl $ apiv2 /: "evolution-trigger" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "evolution-trigger"

-- * Games

-- ** Generations

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

instance PokeApiResource Generation where
  get iden = getFromUrl $ apiv2 /: "generation" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "generation"

-- ** Pokedexes

data Pokedex = Pokedex
  { pokedexId :: Int,
    pokedexName :: Text,
    pokedexIsMainSeries :: Bool,
    pokedexDescriptions :: [Description],
    pokedexNames :: [Name],
    pokedexPokemonEntries :: [PokemonEntry],
    pokedexRegion :: NamedAPIResource Region,
    pokedexVersionGroups :: [NamedAPIResource VersionGroup]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Pokedex where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 7}

instance PokeApiResource Pokedex where
  get iden = getFromUrl $ apiv2 /: "pokedex" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokedex"

data PokemonEntry = PokemonEntry
  { peEntryNumber :: Int,
    pePokemonSpecies :: NamedAPIResource PokemonSpecies
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonEntry where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Version

data Version = Version
  { versionId :: Int,
    versionName :: Text,
    versionNames :: [Name],
    versionVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Version where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 7}

instance PokeApiResource Version where
  get iden = getFromUrl $ apiv2 /: "version" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "version"

-- ** Version Groups

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

instance PokeApiResource VersionGroup where
  get iden = getFromUrl $ apiv2 /: "version-group" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "version-group"

-- * Items

-- ** Item

data Item = Item
  { itemId :: Int,
    itemName :: Text,
    itemCost :: Int,
    itemFlingPower :: Int,
    itemFlingEffect :: NamedAPIResource ItemFlingEffect,
    itemAttributes :: [NamedAPIResource ItemAttribute],
    itemCategory :: NamedAPIResource ItemCategory,
    itemEffectEntries :: [VerboseEffect],
    itemFlavorTextEntries :: [VersionGroupFlavorText],
    itemGameIndices :: [GenerationGameIndex],
    itemNames :: [Name],
    itemSprites :: ItemSprites,
    itemHeldByPokemon :: [ItemHolderPokemon],
    itemBabyTriggerFor :: APIResource EvolutionChain,
    itemMachines :: [MachineVersionDetail]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Item where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

instance PokeApiResource Item where
  get iden = getFromUrl $ apiv2 /: "item" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "item"

newtype ItemSprites = ItemSprites
  { isDefault :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemSprites where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data ItemHolderPokemon = ItemHolderPokemon
  { ihpPokemon :: NamedAPIResource Pokemon,
    ihpVersionDetails :: [ItemHolderPokemonVersionDetail]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemHolderPokemon where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data ItemHolderPokemonVersionDetail = ItemHolderPokemonVersionDetail
  { ihpvdRarity :: Int,
    ihpvdVersion :: NamedAPIResource Version
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemHolderPokemonVersionDetail where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 5}

-- ** Item Attributes

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

instance PokeApiResource ItemAttribute where
  get iden = getFromUrl $ apiv2 /: "item-attribute" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "item-attribute"

-- ** Item Categories

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

instance PokeApiResource ItemCategory where
  get iden = getFromUrl $ apiv2 /: "item-category" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "item-category"

-- ** Item Fling Effects

data ItemFlingEffect = ItemFlingEffect
  { ifeId :: Int,
    ifeName :: Text,
    ifeEffectEntries :: [Effect],
    ifeItems :: [NamedAPIResource Item]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemFlingEffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

instance PokeApiResource ItemFlingEffect where
  get iden = getFromUrl $ apiv2 /: "item-fling-effect" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "item-fling-effect"

-- ** Item Pockets

data ItemPocket = ItemPocket
  { ipId :: Int,
    ipName :: Text,
    ipCategories :: [NamedAPIResource ItemCategory],
    ipNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemPocket where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

instance PokeApiResource ItemPocket where
  get iden = getFromUrl $ apiv2 /: "item-pocket" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "item-pocket"

-- * Locations

-- ** Locations

data Location = Location
  { locationId :: Int,
    locationName :: Text,
    locationRegion :: NamedAPIResource Region,
    locationNames :: [Name],
    locationGameIndices :: [GenerationGameIndex],
    locationAreas :: [NamedAPIResource LocationArea]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 8}

instance PokeApiResource Location where
  get iden = getFromUrl $ apiv2 /: "location" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "location"

-- ** Location Areas

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

instance PokeApiResource LocationArea where
  get iden = getFromUrl $ apiv2 /: "location-area" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "location-area"

data EncounterMethodRate = EncounterMethodRate
  { emrEncounterMethod :: NamedAPIResource EncounterMethod,
    emrVersionDetails :: [EncounterVersionDetails]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EncounterMethodRate where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data EncounterVersionDetails = EncounterVersionDetails
  { evdRate :: Int,
    evdVersion :: NamedAPIResource Version
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EncounterVersionDetails where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data PokemonEncounter = PokemonEncounter
  { pePokemon :: NamedAPIResource Pokemon,
    peVersionDetails :: [VersionEncounterDetail]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonEncounter where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Pal Park Areas

data PalParkArea = PalParkArea
  { ppaId :: Int,
    ppaName :: Text,
    ppaNames :: [Name],
    ppaPokemonEncounters :: [PalParkEncounterSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PalParkArea where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

instance PokeApiResource PalParkArea where
  get iden = getFromUrl $ apiv2 /: "pal-park-area" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "pal-park-area"

data PalParkEncounterSpecies = PalParkEncounterSpecies
  { ppesBaseScore :: Int,
    ppesRate :: Int,
    ppesPokemonSpecies :: NamedAPIResource PokemonSpecies
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PalParkEncounterSpecies where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Regions

data Region = Region
  { regionId :: Int,
    regionName :: Text,
    regionLocations :: [NamedAPIResource Location],
    regionNames :: [Name],
    regionMainGeneration :: NamedAPIResource Generation,
    regionPokedexes :: [NamedAPIResource Pokedex],
    regionVersionGroups :: [NamedAPIResource VersionGroup]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Region where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 6}

instance PokeApiResource Region where
  get iden = getFromUrl $ apiv2 /: "region" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "region"

-- * Machines

data Machine = Machine
  { machineId :: Int,
    machineItem :: NamedAPIResource Item,
    machineMove :: NamedAPIResource Move,
    machineVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Machine where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 7}

instance PokeApiResource Machine where
  get iden = getFromUrl $ apiv2 /: "machine" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "machine"

-- * Moves

-- ** Moves

data Move = Move
  { moveId :: Int,
    moveName :: Text,
    moveAccuracy :: Int,
    moveEffectChance :: Maybe Int, -- Maybe verified
    movePp :: Int,
    movePriority :: Int,
    movePower :: Int,
    moveContestCombos :: ContestComboSets,
    moveContestType :: NamedAPIResource ContestType,
    moveContestEffect :: APIResource ContestEffect,
    moveDamageClass :: NamedAPIResource MoveDamageClass,
    moveEffectEntries :: [VerboseEffect],
    moveEffectChanges :: [AbilityEffectChange],
    moveLearnedByPokemon :: [NamedAPIResource Pokemon],
    moveFlavorTextEntries :: [MoveFlavorText],
    moveGeneration :: NamedAPIResource Generation,
    moveMachines :: [MachineVersionDetail],
    moveMeta :: MoveMetaData,
    moveNames :: [Name],
    movePastValues :: [PastMoveStatValues],
    moveStatChanges :: [MoveStatChange],
    moveSuperContestEffect :: APIResource SuperContestEffect,
    moveTarget :: NamedAPIResource MoveTarget,
    moveType :: NamedAPIResource Type
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Move where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

instance PokeApiResource Move where
  get iden = getFromUrl $ apiv2 /: "move" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "move"

data ContestComboSets = ContestComboSets
  { ccsNormal :: ContestComboDetail,
    ccsSuper :: ContestComboDetail
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContestComboSets where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data ContestComboDetail = ContestComboDetail
  { ccdUseBefore :: [NamedAPIResource Move],
    ccdUseAfter :: [NamedAPIResource Move]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContestComboDetail where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data MoveFlavorText = MoveFlavorText
  { mftFlavorText :: Text,
    mftLanguage :: NamedAPIResource Language,
    mftVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveFlavorText where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

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

data MoveStatChange = MoveStatChange
  { mscChange :: Int,
    mscStat :: NamedAPIResource Stat
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveStatChange where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data PastMoveStatValues = PastMoveStatValues
  { pmsvAccuracy :: Int,
    pmsvEffectChance :: Int,
    pmsvPower :: Int,
    pmsvPp :: Int,
    pmsvEffectEntries :: [VerboseEffect],
    pmsvType :: NamedAPIResource Type,
    pmsvVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PastMoveStatValues where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Move Ailments

data MoveAilment = MoveAilment
  { maId :: Int,
    maName :: Text,
    maMoves :: [NamedAPIResource Move],
    maDescriptions :: [Description]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveAilment where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

instance PokeApiResource MoveAilment where
  get iden = getFromUrl $ apiv2 /: "move-ailment" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-ailment"

-- ** Move Battle Styles

data MoveBattleStyle = MoveBattleStyle
  { mbsId :: Int,
    mbsName :: Text,
    mbsNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveBattleStyle where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

instance PokeApiResource MoveBattleStyle where
  get iden = getFromUrl $ apiv2 /: "move-battle-style" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-battle-style"

-- ** Move Categories

data MoveCategory = MoveCategory
  { mcId :: Int,
    mcName :: Text,
    mcMoves :: [NamedAPIResource Move],
    mcDescriptions :: [Description]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveCategory where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

instance PokeApiResource MoveCategory where
  get iden = getFromUrl $ apiv2 /: "move-category" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-category"

-- ** Move Damage Classes

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

instance PokeApiResource MoveDamageClass where
  get iden = getFromUrl $ apiv2 /: "move-damage-class" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-damage-class"

-- ** Move Learn Methods

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

instance PokeApiResource MoveLearnMethod where
  get iden = getFromUrl $ apiv2 /: "move-learn-method" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-learn-method"

-- ** Move Targets

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

instance PokeApiResource MoveTarget where
  get iden = getFromUrl $ apiv2 /: "move-target" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "move-target"

-- * Pokemon

-- ** Abilities

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

instance PokeApiResource Ability where
  get iden = getFromUrl $ apiv2 /: "ability" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "ability"

data AbilityEffectChange = AbilityEffectChange
  { aecEffectEntries :: [Effect],
    aecVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AbilityEffectChange where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data AbilityFlavorText = AbilityFlavorText
  { aftFlavorText :: Text,
    aftLanguage :: NamedAPIResource Language,
    aftVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AbilityFlavorText where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data AbilityPokemon = AbilityPokemon
  { apIsHidden :: Bool,
    apSlot :: Int,
    apPokemon :: NamedAPIResource Pokemon
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AbilityPokemon where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Characteristics

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

instance PokeApiResource Characteristic where
  get iden = getFromUrl $ apiv2 /: "characteristic" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "characteristic"

-- ** Egg Groups

data EggGroup = EggGroup
  { eggGroupId :: Int,
    eggGroupName :: Text,
    eggGroupNames :: [Name],
    eggGroupPokemonSpecies :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EggGroup where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 8}

instance PokeApiResource EggGroup where
  get iden = getFromUrl $ apiv2 /: "egg-group" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "egg-group"

-- ** Genders

data Gender = Gender
  { genderId :: Int,
    genderName :: Text,
    genderPokemonSpeciesDetails :: [PokemonSpeciesGender],
    genderRequiredForEvolution :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Gender where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 6}

instance PokeApiResource Gender where
  get iden = getFromUrl $ apiv2 /: "gender" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "gender"

data PokemonSpeciesGender = PokemonSpeciesGender
  { psgRate :: Int,
    psgPokemonSpecies :: NamedAPIResource PokemonSpecies
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonSpeciesGender where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- ** Growth rates

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

instance PokeApiResource GrowthRate where
  get iden = getFromUrl $ apiv2 /: "growth-rate" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "growth-rate"

data GrowthRateExperienceLevel = GrowthRateExperienceLevel
  { grelLevel :: Int,
    grelExperience :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON GrowthRateExperienceLevel where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Natures

data Nature = Nature
  { natureId :: Int,
    natureName :: Text,
    natureDecreasedStat :: NamedAPIResource Stat,
    natureIncreasedStat :: NamedAPIResource Stat,
    natureHatesFlavor :: NamedAPIResource BerryFlavor,
    natureLikesFlavor :: NamedAPIResource BerryFlavor,
    naturePokeathlonStatChanges :: [NatureStatChange],
    natureMoveBattleStylePreferences :: [MoveBattleStylePreference],
    natureNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Nature where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 6}

instance PokeApiResource Nature where
  get iden = getFromUrl $ apiv2 /: "nature" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "nature"

data NatureStatChange = NatureStatChange
  { nscMaxChange :: Int,
    nscPokeathlonStat :: NamedAPIResource PokeathlonStat
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NatureStatChange where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data MoveBattleStylePreference = MoveBattleStylePreference
  { mbspLowHpPreference :: Int,
    mbspHighHpPreference :: Int,
    mbspMoveBattleStyle :: NamedAPIResource MoveBattleStyle
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveBattleStylePreference where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Pokeathlon stats

data PokeathlonStat = PokeathlonStat
  { pasId :: Int,
    pasName :: Text,
    pasNames :: [Name],
    pasAffectingNatures :: NaturePokeathlonStatAffectSets
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokeathlonStat where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

instance PokeApiResource PokeathlonStat where
  get iden = getFromUrl $ apiv2 /: "pokeathlon-stat" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokeathlon-stat"

data NaturePokeathlonStatAffectSets = NaturePokeathlonStatAffectSets
  { npsasIncrease :: [NaturePokeathlonStatAffect],
    npsasDecrease :: [NaturePokeathlonStatAffect]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NaturePokeathlonStatAffectSets where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 5}

data NaturePokeathlonStatAffect = NaturePokeathlonStatAffect
  { npsaMaxChange :: Int,
    npsaNature :: NamedAPIResource Nature
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NaturePokeathlonStatAffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Pokemon

data Pokemon = Pokemon
  { pokemonId :: Int,
    pokemonName :: Text,
    pokemonBaseExperience :: Int,
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

instance PokeApiResource Pokemon where
  get iden = getFromUrl $ apiv2 /: "pokemon" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon"

data PokemonAbility = PokemonAbility
  { paIsHidden :: Bool,
    paSlot :: Int,
    paAbility :: NamedAPIResource Ability
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonAbility where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data PokemonType = PokemonType
  { ptSlot :: Int,
    ptType :: NamedAPIResource Type
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonType where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data PokemonFormType = PokemonFormType
  { pftSlot :: Int,
    pftType :: NamedAPIResource Type
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonFormType where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data PokemonTypePast = PokemonTypePast
  { ptpGeneration :: NamedAPIResource Generation,
    ptpTypes :: [PokemonType]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonTypePast where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data PokemonHeldItem = PokemonHeldItem
  { phiItem :: NamedAPIResource Item,
    phiVersionDetails :: [PokemonHeldItemVersion]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonHeldItem where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data PokemonHeldItemVersion = PokemonHeldItemVersion
  { phivVersion :: NamedAPIResource Version,
    phivRarity :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonHeldItemVersion where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

data PokemonMove = PokemonMove
  { pmMove :: NamedAPIResource Move,
    pmVersionGroupDetails :: [PokemonMoveVersion]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonMove where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data PokemonMoveVersion = PokemonMoveVersion
  { pmvMoveLearnMethod :: NamedAPIResource MoveLearnMethod,
    pmvVersionGroup :: NamedAPIResource VersionGroup,
    pmvLevelLearnedAt :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonMoveVersion where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data PokemonStat = PokemonStat
  { psStat :: NamedAPIResource Stat,
    psEffort :: Int,
    psBaseStat :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonStat where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data PokemonSprites = PokemonSprites
  { psFrontDefault :: Text,
    -- | Not implemented for Gen 9
    psFrontShiny :: Maybe Text, -- Maybe verified
    psFrontFemale :: Maybe Text, -- Maybe verified
    psFrontShinyFemale :: Maybe Text, -- Maybe verified

    -- | Not implemented for Gen 9
    psBackDefault :: Maybe Text, -- Maybe verified

    -- | Not implemented for Gen 9
    psBackShiny :: Maybe Text, -- Maybe verified
    psBackFemale :: Maybe Text, -- Maybe verified
    psBackShinyFemale :: Maybe Text -- Maybe verified
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonSprites where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Pokemon Location Areas

newtype PokemonLocationArea = PokemonLocationArea
  {encounters :: [LocationAreaEncounter]}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonLocationArea where
  parseJSON = genericParseJSON defaultOptions

instance PokeApiResource PokemonLocationArea where
  get iden = getFromUrl $ apiv2 /: "pokemon" /: T.toLower iden /: "encounters"

  -- TODO: How do we handle this? It feels really clunky to have to separate
  -- this from the usual typeclass.
  gets lim off = error "The Pokemon Location Areas endpoint does not accept a list."

data LocationAreaEncounter = LocationAreaEncounter
  { laeLocationArea :: NamedAPIResource LocationArea,
    laeVersionDetails :: [VersionEncounterDetail]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON LocationAreaEncounter where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- ** Pokemon Colors

data PokemonColor = PokemonColor
  { pcId :: Int,
    pcName :: Text,
    pcNames :: [Name],
    pcPokemonSpecies :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonColor where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

instance PokeApiResource PokemonColor where
  get iden = getFromUrl $ apiv2 /: "pokemon-color" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon-color"

-- ** Pokemon Forms

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

instance PokeApiResource PokemonForm where
  get iden = getFromUrl $ apiv2 /: "pokemon-form" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon-form"

data PokemonFormSprites = PokemonFormSprites
  { pfsFrontDefault :: Text,
    pfsFrontShiny :: Text,
    pfsBackDefault :: Text,
    pfsBackShiny :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonFormSprites where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- ** Pokemon Habitats

data PokemonHabitat = PokemonHabitat
  { phId :: Int,
    phName :: Text,
    phNames :: [Name],
    phPokemonSpecies :: [NamedAPIResource PokemonSpecies]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonHabitat where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

instance PokeApiResource PokemonHabitat where
  get iden = getFromUrl $ apiv2 /: "pokemon-habitat" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon-habitat"

-- ** Pokemon Shapes

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

instance PokeApiResource PokemonShape where
  get iden = getFromUrl $ apiv2 /: "pokemon-shape" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon-shape"

data AwesomeName = AwesomeName
  { anAwesomeName :: Text,
    anLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AwesomeName where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

-- ** Pokemon Species

data PokemonSpecies = PokemonSpecies
  { psId :: Int,
    psName :: Text,
    psOrder :: Int,
    psGenderRate :: Int,
    psCaptureRate :: Int,
    psBaseHappiness :: Int,
    psIsBaby :: Bool,
    psIsLegendary :: Bool,
    psIsMythical :: Bool,
    psHatchCounter :: Int,
    psHasGenderDifferences :: Bool,
    psFormsSwitchable :: Bool,
    psGrowthRate :: NamedAPIResource GrowthRate,
    psPokedexNumbers :: [PokemonSpeciesDexEntry],
    psEggGroups :: [NamedAPIResource EggGroup],
    psColor :: NamedAPIResource PokemonColor,
    psShape :: Maybe (NamedAPIResource PokemonShape), -- Maybe verified
    psEvolutionChain :: APIResource EvolutionChain,
    psHabitat :: Maybe (NamedAPIResource PokemonHabitat), -- Maybe verified
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

instance PokeApiResource PokemonSpecies where
  get iden = getFromUrl $ apiv2 /: "pokemon-species" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "pokemon-species"

data Genus = Genus
  { genusGenus :: Text,
    genusLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Genus where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 5}

data PokemonSpeciesDexEntry = PokemonSpeciesDexEntry
  { psdeEntryNumber :: Int,
    psdePokedex :: NamedAPIResource Pokedex
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonSpeciesDexEntry where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

data PalParkEncounterArea = PalParkEncounterArea
  { ppeaBaseScore :: Int,
    ppeaRate :: Int,
    ppeaArea :: NamedAPIResource PalParkArea
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PalParkEncounterArea where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

data PokemonSpeciesVariety = PokemonSpeciesVariety
  { psvIsDefault :: Bool,
    psvPokemon :: NamedAPIResource Pokemon
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PokemonSpeciesVariety where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- ** Stats

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

instance PokeApiResource Stat where
  get iden = getFromUrl $ apiv2 /: "stat" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "stat"

data MoveStatAffectSets = MoveStatAffectSets
  { msasIncrease :: [MoveStatAffect],
    msasDecrease :: [MoveStatAffect]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveStatAffectSets where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

data MoveStatAffect = MoveStatAffect
  { msaChange :: Int,
    msaMove :: NamedAPIResource Move
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MoveStatAffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data NatureStatAffectSets = NatureStatAffectSets
  { nsasIncrease :: [NamedAPIResource Nature],
    nsasDecrease :: [NamedAPIResource Nature]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NatureStatAffectSets where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Types

data Type = Type
  { typeId :: Int,
    typeName :: Text,
    typeDamageRelations :: TypeRelations,
    typePastDamageRelations :: [TypeRelationsPast],
    typeGameIndices :: [GenerationGameIndex],
    typeGeneration :: NamedAPIResource Generation,
    typeMoveDamageClass :: NamedAPIResource MoveDamageClass,
    typeNames :: [Name],
    typePokemon :: [TypePokemon],
    typeMoves :: [NamedAPIResource Move]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Type where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

instance PokeApiResource Type where
  get iden = getFromUrl $ apiv2 /: "type" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "type"

data TypePokemon = TypePokemon
  { tpSlot :: Int,
    tpPokemon :: NamedAPIResource Pokemon
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TypePokemon where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

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

data TypeRelationsPast = TypeRelationsPast
  { trpGeneration :: NamedAPIResource Generation,
    trpDamageRelations :: TypeRelations
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TypeRelationsPast where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

-- * Utility

-- ** Languages

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

instance PokeApiResource Language where
  get iden = getFromUrl $ apiv2 /: "language" /: T.toLower iden
  gets lim off = getsFromUrl lim off $ apiv2 /: "language"

-- ** (Named-)APIResources

data NamedAPIResource a = NamedAPIResource
  { narName :: Text,
    narUrl :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (NamedAPIResource a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

newtype APIResource a = APIResource
  { arUrl :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (APIResource a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

class APIResourceType t where
  apiResourceUrl :: t a -> Text

  resolve :: (PokeApiResource a) => t a -> IO a
  resolve = getFromUrl' . apiResourceUrl

instance APIResourceType NamedAPIResource where
  apiResourceUrl = narUrl

instance APIResourceType APIResource where
  apiResourceUrl = arUrl

data NamedAPIResourceList a = NamedAPIResourceList
  { narlCount :: Int,
    narlNext :: Maybe Text,
    narlPrevious :: Maybe Text,
    narlResults :: [NamedAPIResource a]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (NamedAPIResourceList a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

-- ** Common Models

data Description = Description
  { descriptionDescription :: Text,
    descriptionLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Description where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 11}

data Effect = Effect
  { effectEffect :: Text,
    effectLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Effect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 6}

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

data FlavorText = FlavorText
  { ftFlavorText :: Text,
    ftLanguage :: NamedAPIResource Language,
    ftVersion :: NamedAPIResource Version
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON FlavorText where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data GenerationGameIndex = GenerationGameIndex
  { ggiGameIndex :: Int,
    ggiGeneration :: NamedAPIResource Generation
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON GenerationGameIndex where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data MachineVersionDetail = MachineVersionDetail
  { mvdMachine :: APIResource Machine,
    mvdVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MachineVersionDetail where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data Name = Name
  { nameName :: Text,
    nameLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Name where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

data VerboseEffect = VerboseEffect
  { veEffect :: Text,
    veShortEffect :: Text,
    veLanguage :: NamedAPIResource Language
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON VerboseEffect where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 2}

data VersionEncounterDetail = VersionEncounterDetail
  { vedVersion :: NamedAPIResource Version,
    vedMaxChance :: Int,
    vedEncounterDetails :: [Encounter]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON VersionEncounterDetail where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data VersionGameIndex = VersionGameIndex
  { vgiGameIndex :: Int,
    vgiVersion :: NamedAPIResource Version
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON VersionGameIndex where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 3}

data VersionGroupFlavorText = VersionGroupFlavorText
  { vgftText :: Text,
    vgftLanguage :: NamedAPIResource Language,
    vgftVersionGroup :: NamedAPIResource VersionGroup
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON VersionGroupFlavorText where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}
