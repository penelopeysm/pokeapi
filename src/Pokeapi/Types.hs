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
-- a wrapped 'PokeJsonException'.
decodeBody' :: (FromJSON a) => LBS.ByteString -> IO a
decodeBody' body = case eitherDecode body of
  Left err -> throwIO $ PokeJsonException $ T.pack err <> lbsToText body
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
    decodeBody' body

  -- | Retrieve an instance of the resource from the identifier (either a
  -- numeric ID or a name).
  get :: Text -> IO a

-- * TODO Berries

data BerryFlavor

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

-- * TODO Games

data Generation

data Pokedex

data Version

data VersionGroup

-- * TODO Items

data Item

-- * TODO Locations

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

-- ** Location Areas

data LocationArea

-- ** Pal Park Areas

data PalParkArea

-- ** Regions

data Region

-- * TODO Machines

data Machine

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
    moveContestCombos :: Maybe ContestComboSets, -- Maybe unverified
    moveContestType :: Maybe (NamedAPIResource ContestType), -- Maybe unverified
    moveContestEffect :: Maybe (APIResource ContestEffect), -- Maybe unverified
    moveDamageClass :: Maybe (NamedAPIResource MoveDamageClass), -- Maybe unverified
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
    moveSuperContestEffect :: Maybe (APIResource SuperContestEffect), -- Maybe unverified
    moveTarget :: NamedAPIResource MoveTarget,
    moveType :: NamedAPIResource Type
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Move where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 4}

instance PokeApiResource Move where
  get iden = getFromUrl $ apiv2 /: "move" /: T.toLower iden

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
    pmsvEffectChance :: Maybe Int, -- Maybe unverified
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
    natureDecreasedStat :: Maybe (NamedAPIResource Stat), -- Maybe unverified
    natureIncreasedStat :: Maybe (NamedAPIResource Stat), -- Maybe unverified
    natureHatesFlavor :: Maybe (NamedAPIResource BerryFlavor), -- Maybe unverified
    natureLikesFlavor :: Maybe (NamedAPIResource BerryFlavor), -- Maybe unverified
    naturePokeathlonStatChanges :: [NatureStatChange],
    natureMoveBattleStylePreferences :: [MoveBattleStylePreference],
    natureNames :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Nature where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = flm 6}

instance PokeApiResource Nature where
  get iden = getFromUrl $ apiv2 /: "nature" /: T.toLower iden

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
    pokemonBaseExperience :: Maybe Int, -- Maybe unverified
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

-- ** TODO Pokemon Location Areas

-- ** TODO Pokemon Colors

data PokemonColor

-- ** TODO Pokemon Forms

data PokemonForm

-- ** TODO Pokemon Habitats

data PokemonHabitat

-- ** TODO Pokemon Shapes

data PokemonShape

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
    psHabitat :: Maybe (NamedAPIResource PokemonHabitat),  -- Maybe verified
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

-- ** TODO Stats

data Stat

-- ** TODO Types

data Type

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
