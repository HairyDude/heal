module EveApi.Types
    ( APIDataType (..)
    , typeString    -- :: IsString s => APIDataType -> s
    , CharacterID
    , Optional (..)
    , WalletDivision (..)
    , APIArgument (..)
    , APIArgumentType (..)
    , matchArgSpec  -- :: APIArgumentType -> APIArgument -> Bool
    , sameArgType   -- :: APIArgument -> APIArgument -> Bool
    , argString     -- :: IsString s => APIArgumentType -> s
    , Key (..)
    , KeyScope (..)
    , CharKey (..)
    , KeyType (..)
    , Arg (..)
    , Scope (..)
    , scopeString   -- :: IsString s => Scope -> s
    , stringScope   -- :: (Eq s, IsString s) => s => Maybe Scope
    , CallParams (..)
    , FullCall (..)
    , Call (..)
    , InvFlag   -- don't export "unused" InvFlags
        ( None, Wallet, Factory, Hangar, Cargo, Briefcase, Skill, Reward
        , Connected, Disconnected, LoSlot0, LoSlot1, LoSlot2, LoSlot3, LoSlot4
        , LoSlot5, LoSlot6, LoSlot7, MedSlot0, MedSlot1, MedSlot2, MedSlot3
        , MedSlot4, MedSlot5, MedSlot6, MedSlot7, HiSlot0, HiSlot1, HiSlot2
        , HiSlot3, HiSlot4, HiSlot5, HiSlot6, HiSlot7, FixedSlot
        , PromenadeSlot1, PromenadeSlot2, PromenadeSlot3, PromenadeSlot4
        , PromenadeSlot5, PromenadeSlot6, PromenadeSlot7, PromenadeSlot8
        , PromenadeSlot9, PromenadeSlot10, PromenadeSlot11, PromenadeSlot12
        , PromenadeSlot13, PromenadeSlot14, PromenadeSlot15, PromenadeSlot16
        , Capsule, Pilot, Passenger, BoardingGate, Crew, FSkillInTraining
        , CorpMarket, Locked, Unlocked, OfficeSlot1, OfficeSlot2, OfficeSlot3
        , OfficeSlot4, OfficeSlot5, OfficeSlot6, OfficeSlot7, OfficeSlot8
        , OfficeSlot9, OfficeSlot10, OfficeSlot11, OfficeSlot12, OfficeSlot13
        , OfficeSlot14, OfficeSlot15, OfficeSlot16, Bonus, DroneBay, Booster
        , Implant, ShipHangar, ShipOffline, RigSlot0, RigSlot1, RigSlot2
        , RigSlot3, RigSlot4, RigSlot5, RigSlot6, RigSlot7, FactoryOperation
        , CorpSAG2, CorpSAG3, CorpSAG4, CorpSAG5, CorpSAG6, CorpSAG7
        , SecondaryStorage, CaptainsQuarters, WisPromenade, SubsystemSlot0
        , SubsystemSlot1, SubsystemSlot2, SubsystemSlot3, SubsystemSlot4
        , SubsystemSlot5, SubsystemSlot6, SubsystemSlot7, SpecializedFuelBay
        , SpecializedOreHold, SpecializedGasHold, SpecializedMineralHold
        , SpecializedSalvageHold, SpecializedShipHold, SpecializedSmallShipHold
        , SpecializedMediumShipHold, SpecializedLargeShipHold
        , SpecializedIndustrialShipHold, SpecializedAmmoHold, StructureActive
        , StructureInactive, JunkyardReprocessed, JunkyardTrashed
        )
    , loSlot, medSlot, hiSlot, rigSlot, subsystemSlot, corpSAG, officeSlot
            -- :: InvFlag -> Maybe Int
    )
where

import Prelude
    (   -- classes
        Eq (..), Ord (..), Show (..), Read (..), Enum (..), Bounded (..), Bool (..)
        -- types
    ,   Int, Integer, Bool, Maybe (..), Either (..)
        -- operators
    ,   ($), (++), (.)
        -- functions
    ,   reads
    )

import Data.Text (Text)
import Data.String

data APIDataType = AInteger -- any integer type, distinctions are unimportant
                 | ABigint  -- (except bigints)
                 | ADecimal -- e.g. isk balance/prices, standings
                 | ABool
                 | AChar64  -- usually vCode
                 | AString  -- any string other than Char64
                 | ADateString
                 | AIntList
                 | AStringList
    deriving (Eq, Ord, Show, Read)

typeString :: IsString s => APIDataType -> s
typeString AInteger    = "int"
typeString ABigint     = "bigint"
typeString ADecimal    = "decimal"
typeString ABool       = "bool"
typeString AChar64     = "char(64)"
typeString AString     = "string"
typeString ADateString = "date"
typeString AIntList    = "intlist"
typeString AStringList = "stringlist"

type CharacterID = Int
data Optional = Optional
              | FromKey -- ID needed, but can be inferred from a key.
                        -- Required for all character-scope calls when using
                        -- multi-character keys.
                        -- Do not use ID if not needed. E.g. CorporationSheet
                        -- ignores the key if a corporationID is provideed.
              | Required
    deriving (Eq, Ord, Show, Read, Enum)

newtype WalletDivision = WalletDivision Int deriving (Eq, Ord, Show, Read)
                                    -- range: 1000 - 1006
data APIArgument
    = ArgIDs            [Integer]
    | ArgVersion        Integer -- in practice, always 1 (for AllianceList)
    | ArgNames          [Text]
    | ArgCharacterID    Integer
    | ArgCorporationID  Integer
    | ArgEventIDs       [Integer]
    | ArgContractID     Integer
    | ArgBeforeKillID   Integer
    | ArgOrderID        Integer
    | ArgExtended       Bool    -- for corp/MemberTracking
    | ArgRowCount       Integer
    | ArgFromID         Integer
    | ArgAccountKey     WalletDivision
    | ArgItemID         Integer
    deriving (Eq, Ord, Show)
data APIArgumentType
    = ArgTIDs
    | ArgTVersion
    | ArgTNames
    | ArgTCharacterID
    | ArgTCorporationID
    | ArgTEventIDs
    | ArgTContractID
    | ArgTBeforeKillID
    | ArgTOrderID
    | ArgTExtended
    | ArgTRowCount
    | ArgTFromID
    | ArgTAccountKey
    | ArgTItemID
    deriving (Eq, Ord, Show, Read)

matchArgSpec :: APIArgumentType -> APIArgument -> Bool
matchArgSpec ArgTIDs           (ArgIDs _)           = True
matchArgSpec ArgTVersion       (ArgVersion _)       = True
matchArgSpec ArgTNames         (ArgNames _)         = True
matchArgSpec ArgTCharacterID   (ArgCharacterID _)   = True
matchArgSpec ArgTCorporationID (ArgCorporationID _) = True
matchArgSpec ArgTEventIDs      (ArgEventIDs _)      = True
matchArgSpec ArgTContractID    (ArgContractID _)    = True
matchArgSpec ArgTBeforeKillID  (ArgBeforeKillID _)  = True
matchArgSpec ArgTOrderID       (ArgOrderID _)       = True
matchArgSpec ArgTExtended      (ArgExtended _)      = True
matchArgSpec ArgTRowCount      (ArgRowCount _)      = True
matchArgSpec ArgTFromID        (ArgFromID _)        = True
matchArgSpec ArgTAccountKey    (ArgAccountKey _)    = True
matchArgSpec ArgTItemID        (ArgItemID _)        = True
matchArgSpec _                 _                    = False

sameArgType :: APIArgument -> APIArgument -> Bool
sameArgType (ArgIDs           _) (ArgIDs           _) = True
sameArgType (ArgVersion       _) (ArgVersion       _) = True
sameArgType (ArgNames         _) (ArgNames         _) = True
sameArgType (ArgCharacterID   _) (ArgCharacterID   _) = True
sameArgType (ArgCorporationID _) (ArgCorporationID _) = True
sameArgType (ArgEventIDs      _) (ArgEventIDs      _) = True
sameArgType (ArgContractID    _) (ArgContractID    _) = True
sameArgType (ArgBeforeKillID  _) (ArgBeforeKillID  _) = True
sameArgType (ArgOrderID       _) (ArgOrderID       _) = True
sameArgType (ArgExtended      _) (ArgExtended      _) = True
sameArgType (ArgRowCount      _) (ArgRowCount      _) = True
sameArgType (ArgFromID        _) (ArgFromID        _) = True
sameArgType (ArgAccountKey    _) (ArgAccountKey    _) = True
sameArgType (ArgItemID        _) (ArgItemID        _) = True
sameArgType _                    _                    = False

argString :: IsString s => APIArgumentType -> s
argString ArgTIDs           = "IDs"
argString ArgTVersion       = "version"
argString ArgTNames         = "names"
argString ArgTCharacterID   = "characterID"
argString ArgTCorporationID = "corporationID"
argString ArgTEventIDs      = "eventIDs"
argString ArgTContractID    = "contractID"
argString ArgTBeforeKillID  = "beforeKillID"
argString ArgTOrderID       = "orderID"
argString ArgTExtended      = "extended"
argString ArgTRowCount      = "rowCount"
argString ArgTFromID        = "fromID"
argString ArgTAccountKey    = "accountKey"
argString ArgTItemID        = "itemID"

-- There are character keys and corporation keys.
-- Corp keys are tied to a character, who only has one corp, so corporationID
-- arguments are unnecessary for corp calls.
-- Char keys may cover one character, in which case characterID is unnecessary
-- for char calls, or they may cover multiple characters, in which case they're
-- necessary. We don't specify characterID in the list of calls unless it can
-- be used without auth (i.e. regarding someone else).
-- In all cases a key has an access mask which indicates what access it
-- provides.
-- Legacy API keys are not supported.
-- The following algebraic data types encode these requirements.
data Key = Key { keyID :: Int
               , vCode :: Text
               , keyScope :: KeyScope }
         | KeyNone
    deriving (Eq, Ord, Show)
data KeyScope = UnknownKeyScope
              | CharKeyScope CharKey
              | CorpKeyScope
    deriving (Eq, Ord, Show)
data CharKey = SingleCK
             | MultiCK CharacterID
    deriving (Eq, Ord, Show)

-- For specifying API calls.
data KeyType = NoKey
             | AnyKey  { keyOpt :: Optional } -- e.g. api/APIKeyInfo
             | CorpKey { keyOpt :: Optional }
             | CharKey { keyOpt :: Optional }
    deriving (Eq, Ord, Show, Read)

-- Arguments other than keys (and characterIDs that are implied by a key).
data Arg = Arg { argName :: APIArgumentType
               , argType :: APIDataType
               , argOptional :: Optional }
    deriving (Eq, Ord, Show, Read)

data Scope = AcctScope
           | APIScope
           | CharScope
           | CorpScope
           | EVEScope
           | MapScope
           | ServerScope
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

scopeString :: IsString s => Scope -> s
scopeString AcctScope   = "account"
scopeString APIScope    = "api"
scopeString CharScope   = "char"
scopeString CorpScope   = "corp"
scopeString EVEScope    = "eve"
scopeString MapScope    = "map"
scopeString ServerScope = "server"

stringScope :: (Eq s, IsString s) => s -> Maybe Scope
stringScope "account" = Just AcctScope
stringScope "api"     = Just APIScope
stringScope "char"    = Just CharScope
stringScope "corp"    = Just CorpScope
stringScope "eve"     = Just EVEScope
stringScope "map"     = Just MapScope
stringScope "server"  = Just ServerScope
stringScope _         = Nothing

data CallParams = CallParams
        { callKey  :: KeyType
        , callArgs     :: [Arg] }
    deriving (Show, Eq, Ord)

data FullCall = FullCall Scope Call (Maybe Key) [APIArgument]
    deriving (Show, Eq, Ord)

-- Calls.
-- The constructor should have the same name as the call.
-- If the call name isn't a valid Haskell data constructor, it needs
-- special-casing (as e.g. calllist).
data Call
    = CallList
    | AccountBalance | AccountStatus | AllianceList | APIKeyInfo | AssetList
    | CalendarEventAttendees | CertificateTree | CharacterID | CharacterName
    | CharacterInfo | CharacterSheet | Characters | ConquerableStationList
    | ContactList | ContactNotifications | ContainerLog | ContractBids
    | ContractItems | Contracts | CorporationSheet | ErrorList | FacWarStats
    | FacWarTopStats | FacWarSystems | IndustryJobs | Jumps | Kills | KillLog
    | Locations | MailBodies | MailingLists | MailMessages | MarketOrders
    | Medals | MemberMedals | MemberSecurity | MemberSecurityLog
    | MemberTracking | Notifications | NotificationTexts | OutpostList
    | OutpostServiceDetail | RefTypes | Research | ServerStatus | Shareholders
    | SkillInTraining | SkillQueue | SkillTree | Sovereignty | Standings
    | StarbaseDetail | StarbaseList | TypeName | Titles | UpcomingCalendarEvents
    | WalletJournal | WalletTransactions
    deriving (Show, Eq, Ord, Read, Enum, Bounded)

--------------------------------------------------------------------------------

-- Inventory flags.
-- Unused flags are represented with "InvFlagN" and are present so that derived
-- Enum methods work properly.
data InvFlag = None
             | Wallet
             | Factory | InvFlag3 | Hangar | Cargo
             | Briefcase
             | Skill
             | Reward
             | Connected | Disconnected
             | LoSlot0 | LoSlot1 | LoSlot2 | LoSlot3
             | LoSlot4 | LoSlot5 | LoSlot6 | LoSlot7
             | MedSlot0 | MedSlot1 | MedSlot2 | MedSlot3
             | MedSlot4 | MedSlot5 | MedSlot6 | MedSlot7
             | HiSlot0 | HiSlot1 | HiSlot2 | HiSlot3
             | HiSlot4 | HiSlot5 | HiSlot6 | HiSlot7
             | FixedSlot
             | InvFlag36 | InvFlag37 | InvFlag38 | InvFlag39
             | PromenadeSlot1 | PromenadeSlot2 | PromenadeSlot3
             | PromenadeSlot4 | PromenadeSlot5 | PromenadeSlot6
             | PromenadeSlot7 | PromenadeSlot8 | PromenadeSlot9
             | PromenadeSlot10 | PromenadeSlot11 | PromenadeSlot12
             | PromenadeSlot13 | PromenadeSlot14 | PromenadeSlot15
             | PromenadeSlot16
             | Capsule | Pilot | Passenger | BoardingGate | Crew
             | FSkillInTraining -- clashes with the Call
             | CorpMarket
             | Locked | Unlocked
             | InvFlag65 | InvFlag66 | InvFlag67 | InvFlag68 | InvFlag69
             | OfficeSlot1 | OfficeSlot2 | OfficeSlot3 | OfficeSlot4
             | OfficeSlot5 | OfficeSlot6 | OfficeSlot7 | OfficeSlot8
             | OfficeSlot9 | OfficeSlot10 | OfficeSlot11 | OfficeSlot12
             | OfficeSlot13 | OfficeSlot14 | OfficeSlot15 | OfficeSlot16
             | Bonus
             | DroneBay
             | Booster | Implant
             | ShipHangar | ShipOffline
             | RigSlot0 | RigSlot1 | RigSlot2 | RigSlot3
             | RigSlot4 | RigSlot5 | RigSlot6 | RigSlot7
             | FactoryOperation
             | InvFlag101 | InvFlag102 | InvFlag103 | InvFlag104 | InvFlag105
             | InvFlag106 | InvFlag107 | InvFlag108 | InvFlag109 | InvFlag110
             | InvFlag111 | InvFlag112 | InvFlag113 | InvFlag114 | InvFlag115
             | CorpSAG2 | CorpSAG3 | CorpSAG4
             | CorpSAG5 | CorpSAG6 | CorpSAG7
             | SecondaryStorage
             | CaptainsQuarters
             | WisPromenade
             | SubsystemSlot0 | SubsystemSlot1 | SubsystemSlot2
             | SubsystemSlot3 | SubsystemSlot4 | SubsystemSlot5
             | SubsystemSlot6 | SubsystemSlot7
             | SpecializedFuelBay | SpecializedOreHold | SpecializedGasHold
             | SpecializedMineralHold | SpecializedSalvageHold
             | SpecializedShipHold | SpecializedSmallShipHold
             | SpecializedMediumShipHold | SpecializedLargeShipHold
             | SpecializedIndustrialShipHold | SpecializedAmmoHold
             | StructureActive | StructureInactive
             | JunkyardReprocessed | JunkyardTrashed
    deriving (Eq, Ord, Show, Enum, Bounded)

-- Extractor functions for easier pattern matching
-- (use pattern guards: | Just n <- loSlot foo = ...)
loSlot :: InvFlag -> Maybe Int
loSlot LoSlot0 = Just 0
loSlot LoSlot1 = Just 1
loSlot LoSlot2 = Just 2
loSlot LoSlot3 = Just 3
loSlot LoSlot4 = Just 4
loSlot LoSlot5 = Just 5
loSlot LoSlot6 = Just 6
loSlot LoSlot7 = Just 7
loSlot _       = Nothing

medSlot :: InvFlag -> Maybe Int
medSlot MedSlot0 = Just 0
medSlot MedSlot1 = Just 1
medSlot MedSlot2 = Just 2
medSlot MedSlot3 = Just 3
medSlot MedSlot4 = Just 4
medSlot MedSlot5 = Just 5
medSlot MedSlot6 = Just 6
medSlot MedSlot7 = Just 7
medSlot _        = Nothing

hiSlot :: InvFlag -> Maybe Int
hiSlot HiSlot0 = Just 0
hiSlot HiSlot1 = Just 1
hiSlot HiSlot2 = Just 2
hiSlot HiSlot3 = Just 3
hiSlot HiSlot4 = Just 4
hiSlot HiSlot5 = Just 5
hiSlot HiSlot6 = Just 6
hiSlot HiSlot7 = Just 7
hiSlot _       = Nothing

rigSlot :: InvFlag -> Maybe Int
rigSlot RigSlot0 = Just 0
rigSlot RigSlot1 = Just 1
rigSlot RigSlot2 = Just 2
rigSlot RigSlot3 = Just 3
rigSlot RigSlot4 = Just 4
rigSlot RigSlot5 = Just 5
rigSlot RigSlot6 = Just 6
rigSlot RigSlot7 = Just 7
rigSlot _        = Nothing

subsystemSlot :: InvFlag -> Maybe Int
subsystemSlot SubsystemSlot0 = Just 0
subsystemSlot SubsystemSlot1 = Just 1
subsystemSlot SubsystemSlot2 = Just 2
subsystemSlot SubsystemSlot3 = Just 3
subsystemSlot SubsystemSlot4 = Just 4
subsystemSlot SubsystemSlot5 = Just 5
subsystemSlot SubsystemSlot6 = Just 6
subsystemSlot SubsystemSlot7 = Just 7
subsystemSlot _              = Nothing

-- Dunno what these are, but it seems worth defining this
corpSAG :: InvFlag -> Maybe Int
corpSAG CorpSAG2 = Just 2
corpSAG CorpSAG3 = Just 3
corpSAG CorpSAG4 = Just 4
corpSAG CorpSAG5 = Just 5
corpSAG CorpSAG6 = Just 6
corpSAG CorpSAG7 = Just 7
corpSAG _              = Nothing

officeSlot :: InvFlag -> Maybe Int
officeSlot OfficeSlot1  = Just 1
officeSlot OfficeSlot2  = Just 2
officeSlot OfficeSlot3  = Just 3
officeSlot OfficeSlot4  = Just 4
officeSlot OfficeSlot5  = Just 5
officeSlot OfficeSlot6  = Just 6
officeSlot OfficeSlot7  = Just 7
officeSlot OfficeSlot8  = Just 8
officeSlot OfficeSlot9  = Just 9
officeSlot OfficeSlot10 = Just 10
officeSlot OfficeSlot11 = Just 11
officeSlot OfficeSlot12 = Just 12
officeSlot OfficeSlot13 = Just 13
officeSlot OfficeSlot14 = Just 14
officeSlot OfficeSlot15 = Just 15
officeSlot OfficeSlot16 = Just 16
officeSlot _            = Nothing
