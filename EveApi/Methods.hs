{-# LANGUAGE StandaloneDeriving, OverloadedStrings, FlexibleInstances #-}
module EveApi.Methods (
        ApiResult (..)
    ,   CallResult
    ,   doCall
    )
    where

import Prelude -- remove this when splitting into another library

import EveApi.Types
import EveApi.Errors
import EveApi.Orphans ()

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

import Data.ByteString.Lazy (toChunks)
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Text.Encoding
import Data.List (intersperse)
import Network.HTTP.Types
import Network.HTTP.Conduit
import Text.XML

data ApiResult = Success
                    Text        -- ^ raw response xml
--                    Document    -- ^ response DOM tree
               | Error EveApiError
               | UserError Text
               | ServerError Text
               | NotImplemented
               | NotRecognised
    deriving (Show, Eq, Ord)
type CallResult m = ResourceT (ReaderT Manager (ExceptionT m)) ApiResult

-- This supposedly fails the coverage condition, though it should hold transitively
-- TODO: determine whether UndecidableInstances is harmless, and if so, add it
--instance MonadReader r m => MonadReader r (ResourceT m) where
--    ask = lift ask

---------------
-- API scope --
---------------
callList :: (MonadIO m, MonadUnsafeIO m, MonadBaseControl IO m) => CallResult m
callList =
    case callBaseURL APIScope CallList of
        Left  exc -> lift $ monadThrow exc
        Right req -> do
            Response s@(Status statuscode _) _ headers body <-
                        httpLbs req =<< lift ask
            case statuscode of
                200 -> return . Success . decodeUtf8 $
                            B.concat . toChunks $ body
                    -- TODO: parse and determine if the API returned an error
                _   -> lift . monadThrow $
                            StatusCodeException s headers

---------------
-- EVE scope --
---------------

-- Static
certificateTree, errorList, refTypes, skillTree :: Monad m => CallResult m
certificateTree = return NotImplemented
errorList = return NotImplemented
refTypes = return NotImplemented
skillTree = return NotImplemented

-- Dynamic, no args
conquerableStationList, facWarStats, facWarTopStats :: Monad m => CallResult m
conquerableStationList = return NotImplemented
facWarStats = return NotImplemented
facWarTopStats = return NotImplemented

-- Dynamic, args required
typeName, characterName :: Monad m => [Integer] -> CallResult m
characterInfo :: Monad m => Text -> CallResult m
allianceList :: Monad m => Bool -> CallResult m
characterID :: Monad m => [Text] -> CallResult m
typeName _ = return NotImplemented
characterName _ = return NotImplemented
characterInfo _ = return NotImplemented
allianceList _ = return NotImplemented
characterID _ = return NotImplemented

---------------
-- Map scope --
---------------
facWarSystems, jumps, kills, sovereignty :: Monad m => CallResult m
facWarSystems = return NotImplemented
jumps = return NotImplemented
kills = return NotImplemented
sovereignty = return NotImplemented

------------------
-- Server scope --
------------------
serverStatus :: Monad m => CallResult m
serverStatus = return NotImplemented

-------------------
-- Account scope --
-------------------
apiKeyInfo, accountStatus, characters :: Monad m => Key -> CallResult m
apiKeyInfo _ = return NotImplemented
accountStatus _ = return NotImplemented
characters _ = return NotImplemented

----------------
-- Corp scope --
----------------

-- Public info if no key
corporationSheet :: Monad m => Either Key Integer -> CallResult m
corporationSheet _ = return NotImplemented

-- No args
corpAccountBalance, corpAssetList, corpContactList :: Monad m => Key -> CallResult m
containerLog, corpContractBids :: Monad m => Key -> CallResult m
corpFacWarStats, corpIndustryJobs, corpMedals :: Monad m => Key -> CallResult m
memberMedals, memberSecurity :: Monad m => Key -> CallResult m
memberSecurityLog, outpostList :: Monad m => Key -> CallResult m
shareholders, corpStandings, starbaseList :: Monad m => Key -> CallResult m
titles :: Monad m => Key -> CallResult m
corpAccountBalance _ = return NotImplemented
corpAssetList _ = return NotImplemented
corpContactList _ = return NotImplemented
containerLog _ = return NotImplemented
corpContractBids _ = return NotImplemented
corpFacWarStats _ = return NotImplemented
corpIndustryJobs _ = return NotImplemented
corpMedals _ = return NotImplemented
memberMedals _ = return NotImplemented
memberSecurity _ = return NotImplemented
memberSecurityLog _ = return NotImplemented
outpostList _ = return NotImplemented
shareholders _ = return NotImplemented
corpStandings _ = return NotImplemented
starbaseList _ = return NotImplemented
titles _ = return NotImplemented

-- One arg
corpLocations      :: Monad m => Key ->       [Integer] -> CallResult m
corpContracts      :: Monad m => Key ->  Maybe Integer  -> CallResult m
corpContractItems  :: Monad m => Key ->        Integer  -> CallResult m
corpKillLog        :: Monad m => Key ->  Maybe Integer  -> CallResult m
corpMarketOrders   :: Monad m => Key ->  Maybe Integer  -> CallResult m
memberTracking :: Monad m => Key ->        Bool     -> CallResult m
starbaseDetail :: Monad m => Key ->        Integer  -> CallResult m
outpostServiceDetail
                   :: Monad m => Key ->        Integer  -> CallResult m
corpLocations _ _ = return NotImplemented
corpContracts _ _ = return NotImplemented
corpContractItems _ _ = return NotImplemented
corpKillLog _ _ = return NotImplemented
corpMarketOrders _ _ = return NotImplemented
memberTracking _ _ = return NotImplemented
starbaseDetail _ _ = return NotImplemented
outpostServiceDetail _ _ = return NotImplemented

-- Up to 3 args
corpWalletJournal
    :: Monad m => Key -> Maybe Int -> Maybe Int -> Maybe Int -> CallResult m
corpWalletTransactions
    :: Monad m => Key -> Maybe Int -> Maybe Int -> Maybe Int -> CallResult m

corpWalletJournal _ _ _ _ = return NotImplemented
corpWalletTransactions _ _ _ _ = return NotImplemented

----------------
-- Char scope --
----------------

-- One arg, namely character ID for multi-character keys
charAccountBalance     :: Monad m => Key -> Maybe Integer -> CallResult m
charAssetList          :: Monad m => Key -> Maybe Integer -> CallResult m
characterSheet         :: Monad m => Key -> Maybe Integer -> CallResult m
charContactList        :: Monad m => Key -> Maybe Integer -> CallResult m
contactNotifications   :: Monad m => Key -> Maybe Integer -> CallResult m
charContractBids       :: Monad m => Key -> Maybe Integer -> CallResult m
charFacWarStats        :: Monad m => Key -> Maybe Integer -> CallResult m
charIndustryJobs       :: Monad m => Key -> Maybe Integer -> CallResult m
mailingLists           :: Monad m => Key -> Maybe Integer -> CallResult m
mailMessages           :: Monad m => Key -> Maybe Integer -> CallResult m
charMedals             :: Monad m => Key -> Maybe Integer -> CallResult m
notifications          :: Monad m => Key -> Maybe Integer -> CallResult m
research               :: Monad m => Key -> Maybe Integer -> CallResult m
skillInTraining        :: Monad m => Key -> Maybe Integer -> CallResult m
skillQueue             :: Monad m => Key -> Maybe Integer -> CallResult m
charStandings          :: Monad m => Key -> Maybe Integer -> CallResult m
upcomingCalendarEvents :: Monad m => Key -> Maybe Integer -> CallResult m
charAccountBalance _ _ = return NotImplemented
charAssetList _ _ = return NotImplemented
characterSheet _ _ = return NotImplemented
charContactList _ _ = return NotImplemented
contactNotifications _ _ = return NotImplemented
charContractBids _ _ = return NotImplemented
charFacWarStats _ _ = return NotImplemented
charIndustryJobs _ _ = return NotImplemented
mailingLists _ _ = return NotImplemented
mailMessages _ _ = return NotImplemented
charMedals _ _ = return NotImplemented
notifications _ _ = return NotImplemented
research _ _ = return NotImplemented
skillInTraining _ _ = return NotImplemented
skillQueue _ _ = return NotImplemented
charStandings _ _ = return NotImplemented
upcomingCalendarEvents _ _ = return NotImplemented

-- 2 args, first being character ID blah blah
charContractItems      :: Monad m => Key -> Integer -> Integer -> CallResult m
charContracts          :: Monad m => Key -> Integer -> Maybe Integer -> CallResult m
charMarketOrders       :: Monad m => Key -> Integer -> Maybe Integer -> CallResult m
charKillLog            :: Monad m => Key -> Integer -> Maybe Integer -> CallResult m
mailBodies             :: Monad m => Key -> Integer -> [Integer] -> CallResult m
notificationTexts      :: Monad m => Key -> Integer -> [Integer] -> CallResult m
calendarEventAttendees :: Monad m => Key -> Integer -> [Integer] -> CallResult m
charLocations          :: Monad m => Key -> Integer -> [Integer] -> CallResult m
charContractItems _ _ _ = return NotImplemented
charContracts _ _ _ = return NotImplemented
charMarketOrders _ _ _ = return NotImplemented
charKillLog _ _ _ = return NotImplemented
mailBodies _ _ _ = return NotImplemented
notificationTexts _ _ _ = return NotImplemented
calendarEventAttendees _ _ _ = return NotImplemented
charLocations _ _ _ = return NotImplemented

--------------------------------------
-- Convert args to query parameters --
--------------------------------------

listify :: (a -> ByteString) -> [a] -> ByteString
listify sh xs = B.concat . intersperse "," . map sh $ xs

argToParam :: APIArgument -> (ByteString, ByteString)
argToParam (ArgIDs           ids)        = ("IDs", listify (BU.fromString . show) ids)
argToParam (ArgVersion       version)    = ("version", BU.fromString $ show version)
argToParam (ArgNames         names)      = ("names", listify encodeUtf8 names)
argToParam (ArgCharacterID   charid)     = ("characterID", BU.fromString $
                                                                show charid)
argToParam (ArgCorporationID corpid)     = ("corporationID", BU.fromString $ 
                                                                show corpid)
argToParam (ArgEventIDs      eventids)   = ("eventIDs", listify (BU.fromString . show)
                                                                eventids)
argToParam (ArgContractID    contractid) = ("contractID", BU.fromString $
                                                                show contractid)
argToParam (ArgBeforeKillID  bkid)       = ("beforeKillID", BU.fromString $
                                                                show bkid)
argToParam (ArgOrderID       orderid)    = ("orderID", BU.fromString $
                                                                show orderid)
argToParam (ArgExtended      extended)   = ("extended", if extended then "1" else "0")
argToParam (ArgRowCount      rowcount)   = ("rowCount", BU.fromString $
                                                                show rowcount)
argToParam (ArgFromID        fromid)     = ("fromID", BU.fromString $
                                                                show fromid)
argToParam (ArgAccountKey (WalletDivision division)) = ("accountKey", BU.fromString $
                                                                show division)
argToParam (ArgItemID        itemid)     = ("itemID", BU.fromString $
                                                                show itemid)

-------------------
-- Make the call --
-------------------

doCall :: (MonadIO m, MonadUnsafeIO m, MonadThrow m,
           MonadBaseControl IO m) =>
    Scope -> Call -> Maybe Key -> [APIArgument] ->
             ReaderT Manager (ExceptionT m) ApiResult
doCall scope call mKey mArgs = do
    runResourceT $ case (scope, call) of
        (APIScope, CallList) -> callList
        (EVEScope, CertificateTree) -> certificateTree
        (EVEScope, ErrorList) -> errorList
        (EVEScope, RefTypes) -> refTypes
        (EVEScope, SkillTree) -> skillTree
        (EVEScope, ConquerableStationList) -> conquerableStationList
        (EVEScope, FacWarStats) -> facWarStats
        (EVEScope, FacWarTopStats) -> facWarTopStats
        (EVEScope, TypeName) -> typeName undefined
        (EVEScope, CharacterName) -> characterName undefined
        (EVEScope, CharacterInfo) -> characterInfo undefined
        (EVEScope, AllianceList) -> allianceList undefined
        (EVEScope, CharacterID) -> characterID undefined
        (MapScope, FacWarSystems) -> facWarSystems
        (MapScope, Jumps) -> jumps
        (MapScope, Kills) -> kills
        (MapScope, Sovereignty) -> sovereignty
        (ServerScope, ServerStatus) -> serverStatus
        (AcctScope, APIKeyInfo) -> apiKeyInfo undefined
        (AcctScope, AccountStatus) -> accountStatus undefined
        (AcctScope, Characters) -> characters undefined
        (CorpScope, CorporationSheet) -> corporationSheet undefined
        (CorpScope, AccountBalance) -> corpAccountBalance undefined
        (CorpScope, AssetList) -> corpAssetList undefined
        (CorpScope, ContactList) -> corpContactList undefined
        (CorpScope, ContainerLog) -> containerLog undefined
        (CorpScope, ContractBids) -> corpContractBids undefined
        (CorpScope, FacWarStats) -> corpFacWarStats undefined
        (CorpScope, IndustryJobs) -> corpIndustryJobs undefined
        (CorpScope, Medals) -> corpMedals undefined
        (CorpScope, MemberMedals) -> memberMedals undefined
        (CorpScope, MemberSecurity) -> memberSecurity undefined
        (CorpScope, MemberSecurityLog) -> memberSecurityLog undefined
        (CorpScope, OutpostList) -> outpostList undefined
        (CorpScope, Shareholders) -> shareholders undefined
        (CorpScope, Standings) -> corpStandings undefined
        (CorpScope, StarbaseList) -> starbaseList undefined
        (CorpScope, Titles) -> titles undefined
        (CorpScope, Locations) -> corpLocations undefined undefined
        (CorpScope, Contracts) -> corpContracts undefined undefined
        (CorpScope, ContractItems) -> corpContractItems undefined undefined
        (CorpScope, KillLog) -> corpKillLog undefined undefined
        (CorpScope, MarketOrders) -> corpMarketOrders undefined undefined
        (CorpScope, MemberTracking) -> memberTracking undefined undefined
        (CorpScope, StarbaseDetail) -> starbaseDetail undefined undefined
        (CorpScope, OutpostServiceDetail) -> outpostServiceDetail undefined undefined
        (CorpScope, WalletJournal) -> corpWalletJournal undefined undefined undefined undefined
        (CorpScope, WalletTransactions) -> corpWalletTransactions undefined undefined undefined undefined
        (CharScope, AccountBalance) -> charAccountBalance undefined undefined
        (CharScope, AssetList) -> charAssetList undefined undefined
        (CharScope, CharacterSheet) -> characterSheet undefined undefined
        (CharScope, ContactList) -> charContactList undefined undefined
        (CharScope, ContactNotifications) -> contactNotifications undefined undefined
        (CharScope, ContractBids) -> charContractBids undefined undefined
        (CharScope, FacWarStats) -> charFacWarStats undefined undefined
        (CharScope, IndustryJobs) -> charIndustryJobs undefined undefined
        (CharScope, MailingLists) -> mailingLists undefined undefined
        (CharScope, MailMessages) -> mailMessages undefined undefined
        (CharScope, Medals) -> charMedals undefined undefined
        (CharScope, Notifications) -> notifications undefined undefined
        (CharScope, Research) -> research undefined undefined
        (CharScope, SkillInTraining) -> skillInTraining undefined undefined
        (CharScope, SkillQueue) -> skillQueue undefined undefined
        (CharScope, Standings) -> charStandings undefined undefined
        (CharScope, UpcomingCalendarEvents) -> upcomingCalendarEvents undefined undefined
        (CharScope, Contracts) -> charContracts undefined undefined undefined
        (CharScope, ContractItems) -> charContractItems undefined undefined undefined
        (CharScope, MarketOrders) -> charMarketOrders undefined undefined undefined
        (CharScope, KillLog) -> charKillLog undefined undefined undefined
        (CharScope, MailBodies) -> mailBodies undefined undefined undefined
        (CharScope, NotificationTexts) -> notificationTexts undefined undefined undefined
        (CharScope, CalendarEventAttendees) -> calendarEventAttendees undefined undefined undefined
        (CharScope, Locations) -> charLocations undefined undefined undefined
        _ -> return NotRecognised
