{-# LANGUAGE
        StandaloneDeriving
    ,   OverloadedStrings
    ,   FlexibleInstances
    ,   ConstraintKinds #-}
module EveApi.Methods (
        ApiResult (..)
    ,   CallData (..)
    ,   CallResult
    ,   doCall
    )
    where

import Prelude -- remove this when splitting into another library
import Data.List (nubBy, sort)
import Data.Maybe
import Data.Monoid
import GHC.Exts (IsString (..))

import EveApi.Types
import EveApi.Errors
import EveApi.Orphans ()

import Control.Applicative
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

data CallData = CallData
    { callManager :: Manager
    , callParams  :: CallParams
    }

data ApiResult = Success
                    Text        -- ^ raw response xml
--                    Document    -- ^ response DOM tree
               | Error EveApiError
               | UserError Text
               | ServerError Text
               | NotImplemented
               | NotRecognised
    deriving (Show, Eq, Ord)
type CallResult m a = ResourceT (ReaderT CallData (ExceptionT m)) a
type CallContext m = (MonadIO m, MonadUnsafeIO m, MonadBaseControl IO m)

-- This supposedly fails the coverage condition, though it should hold transitively
-- TODO: determine whether UndecidableInstances is harmless, and if so, add it
--instance MonadReader r m => MonadReader r (ResourceT m) where
--    ask = lift ask

-- TODO: parametrize on hostname
callBaseURL :: Scope -> Call -> Either HttpException (Request m)
callBaseURL scope call = case call of
    CallList -> callBaseURL' "calllist"
    _        -> callBaseURL' (show call)
    where callBaseURL' callS = parseUrl $
            "https://api.eveonline.com/"
            ++ scopeString scope ++ "/"
            ++ callS ++ ".xml.aspx"

-- | Worker function. This takes the call parameters and actually makes the call.
makeCall :: CallContext m =>
              Scope -> Call -> Maybe Key -> [APIArgument] -> CallResult m ApiResult
makeCall scope call mKey' args' = do
    mParams <- checkParams mKey' args'
    case mParams of
        Nothing -> return $ UserError "Bad arguments"
        Just (mKey, args) -> case callBaseURL scope call of
            Left  exc -> lift $ monadThrow exc
            Right req' -> do
                let keyargs = maybe []
                                    (\(Key keyid vcode _) ->
                                        [("keyID", BU.fromString $ show keyid)
                                        ,("vCode", encodeUtf8 vcode)])
                                    mKey
                    req = urlEncodedBody (map argToParam args ++ keyargs) req'
                Response s@(Status statuscode _) _ headers body <-
                    httpLbs req =<< lift (asks callManager)
                case statuscode of
                    200 -> return . Success . decodeUtf8 $
                            B.concat . toChunks $ body
                        -- TODO: parse and determine if the API returned an error
                    _   -> lift . monadThrow $
                            StatusCodeException s headers

checkParams :: CallContext m => Maybe Key -> [APIArgument]
                                    -> CallResult m (Maybe (Maybe Key, [APIArgument]))
checkParams mKey args = do
    (CallParams keyType argList) <- lift $ asks callParams
    let mmKey = case (keyType, mKey) of
            (NoKey, Nothing) -> Just Nothing -- no key for this call
            (NoKey, Just _)  -> Nothing      -- provided key when none needed
            (kt   , Nothing) | keyOpt kt == Required -> Nothing
                             | otherwise             -> Just Nothing
                                          -- TODO: validate key type for this call
            (_    , Just _k) -> Just mKey -- TODO: validate access mask for this call
        hasKey = isJust mmKey && isJust (fromJust mmKey)
        -- Are all required args present?
        reqPresent = and [any (matchArgSpec (argName sp)) args
                            | sp <- argList, argOptional sp == Required]
        -- If there's no key provided, are all FromKey args present?
        fromKeyPresentIfNeeded =
            hasKey
            || and [any (matchArgSpec (argName sp)) args
                            | sp <- argList, argOptional sp == FromKey]
        mArgList = if not (reqPresent && fromKeyPresentIfNeeded)
                    then Nothing
                    else Just (nubBy sameArgType $ sort args)
    return $ (,) <$> mmKey <*> mArgList

---------------
-- API scope --
---------------
callList :: CallContext m => CallResult m ApiResult
callList = makeCall APIScope CallList Nothing []

---------------
-- EVE scope --
---------------

-- Static
certificateTree, errorList, refTypes, skillTree :: CallContext m => CallResult m ApiResult
certificateTree = makeCall EVEScope CertificateTree Nothing []
errorList       = makeCall EVEScope ErrorList       Nothing []
refTypes        = makeCall EVEScope RefTypes        Nothing []
skillTree       = makeCall EVEScope SkillTree       Nothing []

-- Dynamic, no args
conquerableStationList, facWarStats, facWarTopStats :: CallContext m => CallResult m ApiResult
conquerableStationList  = makeCall EVEScope ConquerableStationList  Nothing []
facWarStats             = makeCall EVEScope FacWarStats             Nothing []
facWarTopStats          = makeCall EVEScope FacWarTopStats          Nothing []

-- Dynamic, args required
typeName, characterName :: CallContext m => [APIArgument] -> CallResult m ApiResult
characterInfo           :: CallContext m =>  Text         -> CallResult m ApiResult
allianceList            :: CallContext m =>  Bool         -> CallResult m ApiResult
characterID             :: CallContext m => [Text]        -> CallResult m ApiResult
typeName _ = return NotImplemented
characterName _ = return NotImplemented
characterInfo _ = return NotImplemented
allianceList _ = return NotImplemented
characterID _ = return NotImplemented

---------------
-- Map scope --
---------------
facWarSystems, jumps, kills, sovereignty :: CallContext m => CallResult m ApiResult
facWarSystems   = makeCall MapScope FacWarSystems Nothing []
jumps           = makeCall MapScope Jumps         Nothing []
kills           = makeCall MapScope Kills         Nothing []
sovereignty     = makeCall MapScope Sovereignty   Nothing []

------------------
-- Server scope --
------------------
serverStatus :: CallContext m => CallResult m ApiResult
serverStatus = makeCall ServerScope ServerStatus Nothing []

-------------------
-- Account scope --
-------------------
apiKeyInfo, accountStatus, characters :: CallContext m => Key -> CallResult m ApiResult
apiKeyInfo _ = return NotImplemented
accountStatus _ = return NotImplemented
characters _ = return NotImplemented

----------------
-- Corp scope --
----------------

-- Public info if no key
corporationSheet :: CallContext m => Either Key Integer -> CallResult m ApiResult
corporationSheet _ = return NotImplemented

-- No args
corpAccountBalance, corpAssetList               :: CallContext m => Key -> CallResult m ApiResult
corpContactList, containerLog, corpContractBids :: CallContext m => Key -> CallResult m ApiResult
corpFacWarStats, corpIndustryJobs, corpMedals   :: CallContext m => Key -> CallResult m ApiResult
memberMedals, memberSecurity, memberSecurityLog :: CallContext m => Key -> CallResult m ApiResult
outpostList, shareholders, corpStandings        :: CallContext m => Key -> CallResult m ApiResult
starbaseList, titles                            :: CallContext m => Key -> CallResult m ApiResult
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
corpLocations        :: CallContext m => Key ->       [Integer] -> CallResult m ApiResult
corpContracts        :: CallContext m => Key ->  Maybe Integer  -> CallResult m ApiResult
corpContractItems    :: CallContext m => Key ->        Integer  -> CallResult m ApiResult
corpKillLog          :: CallContext m => Key ->  Maybe Integer  -> CallResult m ApiResult
corpMarketOrders     :: CallContext m => Key ->  Maybe Integer  -> CallResult m ApiResult
memberTracking       :: CallContext m => Key ->        Bool     -> CallResult m ApiResult
starbaseDetail       :: CallContext m => Key ->        Integer  -> CallResult m ApiResult
outpostServiceDetail :: CallContext m => Key ->        Integer  -> CallResult m ApiResult
corpLocations _ _ = return NotImplemented
corpContracts _ _ = return NotImplemented
corpContractItems _ _ = return NotImplemented
corpKillLog _ _ = return NotImplemented
corpMarketOrders _ _ = return NotImplemented
memberTracking _ _ = return NotImplemented
starbaseDetail _ _ = return NotImplemented
outpostServiceDetail _ _ = return NotImplemented

-- Up to 3 args
corpWalletJournal, corpWalletTransactions
    :: CallContext m => Key -> Maybe Int -> Maybe Int -> Maybe Int -> CallResult m ApiResult
corpWalletJournal _ _ _ _ = return NotImplemented
corpWalletTransactions _ _ _ _ = return NotImplemented

----------------
-- Char scope --
----------------

-- One arg, namely character ID for multi-character keys
charAccountBalance     :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
charAssetList          :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
characterSheet         :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
charContactList        :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
contactNotifications   :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
charContractBids       :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
charFacWarStats        :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
charIndustryJobs       :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
mailingLists           :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
mailMessages           :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
charMedals             :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
notifications          :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
research               :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
skillInTraining        :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
skillQueue             :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
charStandings          :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
upcomingCalendarEvents :: CallContext m => Key -> Maybe Integer -> CallResult m ApiResult
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
charContractItems :: CallContext m => Key -> Integer ->       Integer  -> CallResult m ApiResult
charContracts     :: CallContext m => Key -> Integer -> Maybe Integer  -> CallResult m ApiResult
charMarketOrders  :: CallContext m => Key -> Integer -> Maybe Integer  -> CallResult m ApiResult
charKillLog       :: CallContext m => Key -> Integer -> Maybe Integer  -> CallResult m ApiResult
mailBodies        :: CallContext m => Key -> Integer ->      [Integer] -> CallResult m ApiResult
notificationTexts :: CallContext m => Key -> Integer ->      [Integer] -> CallResult m ApiResult
calendarEventAttendees :: CallContext m => Key -> Integer -> [Integer] -> CallResult m ApiResult
charLocations     :: CallContext m => Key -> Integer ->      [Integer] -> CallResult m ApiResult
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

listify :: (Monoid s, IsString s) => (a -> s) -> [a] -> s
listify sh xs = mconcat . intersperse "," . map sh $ xs

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

doCall :: (CallContext m, MonadThrow m) =>
    Scope -> Call -> Maybe Key -> [APIArgument] ->
             ReaderT CallData (ExceptionT m) ApiResult
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
