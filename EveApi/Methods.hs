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
--import Control.Monad.Trans.Control
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
--import Text.XML

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
type CallHandler m = Maybe Key -> [APIArgument] -> CallResult m ApiResult

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
makeCall :: CallContext m => Scope -> Call -> CallHandler m
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

-- XXX: FromKey args are sent even if they're not present and the key is.
checkParams :: CallContext m => Maybe Key -> [APIArgument]
                                    -> CallResult m (Maybe (Maybe Key, [APIArgument]))
checkParams mKey args = do
    (CallParams _ _ keyType argList) <- lift $ asks callParams
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
            hasKey || keyType == NoKey
            || and [any (matchArgSpec (argName sp)) args
                            | sp <- argList, argOptional sp == FromKey]
        mArgList = if not (reqPresent && fromKeyPresentIfNeeded)
                    then Nothing
                    else Just (nubBy sameArgType $ sort args)
    return $ (,) <$> mmKey <*> mArgList

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
doCall scope call mKey args = do
    runResourceT $ case (scope, call) of
        -- special cases here
        _ -> makeCall scope call mKey args
