module Handler.Call where

import Import hiding (
        Key -- clashes with custom type
    ,   sequence
    )

import EveApi.Types
import EveApi.Values
import EveApi.Methods
import ModelUtils

import Control.Monad.Reader (runReaderT)
import Control.Monad.Error hiding (sequence)
import Control.Monad.Trans.Resource

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Traversable (sequence)
import Data.Attoparsec.Text hiding (takeWhile)
import qualified Data.Attoparsec.Text as AP

-- Reminder:
-- type Form a = Markup -> MForm App App (FormResult a, Widget)

-- Parse a comma-separated list.
parseList :: Parser a -> Text -> Maybe [a]
parseList p t = case (skipSpace >> p `sepBy` char ',' >>= \n -> endOfInput >> return n)
                     `parseOnly` t of
    Right r -> Just r
    Left  _ -> Nothing

characterName :: Parser Text
characterName = (\x y z -> x <> y <> z)
                    <$> word
                    <*> (option "" spaceword)
                    <*> (option "" spaceword)
    where word = AP.takeWhile $ inClass "-a-zA-Z0-9\'"
          spaceword = liftM2 T.cons (char ' ') word

-- Use mopt or mreq as dictated by the first argument.
-- This is made non-trivial by the return type of the optional version.
-- The last argument is a single Maybe like mreq, not double like mopt.
moptreq :: RenderMessage App FormMessage =>
            Bool -> Field App App a -> FieldSettings App -> Maybe a
                 -> MForm App App (FormResult (Maybe a), FieldView App App)
moptreq req field fsettings def =
            if req
            then (\(r,w) -> (Just <$> r,w)) <$>
                    mreq field fsettings def
            else mopt field fsettings (Just def)

keyForm :: CallParams -> Form (Maybe Key)
keyForm (CallParams keyType _) = renderDivs $ formToAForm $
    case keyType of -- don't make a form if no key is needed
        NoKey -> return (FormSuccess Nothing, mempty)
        _     -> do
            let req = keyOpt keyType == Required
            (frkeyid, fieldkeyid) <- moptreq req intField "Key ID:" Nothing
            (frvcode, fieldvcode) <- moptreq req textField "vCode:" Nothing
            let fields :: [FieldView App App]
                fields = [fieldkeyid, fieldvcode]
                formFail mesgs = return (FormFailure mesgs, fields)
                formSucceed res = return (FormSuccess res,  fields)
            case (frkeyid, frvcode) of
                (FormSuccess mkeyid, FormSuccess mrawvcode) ->
                    -- TODO: validate vCode
                    if isNothing mkeyid || isNothing mrawvcode
                    then if req -- no key provided, but do we need one?
                        then do
                            formFail ["This call requires a key"]
                        else do
                            return (FormSuccess Nothing, fields)
                    else do
                        return
                            (FormSuccess
                                (Just $ Key (fromJust mkeyid)
                                            (fromJust mrawvcode)
                                            UnknownKeyScope)
                            ,fields)
                -- Unfortunately, although FormResult has a Monoid
                -- instance, we can't use it here because the two sides
                -- have different types and the result yet another.
                (FormFailure failkey, FormFailure failvcode) ->
                    if req then do
                            formFail (failkey ++ failvcode)
                           else formSucceed Nothing
                (FormFailure failkey, _) ->
                    if req then formFail failkey
                           else formSucceed Nothing
                (_, FormFailure failvcode) ->
                    if req then formFail failvcode
                           else formSucceed Nothing
                _ -> formFail []

aoptreq :: Field App App a              -- ^ Field to use, e.g. intField
            -> APIArgumentType          -- ^ Argument type, e.g. characterID
            -> Bool                     -- ^ Is this field required?
            -> (a -> Maybe APIArgument) -- ^ Constructor to apply afterwards
            -> AForm App App (Maybe APIArgument)
aoptreq field name required constr =
    (constr =<<) <$> if required
        then Just <$> areq field (argString name) Nothing
        else          aopt field (argString name) Nothing

argForm :: Call -> CallParams -> Form [APIArgument]
argForm call (CallParams _ args) markup = do
    -- XXX: is mconcat correct?
    -- Used at type [(FormResult [Maybe APIArgument],Widget)] ->
    --               (FormResult [Maybe APIArgument],Widget)
    -- but does it mean the right thing?
    fmap mconcat $ forM args $ \(Arg name atype opt) -> do
        let required = opt == Required
        flip renderDivs markup . fmap maybeToList $ 
            if name == ArgTVersion && call == AllianceList
            then aoptreq checkBoxField name False (\b -> Just . ArgVersion $
                                                            if b then 1 else 0)
            else case atype of
                AInteger -> aoptreq intField name required $ Just . case name of
                    ArgTVersion -> ArgVersion
                    ArgTCharacterID -> ArgCharacterID
                    ArgTCorporationID -> ArgCorporationID
                    ArgTContractID -> ArgContractID
                    ArgTBeforeKillID -> ArgBeforeKillID
                    ArgTOrderID -> ArgOrderID
                    ArgTRowCount -> ArgRowCount
                    ArgTFromID -> ArgFromID
                    ArgTItemID -> ArgItemID
                    _          -> error $ "argForm: not an integer field: " ++ show name
                ABool -> aoptreq boolField name required $ Just . case name of
                    ArgTExtended -> ArgExtended
                    _            -> error $ "argForm: not a boolean field: " ++ show name
                AIntList -> aoptreq textField name required $
                                fmap ilconstr . parseList decimal
                    where ilconstr = case name of
                            ArgTIDs      -> ArgIDs
                            ArgTEventIDs -> ArgEventIDs
                            _            -> error $ "argForm: not an intlist field: "
                                                    ++ show name
                AStringList -> aoptreq textField name required $
                                fmap ilconstr . parseList parseElem
                    where ilconstr = case name of
                            ArgTNames -> ArgNames
                            _         -> error $ "argForm: not a stringlist field: "
                                                 ++ show name
                          parseElem = case name of
                            ArgTNames -> characterName
                            _         -> error $ "argForm: not a stringlist field: "
                                                 ++ show name
                _ -> error $ "argForm: Don't know how to handle arguments of this type: "
                             ++ show atype
                             ++ " (trying: " ++ show name ++ ")"

keyArgForm :: Call -> CallParams -> Form (Maybe Key, [APIArgument])
keyArgForm call params@(CallParams keyType _) markup = do
    let wantKey = case keyType of
                    NoKey -> False
                    _     -> True
    (keyres, keywidg) <- if wantKey
                            then keyForm params markup
                            else return (FormSuccess Nothing, mempty)
    (argres, argwidg) <- argForm call params markup
    let widg = keywidg <> argwidg
    case keyres of
        FormSuccess mKey -> case argres of
            FormSuccess args  -> return (FormSuccess (mKey, args), widg)
            FormFailure msgs' -> return (FormFailure msgs',        widg)
            FormMissing       -> return (FormMissing,              widg)
        FormFailure msgs -> case argres of
            FormSuccess _     -> return (FormFailure msgs,            widg)
            FormFailure msgs' -> return (FormFailure (msgs <> msgs'), widg)
            FormMissing       -> return (FormMissing,                 widg)
        FormMissing -> case argres of
            FormSuccess _     -> return (FormMissing,       widg)
            FormFailure msgs' -> return (FormFailure msgs', widg)
            FormMissing       -> return (FormMissing,       widg)

-- Fill the DB with the list of calls.
-- TODO: check if this is in the static dump. (probably not with all the info here)
populateCallDB :: Handler ()
populateCallDB = do
    -- Clear the DB first
    runDB $ deleteWhere ([] :: [Filter CallSpec])
    runDB $ deleteWhere ([] :: [Filter ArgSpec])
    runDB $ deleteWhere ([] :: [Filter CallArg])
    forM_ (M.toList apiCalls) $
        \((call, scope), CallParams key arglist) -> do
            -- Insert the call
            callID <- runDB $ insert $ CallSpec scope call key
            -- Insert the ArgSpecs (getting IDs of any that are already in)
            let argSpecs = map ArgSpec arglist
            argSpecIDs <- mapM (liftM (either entityKey id) . runDB . insertBy)
                               argSpecs
            -- Insert the relation between the two
            mapM (\argID -> runDB $ insertBy (CallArg callID argID)) argSpecIDs

-- Render the page.
-- Form widgets are built separately to let get/post handle them differently.
doPage :: Scope -> Call -> Maybe CallParams
            -> Bool
            -> Maybe ((FormResult (Maybe Key, [APIArgument]), Widget), Enctype)
            -> Maybe ApiResult
            -> Handler RepHtml
doPage scope call mParams wasPost mForm mResult = do
    let wantKey = maybe False -- display a key form?
                        (\(CallParams keyType _) -> case keyType of
                            NoKey -> False
                            _     -> True)
                        mParams
        (mWidg, enctype) = maybe (Nothing, UrlEncoded)
                                 (\((res,widg),enctype) ->
                                     (Just (res, widg),
                                      enctype))
                                 mForm
    defaultLayout $ do
        setTitle "API Calls"
        let -- can't put type annotations in the tempate :(
            argStringT = argString   :: APIArgumentType -> Text
            typeStringT = typeString :: APIDataType -> Text
        $(widgetFile "call")

-- Show list of args and form to provide them.
getCallR :: Scope -> Call -> Handler RepHtml
getCallR scope call = do
    mParams <- runDB $ getArgs scope call
    mForm <- sequence $ runFormPostNoToken <$>
                            (keyArgForm call <$> mParams)
    doPage scope call mParams False mForm Nothing

postCallR :: Scope -> Call -> Handler RepHtml
postCallR scope call = do
    populateString  <- runInputPost $ iopt hiddenField "PopulateCallDB"
    let populate | Just pop <- maybeRead =<< populateString = pop
                 | otherwise = False
    if populate
    then do
        populateCallDB
        redirect (CallR scope call)
    else do
        mParams <- runDB $ getArgs scope call
        mForm <- sequence $ runFormPostNoToken <$>
                                (keyArgForm call <$> mParams)
        mResult <- case mForm of
            Just ((res, _), _) -> case res of
                FormSuccess (mKey, args) -> do
                    manager <- httpManager <$> getYesod
                    eRes <- sequence $ runExceptionT <$>
                                runReaderT (doCall scope call mKey args) <$>
                                    -- ExceptionT m ApiResult
                                    CallData manager <$> mParams
                    case eRes of
                        Just (Left e) -> -- TODO: use fromException to distinguish
                                         -- between different exceptions
                            return . Just $ ServerError (T.pack $ show e)
                        Just (Right result) -> return (Just result) 
                        Nothing             -> return (Just NotRecognised)
                FormMissing      -> return Nothing -- TODO: produce a message
                FormFailure _    -> return Nothing -- TODO: produce a message
            Nothing -> return Nothing -- invalid call TODO: produce a message
        doPage scope call mParams True mForm mResult
