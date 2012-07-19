module Handler.Call where

import Import hiding (Key, sequence) -- clashes with custom type

import EveApi.Types
import EveApi.Values
import EveApi.Methods
import ModelUtils

import Control.Monad.Reader (runReaderT)
import Control.Monad.Error hiding (sequence)
import Control.Monad.Trans.Resource

import Data.Maybe (isNothing, isJust, fromJust, catMaybes)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Traversable (sequence)
import Data.Attoparsec.Text hiding (takeWhile)
import qualified Data.Attoparsec.Text as AP

-- Parse a comma-separated list.
parseList :: Parser a -> Text -> Maybe [a]
parseList p t = case (skipSpace >> p `sepBy` char ',') `parse` t of
    Done _ r -> Just r
    _        -> Nothing

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

keyForm :: CallArgs -> Form (Maybe Key) -- return type (FormResult x, Widget)
keyForm (CallArgs keyType _) = renderDivs $ formToAForm $
    case keyType of -- don't make a form if no key is needed
        NoKey -> return (FormSuccess (Just KeyNone), mempty)
        _     -> do
            let req = keyOpt keyType == Mandatory
            (frkeyid, fieldkeyid) <- moptreq req intField "Key ID:" Nothing
            (frvcode, fieldvcode) <- moptreq req textField "vCode:" Nothing
            let fields :: [FieldView App App]
                fields = [fieldkeyid, fieldvcode]
                formFail mesgs = return (FormFailure mesgs, fields)
            case (frkeyid, frvcode) of
                (FormSuccess mkeyid, FormSuccess mrawvcode) ->
                    -- TODO: validate vCode
                    if isNothing mkeyid || isNothing mrawvcode
                    then if req -- no key provided, but do we need one?
                        then formFail ["This call requires a key"]
                        else return (FormSuccess Nothing, fields)
                    else return
                        (FormSuccess
                            (Just $ Key (fromJust mkeyid)
                                        (fromJust mrawvcode)
                                        UnknownKeyScope)
                        ,fields)
                -- Unfortunately, although FormResult has a Monoid
                -- instance, we can't use it here because the two sides
                -- have different types and the result yet another.
                (FormFailure failkey, FormFailure failvcode) ->
                    formFail (failkey ++ failvcode)
                (FormFailure failkey, _) ->
                    formFail failkey
                (_, FormFailure failvcode) ->
                    formFail failvcode
                _ -> formFail []

argForm :: Call -> CallArgs -> Bool -> Form [APIArgument]
argForm call (CallArgs _ args) hasKey = do
    let consolidate :: -- Collect results, if there's no error
               MForm App App [(FormResult (Maybe a), [FieldView App App])]
            -> Form [a]
        consolidate mresults = renderDivs $ formToAForm $ do
            results <- mresults
            let allresults = fmap catMaybes . mconcat .
                                map (fmap return . fst) $ results
                allfields = concatMap snd results
            return (allresults, allfields)
        --notImplemented = return (FormFailure ["Not implemented"], [])
    consolidate $ forM args $ \(Arg name _atype opt) -> do
        let req = opt == Mandatory  -- Check later in case opt == FromKey
            -- Do the following:
            -- * Use mopt or mreq as appropriate
            -- * If there's a result, pass it to a postprocessing
            --   function, which may return failure
            -- * If this function succeeds, apply the provided constructor
            -- * Put the fieldview in a list for later consolidation
            mkfield :: (b -> APIArgument)
                          -> Field App App a
                          -> FieldSettings App
                          -> Maybe a
                          -> (Maybe a -> FormResult (Maybe b))
                          -> MForm App App (FormResult (Maybe APIArgument),
                                            [FieldView App App])
            mkfield constr fieldtype fsettings def process = do
                (result, field) <- moptreq req fieldtype fsettings def
                case result of
                    FormSuccess a  -> return (fmap constr <$> process a,
                                                                    [field])
                    FormMissing    -> return (FormMissing,          [field])
                    FormFailure ms -> return (FormFailure ms,       [field])
            parseListField :: ([a] -> APIArgument)  -- ^ constructor
                                   -> FieldSettings App -- ^ field settings
                                   -> Text          -- ^ error message
                                   -> Parser a      -- ^ element parser
                                   -> Maybe Text    -- ^ default
                                   -> MForm App App
                                        (FormResult (Maybe APIArgument),
                                         [FieldView App App])
            parseListField constr fsettings errmsg parser def
                = mkfield constr textField fsettings def $ \rawlist ->
                    let maybeElems = parseList parser =<< rawlist
                    in  maybe (FormFailure [errmsg])
                              (\elems -> FormSuccess $ Just elems)
                              maybeElems
        case name of
            ArgTIDs -> parseListField ArgIDs
                            "List of IDs (comma-separated):"
                            "Invalid ID list"
                            decimal
                            Nothing
            ArgTVersion -> mkfield ArgVersion checkBoxField "Include members"
                           Nothing $
                           FormSuccess .
                            (maybe Nothing $ \include ->
                                if include then Nothing else Just 1)
                                    -- alliance list, 1 = omit member corps
            ArgTNames -> parseListField ArgNames
                            "List of names (comma-separated):"
                            "Invalid name list"
                            characterName
                            Nothing
            ArgTCharacterID -> case opt of
                FromKey ->
                    if hasKey -- Assume key is appropriate for this call
                              -- TODO: only use it if it *is* appropriate
                    then return (FormSuccess Nothing, []) -- ignore
                    else mkfield ArgCharacterID intField "Character ID:"
                            Nothing $
                            maybe FormMissing (FormSuccess . Just)
                Mandatory ->
                    mkfield ArgCharacterID intField "Character ID:" Nothing $
                        maybe FormMissing (FormSuccess . Just)
                Optional ->
                    mkfield ArgCharacterID intField "Character ID:" Nothing $
                        maybe (FormSuccess Nothing) (FormSuccess . Just)
            ArgTCorporationID -> case opt of
                FromKey ->
                    if hasKey -- Assume key is appropriate for this call
                              -- TODO: only use it if it *is* appropriate
                    then return (FormSuccess Nothing, []) -- ignore
                    else mkfield ArgCorporationID intField "Corporation ID:"
                            Nothing $
                            maybe FormMissing (FormSuccess . Just)
                Mandatory ->
                    mkfield ArgCorporationID intField "Corporation ID:"
                        Nothing $
                        maybe FormMissing (FormSuccess . Just)
                Optional ->
                    mkfield ArgCorporationID intField "Corporation ID:"
                        Nothing $
                        maybe (FormSuccess Nothing) (FormSuccess . Just)
            ArgTEventIDs -> parseListField ArgIDs
                            "List of event IDs (comma-separated):"
                            "Invalid event ID list"
                            decimal
                            Nothing
            ArgTContractID ->
                mkfield ArgContractID intField "Contract ID:" Nothing $
                    maybe FormMissing (FormSuccess . Just)
            ArgTBeforeKillID ->
                mkfield ArgBeforeKillID intField "Fetch kills before kill ID:"
                    Nothing $
                    maybe FormMissing (FormSuccess . Just)
            ArgTOrderID ->
                mkfield ArgOrderID intField "Order ID:" Nothing $
                    maybe FormMissing (FormSuccess . Just)
            ArgTExtended -> -- for corp member tracking
                mkfield ArgExtended checkBoxField "Extended info"
                    Nothing $
                    FormSuccess
            ArgTRowCount ->
                mkfield ArgRowCount intField "Number of rows to fetch:"
                    Nothing $
                    maybe FormMissing (FormSuccess . Just)
            ArgTFromID ->
                mkfield ArgFromID intField "Fetch entries before entry ID:"
                    Nothing $
                    maybe FormMissing (FormSuccess . Just)
            ArgTAccountKey ->
                mkfield ArgAccountKey (selectField opts) "Wallet division:"
                    Nothing $ -- XXX: check if there should be a default
                    maybe (FormSuccess Nothing)
                          (\division ->
                            if division `elem` [1000..1006]
                                then FormSuccess (Just (WalletDivision division))
                                else FormFailure ["Invalid wallet division"])
                    where opts = return $ mkOptionList
                            [Option ("Division " <> d')
                                    (1000 + d)
                                    (d') | d <- [0..6],
                                           let d' = T.pack (show (succ d))]
            ArgTItemID ->
                let label = case call of
                        StarbaseDetail -> "Starbase item ID:"
                        OutpostServiceDetail -> "Outpost item ID:"
                        _                    -> "Item ID:"
                in  mkfield ArgItemID intField label Nothing $
                        maybe FormMissing (FormSuccess . Just)

-- Fill the DB with the list of calls.
-- TODO: check if this is in the static dump. (probably not with all the info here)
populateCallDB :: Handler ()
populateCallDB = do
    -- Clear the DB first
    runDB $ deleteWhere ([] :: [Filter CallSpec])
    runDB $ deleteWhere ([] :: [Filter ArgSpec])
    runDB $ deleteWhere ([] :: [Filter CallArg])
    forM_ (M.toList apiCalls) $
        \((call, scope), CallArgs key arglist) -> do
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
doPage :: Scope -> Call -> Maybe CallArgs
            -> Bool
            -> Maybe ((FormResult (Maybe Key), Widget), Enctype)
            -> Maybe ((FormResult [APIArgument], Widget), Enctype)
            -> Maybe ApiResult
            -> Handler RepHtml
doPage scope call mArgs wasPost mKeyForm mArgForm mResult = do
    let wantKey = maybe False -- display a key form?
                        (\(CallArgs keyType _) -> case keyType of
                            NoKey -> False
                            _     -> True)
                        mArgs
        (mKeyWidg, keyEnctype) = maybe (Nothing, UrlEncoded)
                                       (\((keyRes, widg),keyenctype) ->
                                            (if wantKey then Just (keyRes, widg)
                                                        else Nothing,
                                             keyenctype))
                                       mKeyForm
        (mArgWidg, argEnctype) = maybe (Nothing, UrlEncoded)
                                       (\((argRes,widg),argenctype) ->
                                            (Just (argRes, widg),
                                             argenctype))
                                       mArgForm
        enctype = keyEnctype <> argEnctype
    defaultLayout $ do
        setTitle "API Calls"
        let -- can't put type annotations in the tempate :(
            argStringT = argString   :: APIArgumentType -> Text
            typeStringT = typeString :: APIDataType -> Text
        $(widgetFile "call")

-- Show list of args and form to provide them.
getCallR :: Scope -> Call -> Handler RepHtml
getCallR scope call = do
    mArgs <- runDB $ getArgs scope call -- Maybe CallArgs
    mKeyForm <- sequence $ runFormPost <$> keyForm <$> mArgs
    mArgForm <- sequence $ runFormPost <$> (argForm call <$> mArgs <*> pure False)
    doPage scope call mArgs False mKeyForm mArgForm Nothing

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
        mArgs <- runDB $ getArgs scope call
        mKeyForm <- sequence $ runFormPost <$> keyForm <$> mArgs
        let mKey = join $ maybe Nothing
                           (\((kr,_),_) -> case kr of
                                FormSuccess key -> Just key
                                _               -> Nothing)
                           mKeyForm
            hasKey = isJust mKey
        mArgForm <- sequence $ runFormPost <$> (argForm call <$> mArgs <*> pure hasKey)
        mResult <- case mArgForm of
            Just ((res, _), _) -> case res of
                FormSuccess args -> do
                    manager <- httpManager <$> getYesod
                    eRes <- runExceptionT $
                            runReaderT (doCall scope call mKey args) manager
                    case eRes of
                        Left e -> -- TODO: use fromException to distinguish
                                  -- between different exceptions
                            return . Just $ ServerError (T.pack $ show e)
                        Right result -> return (Just result) 
                FormMissing      -> return Nothing -- TODO: produce a message
                FormFailure _    -> return Nothing -- TODO: produce a message
            Nothing -> return Nothing -- invalid call TODO: produce a message
        doPage scope call mArgs True mKeyForm mArgForm mResult
