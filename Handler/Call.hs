module Handler.Call where

import Import hiding (Key, sequence) -- clashes with custom type

import EveApiTypes
import EveApiValues
import ModelUtils

import Data.Maybe (isNothing, fromJust, catMaybes)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Traversable (sequence)
import Control.Monad (join, liftM2)
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

callForm :: Scope -> Call -> CallArgs -> Form FullCall
callForm scope call callargs html = do
    -- First, some definitions
    let CallArgs keyType args = callargs
        -- Use mopt or mreq as dictated by the first argument.
        -- This is made non-trivial by the return type of the optional version.
        -- The last argument is a single Maybe like mreq, not double like mopt.
        moptreq :: RenderMessage App FormMessage =>
                    Bool -> Field App App a -> FieldSettings App -> Maybe a
                         -> MForm App App (FormResult (Maybe a), FieldView App App)
        moptreq req field settings def =
                    if req
                    then (\(r,w) -> (Just <$> r,w)) <$>
                            mreq field settings def
                    else mopt field settings (Just def)
        -- For some argument fields, we need to know whether a key has been
        -- provided to know whether they are needed or not. Hence, we process
        -- the key first.
        keyfields :: MForm App App (FormResult (Maybe Key), [FieldView App App])
        keyfields = case keyType of
            NoKey -> return (FormSuccess (Just KeyNone), [])
            _     -> do
                let req = keyOpt keyType == Mandatory
                (frkeyid, fieldkeyid) <- moptreq req intField "Key ID" Nothing
                (frvcode, fieldvcode) <- moptreq req textField "vCode" Nothing
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
    (keyres,keyview) <- keyfields
    let haskey = case keyres of { FormSuccess (Just _) -> True; _ -> False }
        -- The main worker "function"
        argfields :: MForm App App (FormResult [APIArgument], [FieldView App App])
        argfields = do
            let consolidate :: -- Collect results, if there's no error
                       MForm App App [(FormResult (Maybe a), [FieldView App App])]
                    -> MForm App App  (FormResult       [a], [FieldView App App])
                consolidate mresults = do
                    results <- mresults
                    let allresults = fmap catMaybes . mconcat .
                                        map (fmap return . fst) $ results
                        allfields = concatMap snd results
                    return (allresults, allfields)
            consolidate $ forM args $ \(Arg name _atype opt) -> do
                let req = opt == Mandatory  -- Check later in case opt == FromKey
                    notImplemented = return (FormFailure ["Not implemented"], [])
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
                    mkfield constr fieldtype settings def process = do
                        (result, field) <- moptreq req fieldtype settings def
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
                    parseListField constr settings errmsg parser def
                        = mkfield constr textField settings def $ \rawlist ->
                            let maybeElems = parseList parser =<< rawlist
                            in  maybe (FormFailure [errmsg])
                                      (\elems -> FormSuccess $ Just elems)
                                      maybeElems
                case name of
                    ArgTIDs -> parseListField ArgIDs
                                    "List of IDs (comma-separated)"
                                    "Invalid ID list"
                                    decimal
                                    Nothing
                    ArgTVersion -> mkfield
                                   ArgVersion
                                   checkBoxField
                                   "Include members"
                                   Nothing $ FormSuccess .
                                    (maybe Nothing $ \include ->
                                        if include then Nothing else Just 1)
                                            -- alliance list, 1 = omit member corps
                    ArgTNames -> parseListField ArgNames
                                    "List of names (comma-separated)"
                                    "Invalid name list"
                                    characterName
                                    Nothing
                    --ArgTCharacterID -> notImplemented   -- int, may be FromKey
                    ArgTCharacterID -> case opt of
                        FromKey ->
                            if haskey -- Assume key is appropriate for this call
                                      -- TODO: only use it if it *is* appropriate
                            then return (FormSuccess Nothing, []) -- ignore
                            else mkfield ArgCharacterID intField "Character ID"
                                    Nothing $
                                    maybe FormMissing (FormSuccess . Just)
                        Mandatory ->
                            mkfield ArgCharacterID intField "Character ID" Nothing $
                                maybe FormMissing (FormSuccess . Just)
                        Optional ->
                            mkfield ArgCharacterID intField "Character ID" Nothing $
                                maybe (FormSuccess Nothing) (FormSuccess . Just)
                    ArgTCorporationID -> case opt of
                        FromKey ->
                            if haskey -- Assume key is appropriate for this call
                                      -- TODO: only use it if it *is* appropriate
                            then return (FormSuccess Nothing, []) -- ignore
                            else mkfield ArgCorporationID intField "Corporation ID"
                                    Nothing $
                                    maybe FormMissing (FormSuccess . Just)
                        Mandatory ->
                            mkfield ArgCorporationID intField "Corporation ID"
                                Nothing $
                                maybe FormMissing (FormSuccess . Just)
                        Optional ->
                            mkfield ArgCorporationID intField "Corporation ID"
                                Nothing $
                                maybe (FormSuccess Nothing) (FormSuccess . Just)
                    ArgTEventIDs -> parseListField ArgIDs
                                    "List of event IDs (comma-separated)"
                                    "Invalid event ID list"
                                    decimal
                                    Nothing
                    ArgTContractID ->
                        mkfield ArgContractID intField "Contract ID" Nothing $
                            maybe FormMissing (FormSuccess . Just)
                    ArgTBeforeKillID ->
                        mkfield ArgBeforeKillID intField "Fetch kills before kill ID:"
                            Nothing $
                            maybe FormMissing (FormSuccess . Just)
                    ArgTOrderID ->
                        mkfield ArgOrderID intField "Order ID" Nothing $
                            maybe FormMissing (FormSuccess . Just)
                    ArgTExtended -> notImplemented      -- for corp member tracking
                        {-mfield checkBoxField "Extended information" Nothing-}
                    ArgTRowCount ->
                        mkfield ArgRowCount intField "Number of rows to fetch"
                            Nothing $
                            maybe FormMissing (FormSuccess . Just)
                    ArgTFromID ->
                        mkfield ArgFromID intField "Fetch entries before entry ID:"
                            Nothing $
                            maybe FormMissing (FormSuccess . Just)
                    ArgTAccountKey -> notImplemented    -- list box
                    ArgTItemID ->
                        let label = case call of
                                StarbaseDetail -> "Starbase item ID"
                                OutpostServiceDetail -> "Outpost item ID"
                                _                    -> "Item ID"
                        in  mkfield ArgItemID intField label Nothing $
                                maybe FormMissing (FormSuccess . Just)
    (argres,argview) <- argfields -- (FormResult [APIArgument], [FieldView App App])
    renderDivs (formToAForm $ return (FullCall scope call <$> keyres <*> argres,
                                       keyview <> argview)) html

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
-- Form widget is built separately to let get/post handle it differently.
doPage :: Scope -> Call -> Maybe CallArgs -> Maybe (Widget, Enctype) -> Handler RepHtml
doPage scope call maybeArgs mArgForm =
    defaultLayout $ do
        setTitle "API Calls"
        let -- can't put type annotations in the tempate :(
            argStringT = argString   :: APIArgumentType -> Text
            typeStringT = typeString :: APIDataType -> Text
        $(widgetFile "call")

-- Show list of args and form to provide them.
getCallR :: Scope -> Call -> Handler RepHtml
getCallR scope call = do
    maybeArgs <- runDB $ getArgs scope call
    mArgForm  <- sequence $ generateFormPost <$> callForm scope call <$> maybeArgs
    doPage scope call maybeArgs mArgForm

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
        maybeArgs <- runDB $ getArgs scope call
        mArgForm' <- sequence $ runFormPost <$> callForm scope call <$> maybeArgs
        let (formResult,mArgForm) =
                maybe (FormFailure ["internal error: bad call type"], Nothing)
                      (\((res,widget),et) -> (res, Just (widget, et)))
                      mArgForm'
        -- TODO: do something with formResult
        doPage scope call maybeArgs mArgForm
