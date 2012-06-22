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
callForm scope call callargs = do
    let CallArgs keyType args = callargs
        moptreq :: RenderMessage App FormMessage =>
                    Bool -> Field App App a -> FieldSettings App -> Maybe a
                         -> MForm App App (FormResult (Maybe a),
                                              FieldView App App)
        moptreq req field settings def =
                    if req
                    then (\(r,w) -> (Just <$> r,w)) <$>
                            mreq field settings def
                    else mopt field settings (Just def)
        keyfields :: AForm App App (Maybe Key)
        keyfields = formToAForm $ case keyType of
            NoKey -> return (FormSuccess (Just KeyNone), [])
            _     -> do
                let req = keyOpt keyType == Mandatory
                (frkeyid, widgkeyid) <- moptreq req intField "Key ID" Nothing
                (frvcode, widgvcode) <- moptreq req textField "vCode" Nothing
                let fields :: [FieldView App App]
                    fields = [widgkeyid, widgvcode]
                    formFail mesgs = return (FormFailure mesgs, fields)
                case (frkeyid, frvcode) of
                    (FormSuccess mkeyid, FormSuccess mrawvcode) ->
                        -- TODO: validate vCode
                        if isNothing mkeyid || isNothing mrawvcode
                        then if req -- no key provided, but do we need one?
                            then return (FormFailure ["This call requires a key"]
                                        ,fields)
                            else return (FormSuccess Nothing, fields)
                        else return
                            (FormSuccess
                                (Just $ Key (fromJust mkeyid)
                                            (fromJust mrawvcode)
                                            UnknownKeyScope)
                            ,fields)
                    (FormFailure failkey, FormFailure failvcode) ->
                        formFail (failkey ++ failvcode)
                    (FormFailure failkey, _) ->
                        formFail failkey
                    (_, FormFailure failvcode) ->
                        formFail failvcode
                    _ -> formFail []
        argfields :: AForm App App [APIArgument]
        argfields = formToAForm $ do
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
                    ArgTCharacterID -> notImplemented   -- int, may be FromKey
                    ArgTCorporationID -> notImplemented -- int, may be FromKey
                    ArgTEventIDs -> parseListField ArgIDs
                                    "List of event IDs (comma-separated)"
                                    "Invalid event ID list"
                                    decimal
                                    Nothing
                    ArgTContractID ->
                        mkfield ArgContractID intField "Contract ID" Nothing $
                            maybe FormMissing (FormSuccess . Just)
                    ArgTBeforeKillID -> notImplemented  -- int
                    ArgTOrderID -> notImplemented       -- int
                    ArgTExtended -> notImplemented      -- for corp member tracking
                        {-mfield checkBoxField "Extended information" Nothing-}
                    ArgTRowCount -> notImplemented      -- int
                    ArgTFromID -> notImplemented        -- int
                    ArgTAccountKey -> notImplemented    -- list box
                    ArgTItemID -> notImplemented        -- int
    renderDivs $ FullCall scope call
        <$> keyfields
        <*> argfields

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
    let populate | Just populate <- join $ maybeRead <$> populateString = populate
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
