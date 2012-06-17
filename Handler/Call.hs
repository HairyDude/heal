module Handler.Call where

import Import

import EveApiTypes
import EveApiValues
import ModelUtils

import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad (when)

getCallR :: Scope -> Call -> Handler RepHtml
getCallR scope call = do
    maybeArgs <- runDB $ getArgs scope call
    defaultLayout $ do
        setTitle "API Calls"
        let -- can't put type annotations in the tempate :(
            scp = scopeString scope  :: Text
            argStringT = argString   :: APIArgumentType -> Text
            typeStringT = typeString :: APIDataType -> Text
        $(widgetFile "call")

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

postCallR :: Scope -> Call -> Handler RepHtml
postCallR scope call = do
    populateString <- runInputPost $ ireq hiddenField "PopulateCallDB"
    let populate = maybeRead populateString
    if (isJust populate && fromJust populate)
    then do
        populateCallDB
        redirect (CallR scope call)
    else getCallR scope call
