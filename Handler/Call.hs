module Handler.Call where

import Import
import EveApiTypes
import EveApiValues

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M

getCallR :: Scope -> Call -> Handler RepHtml
getCallR scope call = defaultLayout $ do
    setTitle "API Calls"
    let scp = scopeString scope :: Text
        args = M.lookup (call, scope) apiCalls
        -- can't put type annotations in the tempate :(
        argStringT = argString :: APIArgumentType -> Text
        typeStringT = typeString :: APIDataType -> Text
    $(widgetFile "call")
