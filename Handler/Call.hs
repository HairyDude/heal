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
    [whamlet|
<h1>API Calls
<p>You're asking for the call #{T.pack $ show call} in scope #{scp}.
$maybe CallArgs keytype arglist <- args
    $if null arglist
        <p>This call takes no arguments.
    $else
        <p>This call takes the following arguments:
        <ul>
        $forall Arg name type _ <- arglist
            <li>#{argStringT name} of type #{typeStringT type}
$nothing
    <p>Oops! I forgot to put this call in the list!
|]
