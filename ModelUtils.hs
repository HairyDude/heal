module ModelUtils where

import Import
import EveApi.Types
import Data.Maybe (catMaybes)

-- Given a call and its scope, gather the KeyType and ArgSpecs and construct a
-- CallArgs.
getArgs :: Scope -> Call -> YesodDB App App (Maybe CallParams)
getArgs scope name = do
    -- Get the CallSpecID for this call
    maybeCallSpec <- getBy $ UniqueCall scope name
    case maybeCallSpec of
        Just (Entity callSpecID callSpec) -> do
            -- Get the ArgSpecIds for this call
            argSpecIDs <- map (callArgArgId . entityVal) <$>
                            selectList [CallArgCallId ==. callSpecID] []
            -- Get the actual Args
            -- XXX: instead of catMaybes, properly handle Nothing (it's "impossible")
            args <- map argSpecArgSpec . catMaybes <$> mapM get argSpecIDs
            return $ Just $ CallParams (callSpecKey callSpec) args
        Nothing -> return Nothing
