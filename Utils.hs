module Utils
    (
        maybeRead       -- :: Read a => String -> Maybe a
    ,   nothingToNull   -- :: Maybe [a] -> [a]
    )
where

import Prelude (Read (..), String, Maybe (..), reads)

-- utility that should be in Prelude, IMO
maybeRead :: Read a => String -> Maybe a
maybeRead s | [(x,"")] <- reads s = Just x
maybeRead _ = Nothing

-- another one
nothingToNull :: Maybe [a] -> [a]
nothingToNull Nothing  = []
nothingToNull (Just l) = l
