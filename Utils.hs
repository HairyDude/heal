module Utils
    (
        maybeRead   -- :: Read a => String -> Maybe a
    )
where

import Prelude (Read (..), String, Maybe (..), reads)

-- utility that should be in Prelude, IMO
maybeRead :: Read a => String -> Maybe a
maybeRead s | [(x,"")] <- reads s = Just x
maybeRead _ = Nothing
