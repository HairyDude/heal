-- mkYesodDispatch creates orphan instances and shadowing variables
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
--import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback, logCallbackDev)
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Call

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
-- This generates an orphan "instance YesodDispatch App App" which provokes a
-- warning unless we turn them off, hence the pragma.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplication conf logger = do
    foundation <- makeFoundation conf setLogger
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    setLogger = if development then logger else toProduction logger
    logWare   = if development then logCallbackDev (logBS setLogger)
                               else logCallback    (logBS setLogger)

makeFoundation :: AppConfig DefaultEnv Extra -> Logger -> IO App
makeFoundation conf setLogger = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
    return $ App conf setLogger s p manager dbconf

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
