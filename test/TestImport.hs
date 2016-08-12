module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeApplication, makeFoundation, makeLogWare,
                              warpSettings)
import Model                 as X
import Settings              as X (AppSettings(..))

import ClassyPrelude         as X hiding (delete, deleteBy)
import Data.FileEmbed
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool,
                              rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Network.Wai.Handler.Warp (setPort)
import Network.Wai.Handler.WarpTLS (tlsSettingsMemory, runTLS)
import Test.Hspec            as X
import Test.WebDriver as WD
import Text.Shakespeare.Text (st)
import Yesod.Auth            as X (Route(..))
import Yesod.Default.Config2 as X (useEnv, loadYamlSettings)
import Yesod.Test            as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

runDB' :: SqlPersistM a -> ReaderT (TestApp App) WD a
runDB' q = ask >>= \(app, _) -> liftIO $ runDBWithApp app q

withApp :: SpecWith (TestApp App) -> Spec
withApp = before setupApp

withServerM :: ReaderT (TestApp App) WD () -> Expectation
withServerM a = do
  appTuple@(foundation, _logWare) <- setupApp
  app <- makeApplication foundation
  -- When using yesod-devel, reverse proxying is used for TLS. We need our own
  -- warp-tls server here.
  let myTlsSettings = tlsSettingsMemory
        $(embedFile "test/cert.crt") $(embedFile "test/key.pem")
  bracket
    (fork $ runTLS myTlsSettings (setPort 3443 $ warpSettings foundation) app)
    killThread
    (const $ runSession defaultConfig $ finallyClose $ runReaderT a appTuple)

setupApp :: IO (TestApp App)
setupApp = do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables
