module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeApplication, makeFoundation, makeLogWare,
                              warpSettings)
import Model                 as X
import Settings              as X (AppSettings(..))

import ClassyPrelude         as X hiding (assert, delete, deleteBy)
import Control.Concurrent.Async
import Data.FileEmbed
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool,
                              rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Network.Wai.Handler.Warp (setBeforeMainLoop, setPort)
import Network.Wai.Handler.WarpTLS (TLSSettings(..), tlsSettingsMemory, runTLS)
import Test.Hspec            as X
import Test.QuickCheck       as X hiding (label)
import Test.QuickCheck.Monadic as X
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

myTlsSettings :: TLSSettings
myTlsSettings = tlsSettingsMemory
  $(embedFile "test/cert.crt") $(embedFile "test/key.pem")

withServerM :: ReaderT (TestApp App) WD a -> IO a
withServerM test = do
  -- Start warp-tls, wait for it to be ready, run the test and let async kill
  -- the server for us. If an exception is thrown by the server thread, make
  -- sure it's rethrown in the test thread.
  waitVar <- newEmptyMVar
  appTuple@(foundation, _logWare) <- liftIO $ setupApp
  app <- makeApplication foundation
  let warpSettings' = setBeforeMainLoop (putMVar waitVar ()) $ setPort 3443 $
        warpSettings foundation
  withAsync (runTLS myTlsSettings warpSettings' app) $ \a -> do
    link a
    takeMVar waitVar
    runSession defaultConfig $ finallyClose $ runReaderT test appTuple

wdProperty :: PropertyM (ReaderT (TestApp App) WD) a -> Property
wdProperty = monadic wdPropToProp
  where wdPropToProp :: ReaderT (TestApp App) WD Property -> Property
        wdPropToProp prop = ioProperty $ withServerM prop

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
