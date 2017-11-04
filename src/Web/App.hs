{-# LANGUAGE OverloadedStrings #-}
module Web.App
  ( runApp )
where

import           Control.Monad.Logger        (runStdoutLoggingT)
import           Database.Persist.Postgresql hiding (delete, get)
import           Web.Spock
import           Web.Spock.Config

import           Model.CoreTypes


type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

runApp :: IO ()
runApp = do
    pool <- runStdoutLoggingT (createPostgresqlPool connStr 10)
    runStdoutLoggingT (runSqlPool (runMigration migrateCore) pool)
    spockCfg <-  defaultSpockCfg () (PCPool pool) ()
    runSpock 1337 (spock spockCfg app)
  where
    connStr = "host=localhost dbname=ideabox2 user=postgres password=postgres port=5432"

app :: Api
app = do
    get "hello" $ do
        text $ "Hello World!"
