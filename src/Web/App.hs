{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.App
  ( runApp )
where

import           Control.Monad.Logger          (NoLoggingT, runNoLoggingT,
                                                runStdoutLoggingT)
import           Control.Monad.Trans.Resource  (ResourceT, runResourceT)
import           Data.Aeson                    hiding (json)
import           Database.Persist.Postgresql   hiding (delete, get)
import           Network.Wai.Middleware.Static
import           Web.Spock
import           Web.Spock.Config

import           Model.CoreTypes
import qualified Web.Actions                   as A


type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

runApp :: IO ()
runApp = do
    pool <- runStdoutLoggingT (createPostgresqlPool connStr 10)
    runStdoutLoggingT (runSqlPool (runMigration migrateCore) pool)
    spockCfg <- defaultSpockCfg () (PCPool pool) ()
    runSpock 1337 (spock spockCfg app)
  where
    connStr = "host=localhost dbname=ideabox2 user=postgres password=postgres port=5432"

app :: Api
app = do
    middleware (staticPolicy (only [("css/app.css", "static/css/app.css"), ("js/app.js", "static/js/app.js")]))
    get "" $
        file "" "static/app.html"
    get "api/ideas" showIdeas
    get ("api/ideas" <//> var) showIdea
    post "api/ideas" createIdea

-- Idea Handlers
showIdeas :: ApiAction ()
showIdeas = runSQL A.allIdeas >>= json

showIdea :: IdeaId -> ApiAction ()
showIdea ideaId = do
    maybeIdea <- runSQL (A.getIdea ideaId)
    case maybeIdea of
        Nothing      -> json (object ["error" .= String "no such entity"])
        Just theIdea -> json theIdea

createIdea :: ApiAction ()
createIdea = do
    maybeIdea <- jsonBody
    case maybeIdea of
        Nothing -> json (object ["error" .= String "malformed entity"])
        Just thePerson -> do
            newId <- runSQL $ A.createIdea thePerson
            json $ object ["id" .= newId]



-- Some Util stuff
runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action = runQuery $ runResourceT . runNoLoggingT . runSqlConn action
