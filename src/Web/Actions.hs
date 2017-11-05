{-# LANGUAGE OverloadedStrings #-}
module Web.Actions where

import           Database.Persist
import           Database.Persist.Sql

import           Model.CoreTypes


getIdea :: IdeaId -> SqlPersistM (Maybe (Entity Idea))
getIdea ideaId = getEntity ideaId

createIdea :: Idea -> SqlPersistM IdeaId
createIdea = insert

allIdeas :: SqlPersistM [Entity Idea]
allIdeas = selectList [] [Asc IdeaId]
