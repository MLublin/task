{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Task.Models ( Task(..)
                   , User(..)
                   , Project(..) ) where

import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

import Control.Monad
import Debug.Trace

import Hails.HttpServer hiding (Query)
import Hails.Web
import Hails.Web.Frank
import Hails.Web.Controller
import Hails.Data.Hson
import Hails.Web.User
import Hails.Database
import Hails.Database.Structured
import LIO
import LIO.DCLabel
import Data.Maybe
import Data.Time.Clock
import Data.Typeable

import Task.Policy


data Task = Task {
  taskId :: Maybe ObjectId,
  taskName :: String,
  taskMembers :: [UserName],
  taskCompleted :: Bool,
  taskProject :: ObjectId
} deriving (Show, Eq, Typeable)

instance DCRecord Task where
  fromDocument doc = do
    let tid = lookupObjIdh "_id" doc
    name <- lookup "name" doc
    members <- lookup "members" doc
    completed <- lookup "completed" doc
    project <- lookup "project" doc
    return Task { taskId = tid
                , taskName = name
                , taskMembers = members
                , taskCompleted = read completed
                , taskProject = read project }

  toDocument t =
    [ "_id"  -: taskId t
    , "name" -: taskName t
    , "members" -: (taskMembers t :: [UserName])
    , "completed" -: taskCompleted t
    , "project" -: taskProject t]

  recordCollection _ = "tasks"


data User = User {
  userId :: Maybe ObjectId,
  userName :: UserName,
  userTasks :: [ObjectId],
  userProjects :: [ObjectId]
} deriving (Show, Eq, Typeable)

instance DCRecord User where
  fromDocument doc = trace "fromDoc user" $ do
    let uid = lookupObjIdh "_id" doc
    name <- lookup "name" doc
    let projects = trace "lookup projs" $ at "projects" doc
    let tasks = trace "lookup tasks" $ at "tasks" doc
    trace "returning user" $ return User { userId = uid
                , userName = name
                , userTasks = tasks
                , userProjects = projects }

  toDocument u = trace "toDoc user" $
    [ "_id"  -: userId u
    , "name" -: userName u
    , "tasks" -: userTasks u 
    , "projects" -: userProjects u]

  recordCollection _ = "tasks"


data Project = Project {
  projectId :: Maybe ObjectId,
  projectTitle :: String,
  projectMembers :: [UserName],
  projectCompleted :: Bool,
  projectStartTime :: String,
  projectEndTime :: String,
  projectLeaders :: [UserName],
  projectTasks :: [ObjectId]
} deriving (Show, Eq, Typeable)

instance DCRecord Project where
  fromDocument doc = do
    let pid = lookupObjIdh "_id" doc
    title <- lookup "title" doc
    members <- lookup "members" doc
    completed <- lookup "completed" doc
    startTime <- lookup "startTime" doc
    endTime <- lookup "endTime" doc
    leaders <- lookup "leaders" doc
    tasks <- lookup "tasks" doc
    return Project { projectId = pid
                , projectTitle = title
                , projectMembers = members
                , projectCompleted = read completed 
                , projectStartTime = startTime
                , projectEndTime = endTime
                , projectLeaders = leaders
                , projectTasks = tasks }

  toDocument t =
    [ "_id"  -: projectId t
    , "title" -: projectTitle t
    , "members" -: (projectMembers t :: [UserName])
    , "completed" -: projectCompleted t
    , "startTime" -: projectStartTime t
    , "endTime" -: projectEndTime t
    , "leaders" -: projectLeaders t
    , "tasks" -: projectTasks t ]

  recordCollection _ = "projects"


lookupObjIdh :: Monad m => FieldName -> HsonDocument -> m ObjectId
lookupObjIdh n d = case lookup n d of
    Just i -> return (i :: ObjectId)
    _ -> case do { s <- lookup n d; maybeRead s } of
          Just i -> return i
          _ -> fail $ "lookupObjId: cannot extract id from " ++ show n
  where maybeRead = fmap fst . listToMaybe . reads
 
lookupObjIdb :: Monad m => FieldName -> BsonDocument -> m ObjectId
lookupObjIdb n d = case lookup n d of
    Just i -> return (i :: ObjectId)
    _ -> case do { s <- lookup n d; maybeRead s } of
          Just i -> return i
          _ -> fail $ "lookupObjId: cannot extract id from " ++ show n
  where maybeRead = fmap fst . listToMaybe . reads

