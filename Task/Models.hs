{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Task.Models ( Task(..)
                   , User(..)
                   , Comment(..)
                   , Project(..) ) where

import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import           Data.Aeson
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
  taskPriority :: String,
  taskProject :: ObjectId
} deriving (Show, Eq, Typeable)

instance DCRecord Task where
  fromDocument doc = trace "fromDoc task" $ do
    let tid = lookupObjIdh "_id" doc
    name <- lookup "name" doc
    members <- lookup "members" doc
    completed <- trace "lookup completed" $ lookup "completed" doc
    priority <- trace "lookup priority" $ lookup "priority" doc
    project <- trace "lookup proj" $ lookup "project" doc
    trace "returning task" $ return Task { taskId = tid
                , taskName = name
                , taskMembers = members
                , taskCompleted = read completed
                , taskPriority = priority
                , taskProject = read project }

  toDocument t =
    [ "_id"  -: taskId t
    , "name" -: taskName t
    , "members" -: (taskMembers t :: [UserName])
    , "completed" -: taskCompleted t
    , "priority" -: taskPriority t
    , "project" -: taskProject t]

  recordCollection _ = "tasks"


data User = User {
  userId :: Maybe ObjectId,
  userName :: UserName,
  userNotifs :: [String],
  userTasks :: [ObjectId],
  userInvites :: [ObjectId],
  userProjects :: [ObjectId]
} deriving (Show, Eq, Typeable)

instance DCRecord User where
  fromDocument doc = trace "fromDoc user" $ do
    let uid = lookupObjIdh "_id" doc
    name <- lookup "name" doc
    notifs <- lookup "notifs" doc
    let projects = trace "lookup projs" $ at "projects" doc
    let invites = trace "lookup projs" $ at "invites" doc
    let tasks = trace "lookup tasks" $ at "tasks" doc
    trace "returning user" $ return User { userId = uid
                , userName = name
                , userNotifs = notifs
                , userTasks = tasks
                , userInvites = invites
                , userProjects = projects }

  toDocument u = trace "toDoc user" $
    [ "_id"  -: userId u
    , "name" -: userName u
    , "notifs" -: userNotifs u
    , "tasks" -: userTasks u
    , "invites" -: userInvites u
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
  projectTasks :: [ObjectId],
  projectDesc :: String
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
    tasks <- trace "lookup tasks" $ lookup "tasks" doc
    desc <- trace "lookup desc" $ lookup "desc" doc
    trace "returning project" $ return Project { projectId = pid
                   , projectTitle = title
                   , projectMembers = members
                   , projectCompleted = read completed 
                   , projectStartTime = startTime
                   , projectEndTime = endTime
                   , projectLeaders = leaders
                   , projectTasks = tasks
                   , projectDesc = desc }

  toDocument t =
    [ "_id"  -: projectId t
    , "title" -: projectTitle t
    , "members" -: (projectMembers t :: [UserName])
    , "completed" -: projectCompleted t
    , "startTime" -: projectStartTime t
    , "endTime" -: projectEndTime t
    , "leaders" -: projectLeaders t
    , "tasks" -: projectTasks t 
    , "desc" -: projectDesc t ]

  recordCollection _ = "projects"


-- Comments ----

data Comment = Comment
    { commentId        :: Maybe ObjectId
    , commentAuthor    :: UserName  -- author
    , commentAssocProj :: ObjectId -- what proj it belongs to
    , commentText :: String -- the comment body text
    , commentInReplyTo :: Maybe ObjectId  -- comment it's in reply to
    } deriving Show

instance ToJSON Comment where
  toJSON (Comment i a p t mp) =  -- id, author, proj, text, parent
    object [ "_id"    .= (show $ fromJust i)
           , "author" .= a
           , "proj"   .= (show p)
           , "text"   .= t
           , "parent" .= case mp of
                           Just p -> show p
                           _ -> ""
           ]

instance DCRecord Comment where
  fromDocument doc = do
    let cid = lookupObjId "_id" doc
    author <- lookup "author" doc
    text <- lookup "text" doc -- body text
    proj <- lookupObjId "proj" doc -- associated proj
    let parent = lookupObjId "parent" doc -- the comment it's in reply to
    return Comment { commentId = cid
                   , commentAuthor = author
                   , commentAssocProj = proj
                   , commentText = text
                   , commentInReplyTo = parent }

  toDocument c =
    let mparent = commentInReplyTo c
        parent = if isJust mparent
                   then [ "parent" -: fromJust mparent ]
                   else []
        mid = commentId c
        id = if isJust mparent
               then [ "_id" -: fromJust mid ]
               else []
    in id ++
       [ "author" -: commentAuthor c
       , "post" -: commentAssocProj c
       , "text" -: commentText c ]
       ++ parent

  recordCollection _ = "comments"

lookupObjId :: Monad m => FieldName -> HsonDocument -> m ObjectId
lookupObjId n d = case lookup n d of
    Just i -> return (i :: ObjectId)
    _ -> case do { s <- lookup n d; maybeRead s } of
          Just i -> return i
          _ -> fail $ "lookupObjId: cannot extract id from " ++ show n
  where maybeRead = fmap fst . listToMaybe . reads


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

