{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Task.Models ( Task(..)
                   , User(..)
                   , Comment(..)
                   , Project(..) 
		   , lookupObjIdh
		   , lookupObjId ) 
		   where

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


data Task = Task {
  taskId :: Maybe ObjectId,
  taskName :: String,
  taskMembers :: [UserName],
  taskCompleted :: Bool,
  taskPriority :: String,
  taskProject :: ObjectId
} deriving (Show, Eq, Typeable)

instance ToJSON Task where
  toJSON (Task id name mem com pri proj) =
    object [ "_id"       .= (show $ fromJust id)
           , "name"      .= name
           , "members"   .= mem
           , "completed" .= com
           , "priority"  .= pri
           , "project"   .= show proj
           ]

data User = User {
  userId :: Maybe ObjectId,
  userName :: UserName,
  userNotifs :: [String],
  userInvites :: [ObjectId],
  userProjects :: [ObjectId]
} deriving (Show, Eq, Typeable)

instance ToJSON User where
  toJSON (User id name notifs i p) =
    object [ "_id"       .= (show $ fromJust id)
           , "name"      .= name
           , "notifs"    .= notifs
           , "invites"   .= show i
           , "projects"  .= show p
           ]

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

