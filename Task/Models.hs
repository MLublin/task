{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Task.Models ( Task(..), User(..) ) where

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
import Data.Typeable

import Task.Policy


data Task = Task {
  taskId :: Maybe ObjectId,
  taskName :: String,
  taskMembers :: [UserName],
  taskCompleted :: Bool
} deriving (Show, Eq, Typeable)

instance DCRecord Task where
  fromDocument doc = do
    let tid = lookupObjIdh "_id" doc
    name <- lookup "name" doc
    members <- lookup "members" doc
    completed <- lookup "completed" doc
    return Task { taskId = tid
                , taskName = name
                , taskMembers = members
                , taskCompleted = read completed }

  toDocument t =
    [ "_id"  -: taskId t
    , "name" -: taskName t
    , "members" -: (taskMembers t :: [UserName])
    , "completed" -: taskCompleted t ]

  recordCollection _ = "tasks"

instance HsonVal Task where
  fromHsonValue (HsonValue (BsonDoc doc)) = do
    let tid = lookupObjIdb "_id" doc  
    name <- lookup "name" doc
    members <- lookup "members" doc
    completed <- lookup "completed" doc
    return Task { taskId = tid
                , taskName = name
                , taskMembers = members 
                , taskCompleted = read completed }
  
  fromHsonValue _ = fail "fromHsonValue error"  

  toHsonValue t = HsonValue $ BsonDoc 
    [ "_id"  -: taskId t
    , "name" -: taskName t
    , "members" -: taskMembers t
    , "completed" -: taskCompleted t ]

instance BsonVal Task where
  fromBsonValue (BsonDoc doc) = do
    let tid = lookupObjIdb "_id" doc  
    name <- lookup "name" doc
    members <- lookup "members" doc
    completed <- lookup "completed" doc
    return Task { taskId = tid
                , taskName = name
                , taskMembers = members 
                , taskCompleted = read completed }
  
  fromBsonValue _ = fail "fromHsonValue error"  

  toBsonValue t = BsonDoc 
    [ "_id"  -: taskId t
    , "name" -: taskName t
    , "members" -: taskMembers t
    , "completed" -: taskCompleted t ]

data User = User {
  userId :: Maybe ObjectId,
  userName :: UserName,
  userTasks :: [ObjectId]
} deriving (Show, Eq, Typeable)

instance DCRecord User where
  fromDocument doc = trace "fromDoc user" $ do
    let uid = lookupObjIdh "_id" doc
    name <- lookup "name" doc
    let tasks = at "tasks" doc
    return User { userId = uid
                , userName = name
                , userTasks = tasks }

  toDocument u = trace "toDoc user" $
    [ "_id"  -: userId u
    , "name" -: userName u
    , "tasks" -: userTasks u ]

  recordCollection _ = "tasks"

instance HsonVal User where
  fromHsonValue (HsonValue (BsonDoc doc)) = do
    let tid = lookupObjIdb "_id" doc
    name <- lookup "name" doc
    let tasks = lookupObjIdb "tasks" doc
    return User { userId = tid
                , userName = name
                , userTasks = tasks }
  
  fromHsonValue _ = fail "fromHsonValue error"  

  toHsonValue u = HsonValue $ BsonDoc 
    [ "_id"  -: userId u
    , "name" -: userName u
    , "tasks" -: userTasks u ]

instance BsonVal User where
  fromBsonValue (BsonDoc doc) = do
    let tid = lookupObjIdb "_id" doc
    name <- lookup "name" doc
    otasks <- lookup "tasks" doc
    let stasks = map show otasks
    let tasks = map (\t -> read t :: ObjectId) otasks
    return User { userId = tid
                , userName = name
                , userTasks = tasks }
  
  fromBsonValue _ = fail "fromBsonValue error"

  toBsonValue t = BsonDoc
    [ "_id"  -: userId t
    , "name" -: userName t
    , "tasks" -: userTasks t ]


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
 
