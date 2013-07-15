{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Task.Models ( Task(..) ) where

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
import Task.Views



data Task = Task {
  taskId :: Maybe ObjectId,
  taskName :: String,
  taskMembers :: [UserName],
  taskCompleted :: String
} deriving (Show, Eq, Typeable)

instance DCRecord Task where
  fromDocument doc = do
    let tid = lookupObjIdh "_id" doc
    name <- trace "41" $ lookup "name" doc
    members <- trace "42" $ lookup "members" doc
    completed <- trace "43" $ lookup "completed" doc
    trace "returning" $ return Task { taskId = tid
                , taskName = name
                , taskMembers = members
                , taskCompleted = completed }

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
                , taskCompleted = completed }
  
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
                , taskCompleted = completed }
  
  fromBsonValue _ = fail "fromHsonValue error"  

  toBsonValue t = BsonDoc 
    [ "_id"  -: taskId t
    , "name" -: taskName t
    , "members" -: taskMembers t
    , "completed" -: taskCompleted t ]


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
 
