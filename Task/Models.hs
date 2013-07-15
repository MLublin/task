{-# LANGUAGE OverloadedStrings #-}
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

import Task.Policy
import Task.Views


data Task = Task {
  taskId :: Maybe ObjectId,
  taskName :: String,
  taskMembers :: [UserName],
  taskCompleted :: Bool
} deriving (Show, Eq)

instance DCRecord Task where
  fromDocument doc = do
    let tid = lookupObjId "_id" doc
    name <- lookup "name" doc
    members <- lookup "members" doc
    completed <- lookup "completed" doc
    return Task { taskId = tid
                , taskName = name
                , taskMembers = members
                , taskCompleted = completed }

  toDocument t =
    [ "_id"  -: taskId t
    , "name" -: taskName t
    , "members" -: taskMembers t
    , "completed" -: taskCompleted t ]

  recordCollection _ = "tasks"


lookupObjId :: Monad m => FieldName -> HsonDocument -> m ObjectId
lookupObjId n d = case lookup n d of
    Just i -> return (i :: ObjectId)
    _ -> case do { s <- lookup n d; maybeRead s } of
          Just i -> return i
          _ -> fail $ "lookupObjId: cannot extract id from " ++ show n
  where maybeRead = fmap fst . listToMaybe . reads
 
