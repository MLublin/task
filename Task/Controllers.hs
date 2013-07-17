{-# LANGUAGE OverloadedStrings #-}
module Task.Controllers where
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
import Hails.Database.Structured hiding (findAll)
import LIO
import LIO.DCLabel
import Data.Maybe
import Data.List.Split
import Text.Blaze.Renderer.Text
import Text.Blaze.Html5 hiding (Tag, map, head, select)

import qualified Web.Simple.Responses as W

import Task.Policy
import Task.Models
import Task.Views

server :: Application
server = mkRouter $ do

  get "/" $ withUserOrDoAuth $ \user -> trace ("user logged in: " ++ T.unpack user) $ do
    musr <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
    case musr of
      Nothing -> trace "user not found" $ do
        respond $ respondHtml "Tasks" $ trace "creating new user" $ newUser user
      Just usr -> trace "found user" $ do
        unlabeled <- liftLIO $ unlabel usr
        u <- fromDocument unlabeled -- returns a User
        let tids = userTasks u
        mtasks <- liftLIO $ withTaskPolicyModule $ mapM (findBy "tasks" "_id") tids 
        let tasks = map fromJust mtasks
        respond $ respondHtml "Tasks" $ displayPage user tasks

  post "/people" $ trace "post /people" $ do
    userdoc <- include ["name", "tasks"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    liftLIO $ withTaskPolicyModule $ trace "line 49" $ insert "users" userdoc  
    respond $ redirectTo "/" 
 
  get "/people" $ do
    people <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let users =  map (\u -> "name" `at` u) people
    respond $ respondHtml "Users" $ showUsers users    

  post "/task" $ trace "Post/Task" $ do   
    taskdoc <- include ["name", "members", "completed"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    let members = trace "line 63" $ splitOn (" " :: String) ("members" `at` taskdoc)
    let task = trace "line 64" $ merge ["members" -: (members :: [String])] taskdoc 
    tId <- liftLIO $ withTaskPolicyModule $ insert "tasks" task
    alldocs <- liftLIO $ withTaskPolicyModule $ trace "line 66" $ findAll $ select [] "users"
    let memDocs = trace (show alldocs) $ filter (\u -> ("name" `at` u) `elem` members) alldocs
    liftLIO $ withTaskPolicyModule $ trace "addTasks" $ addTasks memDocs tId
    respond $ redirectTo "/"   

addTasks :: [HsonDocument] -> ObjectId -> DBAction ()
addTasks memDocs taskId = trace "addTasks called" $ do
  if memDocs == []
    then return ()
    else do
      let doc = head memDocs
      let curTasks = "tasks" `at` doc
      let newTasks = taskId:curTasks
      let newDoc = merge ["tasks" -: newTasks] doc
      save "users" newDoc
      trace (show $ length memDocs) $ addTasks (tail memDocs) taskId

findAll :: Query -> DBAction [HsonDocument]
findAll q = do
        cur <- find q
        getAll cur []
        where getAll cur list = do
              mldoc <- next cur
              case mldoc of
                Nothing -> return list
                Just ldoc -> do
                        doc <- liftLIO $ unlabel ldoc
                        getAll cur (list ++ [doc])

