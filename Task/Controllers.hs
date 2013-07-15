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

import Task.Policy
import Task.Models
import Task.Views

server :: Application
server = mkRouter $ do

  get "/" $ withUserOrDoAuth $ \user -> respond $ okHtml $ L8.pack $ displayPage user
   

  post "/task" $ do   
    taskdoc <- include ["name", "members", "completed"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    let members = splitOn (" " :: String) ("members" `at` taskdoc)
    let task = merge ["members" -: (members :: [String])] taskdoc 
    alldocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "tasks"
    let memDocs = filter (\u -> ("user" `at` u) `elem` members) alldocs
    liftLIO $ withTaskPolicyModule $ addTasks memDocs $ fromDocument task
    respond $ redirectTo "/"   

addTasks memDocs task = do
  if memDocs == []
    then return ()
    else do
      let doc = head memDocs
      let curTasks = "tasks" `at` doc
      let newTasks = task:curTasks
      let newDoc = merge ["tasks" -: newTasks] doc
      save "tasks" newDoc
      addTasks (tail memDocs) task

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

getTasks :: UserName -> DC String
getTasks user = withTaskPolicyModule $ do
  musr <- findOne $ select ["user" -: user] "tasks"
  case musr of 
    Nothing -> do
      let doc = ["user" -: user, "tasks" -: ([] :: [Task]), "coworkers" -: ([] :: [UserName])] :: HsonDocument
      insert "tasks" doc
      return ""
    (Just usr) -> trace "Just" $ do 
      us <- findOne $ select ["user" -: user] "tasks"
      u <- liftLIO $ unlabel $ fromJust us
      let tasks = filter (\t -> not $ taskCompleted t) ("tasks" `at` u)
      trace "slist" $ return $ slist tasks ""
              where slist :: [Task] -> String -> String
                    slist taskList str = if (taskList == [])
                    then str
                    else do
                      n <-  lookup "name" $ toDocument (head taskList)
                      m <- lookup  "members" $ toDocument (head taskList)
                      slist (tail taskList) (("<li class=\"status\" id=\"" ++ n ++ "\">" ++ (n ++ " members: " ++ (show m) ++ "</li>") ++ str))

