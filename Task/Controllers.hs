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

  get "/" $ withUserOrDoAuth $ \user -> do
    musr <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
    case musr of
      Nothing -> trace "line 35" $ do 
        respond $ okHtml $ L8.pack $ newUser user
      Just usr -> do
        unlabeled <- liftLIO $ unlabel usr
        u <- fromDocument unlabeled -- return of type User
        let tids = userTasks u -- type ObjectId
        mtasks <- liftLIO $ withTaskPolicyModule $ mapM (findBy "tasks" "_id") tids 
        --tdocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "tasks"
        --let alltasks = filter (extractTasks tids) tdocs
        let tasks = map fromJust mtasks
        respond $ okHtml $ L8.pack $ displayPage user tasks

  post "/people" $ do
    userdoc <- include ["name", "tasks"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    liftLIO $ withTaskPolicyModule $ trace "line 49" $ insert "users" userdoc  
    respond $ redirectTo "/" 
 
  get "/people" $ do
    people <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let users =  map (\u -> "name" `at` u) people
    respond $ okHtml $ L8.pack $ showUsers users    

  post "/task" $ trace "Post/Task" $ do   
    taskdoc <- include ["name", "members", "completed"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    let members = trace "line 49" $ splitOn (" " :: String) ("members" `at` taskdoc)
    let task = trace "line 50" $ merge ["members" -: (members :: [String])] taskdoc 
    tId <- liftLIO $ withTaskPolicyModule $ insert "tasks" task
    alldocs <- liftLIO $ withTaskPolicyModule $ trace "line 51" $ findAll $ select [] "users"
    let memDocs = trace (show alldocs) $ filter (\u -> ("name" `at` u) `elem` members) alldocs
    --ntask <- trace "line 54" $ fromDocument task
    liftLIO $ withTaskPolicyModule $ trace "addTasks" $ addTasks memDocs tId
    respond $ redirectTo "/"   
{-
loop :: [ObjectId] -> [Task] -> [Task]
loop tids tasks =  
    if tids == []     
      then tasks   
      else do
        mtask <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: (head tids)] "tasks"
        taskdoc <- unlabel $ fromJust $ mtask 
        task <- fromDocument taskdoc
        loop (tail tids) (task:tasks)
-}
addTasks :: [HsonDocument] -> ObjectId -> DBAction ()
addTasks memDocs taskId = do
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


{-
getTasks :: UserName -> DC String
getTasks user = withTaskPolicyModule $ do
  musr <- findOne $ select ["user" -: user] "users"
  case musr of 
    Nothing -> do
      let doc = ["user" -: user, "tasks" -: ([] :: [Task]), "coworkers" -: ([] :: [UserName])] :: HsonDocument
      insert "users" doc
      return ""
    (Just usr) -> trace "Just" $ do 
      us <- findOne $ select ["user" -: user] "users"
      u <- liftLIO $ unlabel $ fromJust us
      let tasks = filter (\t -> (taskCompleted t) == "False") ("tasks" `at` u)
      trace "slist" $ return $ slist tasks ""
              where slist :: [Task] -> String -> String
                    slist taskList str = if (taskList == [])
                    then str
                    else do
                      n <-  lookup "name" $ toDocument (head taskList)
                      m <- (lookup  "members" $ toDocument (head taskList)) :: [UserName]
                      slist (tail taskList) (("<li class=\"status\" id=\"" ++ n ++ "\">" ++ (n ++ " members: " ++ (show m) ++ "</li>") ++ str))
-}

