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
 
-- Projects ----

  get "/" $ withUserOrDoAuth $ \user -> do
    musr <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
    case musr of
      Nothing -> trace "user not found" $ do
        respond $ respondHtml "Projects" $ trace "creating new user" $ newUser user
      Just usr -> trace ("line 41 ") $ do
        unlabeled <- liftLIO $ unlabel usr
        u <- fromDocument unlabeled -- returns a User
        let pids = userProjects u
        mprojects <- liftLIO $ withTaskPolicyModule $ trace (show pids) $  mapM (findBy "projects" "_id") pids 
        let projects = map fromJust mprojects
        respond $ respondHtml "Projects" $ displayHomePage user projects

  get "/projects/:pid" $ withUserOrDoAuth $ \user -> trace ("user logged in: " ++ T.unpack user) $ do
    pid <- queryParam "pid"
    mpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    case mpdoc of
      Nothing -> respond notFound 
      Just pdoc -> do
        proj <- (liftLIO . unlabel pdoc) >>= fromDocument
        if not $ user `elem` (projectMembers proj)
          then respond $ redirectTo "/"
          else do
            mudoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
            unlabeled <- liftLIO $ unlabel $ fromJust mudoc
            u <- fromDocument unlabeled -- returns a User
            let tids = projectTasks proj
            mtasks <- liftLIO $ withTaskPolicyModule $ mapM (findBy "tasks" "_id") tids 
            let tasks = trace (show mtasks) $ map fromJust mtasks
            respond $ respondHtml "Tasks" $ displayProjectPage u tasks proj
 
  get "/projects/new" $ withUserOrDoAuth $ \user -> do
    respond $ respondHtml "newProject" $ newProject user

  post "/projects" $ do
    pdoc <- include ["title", "members", "completed", "startTime", "endTime", "leaders", "tasks"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    let members = splitOn (" " :: String) ("members" `at` pdoc)
    let leaders = splitOn (" " :: String) ("leaders" `at` pdoc)
    let project = trace "line 64" $ merge ["members" -: (members :: [String]), "leaders" -: (leaders :: [String])] pdoc 
    pId <- liftLIO $ withTaskPolicyModule $ insert "projects" project
    alldocs <- liftLIO $ withTaskPolicyModule $ trace "line 66" $ findAll $ select [] "users"
    let memDocs = trace (show alldocs) $ filter (\u -> ("name" `at` u) `elem` members) alldocs
    liftLIO $ withTaskPolicyModule $ trace "addProjects" $ addProjects memDocs pId
    respond $ redirectTo "/projects/" ++ show pId 


-- Tasks -----

  post "/task" $ trace "Post/Task" $ do   
    taskdoc <- include ["name", "members", "completed"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    let members = trace "line 63" $ splitOn (" " :: String) ("members" `at` taskdoc)
    let task = trace "line 64" $ merge ["members" -: (members :: [String])] taskdoc 
    tId <- liftLIO $ withTaskPolicyModule $ insert "tasks" task
    alldocs <- liftLIO $ withTaskPolicyModule $ trace "line 66" $ findAll $ select [] "users"
    let memDocs = trace (show alldocs) $ filter (\u -> ("name" `at` u) `elem` members) alldocs
    liftLIO $ withTaskPolicyModule $ trace "addTasks" $ addTasks memDocs tId
    respond $ redirectTo "/"   


-- Users -----

  post "/people" $ trace "post /people" $ do
    userdoc <- include ["name", "tasks"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    let usrdoc = merge ["tasks" -: ([] :: [ObjectId])] userdoc
    liftLIO $ withTaskPolicyModule $ trace "line 49" $ insert "users" usrdoc
    respond $ redirectTo "/"
 
  get "/people" $ do
    people <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let users =  map (\u -> "name" `at` u) people
    respond $ respondHtml "Users" $ showUsers users


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

addProjects :: [HsonDocument] -> ObjectId -> DBAction ()
addProjects memDocs pId = do
  if memDocs == []
    then return ()
    else do
      let doc = head memDocs
      let curProjects = "projects" `at` doc
      let newProjects = pId:curProjects
      let newDoc = merge ["projects" -: newProjects] doc
      save "users" newDoc
      trace (show $ length memDocs) $ addProjects (tail memDocs) pId

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

