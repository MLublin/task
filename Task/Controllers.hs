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
import Data.Time.Clock
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
 
  get "/projects/new" $ trace "/projects/new" $ withUserOrDoAuth $ \user -> do
    respond $ respondHtml "newProject" $ trace "newProject" $ newProject user

  post "/projects/edit" $ do
    pdoc <- include ["_id", "title", "desc", "members", "completed", "startTime", "endTime", "leaders"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    let pId = read (drop 5 $ at "_id" pdoc) :: ObjectId
    mloldproj <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pId] "projects"
    oldproj <- liftLIO $ unlabel $ fromJust mloldproj
    let members = splitOn (" " :: String) ("members" `at` pdoc)
    let leaders = splitOn (" " :: String) ("leaders" `at` pdoc)
    let project = merge [ "members" -: (members :: [String])
                        , "leaders" -: (leaders :: [String])
                        , "_id" -: pId
                        , "tasks" -: (("tasks" `at` oldproj) :: [ObjectId]) ] pdoc 
    liftLIO $ withTaskPolicyModule $ save "projects" project
    alldocs <- liftLIO $ withTaskPolicyModule $ trace "line 66" $ findAll $ select [] "users"
    let memDocs = trace (show alldocs) $ filter (\u -> ("name" `at` u) `elem` members) alldocs
    liftLIO $ withTaskPolicyModule $ trace "addProjects" $ addProjects memDocs pId
    respond $ redirectTo ("/projects/" ++ show pId)
     
  get "/projects/:pid" $ withUserOrDoAuth $ \user -> trace ("user logged in: " ++ T.unpack user) $ do
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    mpdoc <- trace ("pid: " ++ show pid) $ liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    case mpdoc of
      Nothing -> trace "proj not found" $ respond notFound 
      Just pdoc -> do
        proj <- trace "62" $ (liftLIO $ unlabel pdoc) >>= fromDocument
        if not $ user `elem` (projectMembers proj)
          then trace "64" $ respond $ redirectTo "/"
          else do
            mudoc <- trace "66" $ liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
            let udoc = fromJust mudoc
            unlabeled <- trace "68" $ liftLIO $ unlabel udoc
            u <- fromDocument unlabeled
            let tids = projectTasks proj
            mtasks <- trace ("tids: " ++ show tids) $ liftLIO $ withTaskPolicyModule $ mapM (findBy "tasks" "_id") tids 
            let tasks = trace (show mtasks) $ map fromJust mtasks
            respond $ respondHtml "Tasks" $ displayProjectPage u tasks proj

  post "/projects" $ do
    pdoc <- include ["title", "desc", "members", "completed", "startTime", "endTime", "leaders", "tasks"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    let members = splitOn (" " :: String) ("members" `at` pdoc)
    let leaders = splitOn (" " :: String) ("leaders" `at` pdoc)
    let project = merge [ "members" -: (members :: [String])
                        , "leaders" -: (leaders :: [String])
                        , "tasks" -: ([] :: [ObjectId])] pdoc 
    pId <- liftLIO $ withTaskPolicyModule $ insert "projects" project
    alldocs <- liftLIO $ withTaskPolicyModule $ trace "line 66" $ findAll $ select [] "users"
    let memDocs = trace (show alldocs) $ filter (\u -> ("name" `at` u) `elem` members) alldocs
    liftLIO $ withTaskPolicyModule $ trace "addProjects" $ addProjects memDocs pId
    respond $ redirectTo ("/projects/" ++ show pId)

  get "/projects/:pid/edit" $ withUserOrDoAuth $ \user -> do
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId 
    mpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    proj <- (liftLIO $ unlabel $ fromJust mpdoc) >>= fromDocument
    respond $ respondHtml "Edit" $ editProject proj user 
-- Tasks -----

  post "/projects/:pid/tasks" $ trace "Post/Task" $ do
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    taskdoc <- include ["name", "members", "project", "completed"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    let members = trace "line 63" $ splitOn (" " :: String) ("members" `at` taskdoc)
    let task = trace "line 64" $ merge ["members" -: (members :: [String])] taskdoc 
    tId <- liftLIO $ withTaskPolicyModule $ insert "tasks" task
    mlpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select [ "_id" -: pid ] "projects"
    pdoc <- liftLIO $ unlabel $ fromJust mlpdoc
    let curTasks = "tasks" `at` pdoc
    let newTasks = tId:curTasks
    let newDoc = merge ["tasks" -: newTasks] pdoc
    alludocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let memDocs = filter (\u -> ("name" `at` u) `elem` members) alludocs
    liftLIO $ withTaskPolicyModule $ save "projects" newDoc
    liftLIO $ withTaskPolicyModule $ trace "addTasks" $ addTasks memDocs tId
    respond $ redirectTo ("/projects/" ++ show pid)   


-- Users -----

  post "/people" $ trace "post /people" $ do
    userdoc <- include ["name", "tasks"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabel))
    let usrdoc = merge [ "tasks"    -: ([] :: [ObjectId])
                       , "projects" -: ([] :: [ObjectId]) ] userdoc
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
      if (pId `elem` curProjects) then addProjects (tail memDocs) pId
      else do 
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

