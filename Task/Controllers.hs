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
import Hails.Web.REST (RESTController)
import qualified Hails.Web.REST as REST
import Hails.Web.Frank
import Hails.Web.Controller
import Hails.Data.Hson
import Hails.Web.User
import Hails.Database
import Hails.Database.Structured hiding (findAll)
import qualified Hails.Database.Structured as D
import LIO
import LIO.DCLabel
import Data.Maybe
import Data.Time.Clock
import Data.List.Split
import Data.Aeson (decode, encode, toJSON)
import Text.Blaze.Renderer.Text
import Text.Blaze.Html5 hiding (Tag, map, head, select, label)

import qualified Web.Simple.Responses as W

import Task.Policy
import Task.Models
import Task.Views

server :: Application
server = mkRouter $ do

  routeVar "pid" $ do
    routeName "comments" commentController

  --priv <- liftDB $ withTaskPolicyModule $ appPriv

 
-- Projects ----

  -- The Home Page displays the user's projects and notifications, and allows the user to create a new project
  get "/" $ withUserOrDoAuth $ \user -> do
    priv <- appPriv
    musr <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
    case musr of
      Nothing ->  do     -- New user
        respond $ respondHtml "Projects" $ trace "creating new user" $ newUser user
      Just usr -> do     -- Existing user
        u <- (liftLIO $ unlabelP priv usr) >>= fromDocument 
        let pids = userProjects u
        mprojects <- liftLIO $ withTaskPolicyModule $ mapM (findBy "projects" "_id") pids 
        let projects = map fromJust mprojects
        respond $ respondHtml "Projects" $ displayHomePage user projects (userNotifs u)
 
  -- Display the New Project page
  get "/projects/new" $  withUserOrDoAuth $ \user -> do
    allUserdocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    allUsers <- mapM (\ud -> fromDocument ud) allUserdocs
    let allUserNames = map (\u -> userName u) allUsers
    respond $ respondHtml "newProject" $ newProject user allUserNames 

  -- Process the information for an edited project
  post "/projects/edit" $ do
    priv <- appPriv
    liftLIO $ do
      clr <- getClearance
      setClearanceP priv $ (priv %% True) `lub` clr
    pdoc <- include ["_id", "title", "desc", "members", "completed", "startTime", "endTime", "leaders"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabelP priv))
    let pid = read (drop 5 $ at "_id" pdoc) :: ObjectId
    mloldproj <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    oldproj <- liftLIO $ unlabelP priv $ fromJust mloldproj
    let members = ("members" `at` pdoc)
    let leaders = ("leaders" `at` pdoc)
    let project = merge [ "members" -: (members :: [String])
                        , "leaders" -: (leaders :: [String])
                        , "_id" -: pid
                        , "tasks" -: (("tasks" `at` oldproj) :: [ObjectId]) ]
                        pdoc 
    trace "saving new project" $ liftLIO $ withTaskPolicyModule $ saveP priv "projects" project
    alldocs <- trace "new project save done" $ liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let memDocs = filter (\u -> ("name" `at` u) `elem` members) alldocs
    liftLIO $ withTaskPolicyModule $ addProjects memDocs pid priv
    respond $ redirectTo ("/projects/" ++ show pid)
    
  -- Display the Project Page  
  get "/projects/:pid" $ withUserOrDoAuth $ \user -> do
    priv <- appPriv
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    mpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    case mpdoc of
      Nothing -> respond notFound 
      Just pdoc -> do
        proj <- (liftLIO $ unlabelP priv pdoc) >>= fromDocument
        if not $ user `elem` (projectMembers proj)
          then respond $ redirectTo "/"
          else do
            mudoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
            u <- (liftLIO $ unlabelP priv $ fromJust mudoc) >>= fromDocument
            let tids = projectTasks proj
            mtasks <- liftLIO $ withTaskPolicyModule $ mapM (findBy "tasks" "_id") tids 
            let tasks = map fromJust mtasks
            matype <- requestHeader "accept"
            case matype of
              Just atype | "application/json" `S8.isInfixOf` atype ->
                 return $ ok "application/json" (encode $ toJSON tasks)
              _ -> return $ respondHtml "Tasks" $ displayProjectPage u tasks proj

  -- Process the information for a new project
  post "/projects" $ do
    priv <- appPriv
    user <- getHailsUser
    ldoc <- request >>= labeledRequestToHson
    pdoc <- trace ("label of ldoc: " ++ (show $ labelOf ldoc)) $ include ["title", "desc", "members", "completed", "startTime", "endTime", "leaders", "tasks"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabelP priv))
    let members = ("members" `at` pdoc) :: [UserName]
    let leaders = ("leaders" `at` pdoc)
    id <- genObjectId
    let project = merge [ "members" -: (members :: [UserName])
                        , "leaders" -: (leaders :: [String])
                        , "tasks" -: ([] :: [ObjectId])
                        , "_id" -: (id :: ObjectId)]
                        pdoc 
    pid <- liftLIO $ withTaskPolicyModule $ insertP priv "projects" project
    alldocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let memDocs = filter (\doc -> ("name" `at` doc) `elem` members) alldocs 
    liftLIO $ withTaskPolicyModule $ addProjects memDocs pid priv
    modifieddocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let modifiedMemDocs = filter (\doc -> ("name" `at` doc) `elem` members) modifieddocs 
    liftLIO $ withTaskPolicyModule $ addNotifs modifiedMemDocs ("You were added to a new project: " ++ ("title" `at` project) ++ " by " ++ (T.unpack $ fromJust user)) priv
    respond $ redirectTo ("/projects/" ++ show pid)

  -- Display the Edit Project page
  get "/projects/:pid/edit" $ withUserOrDoAuth $ \user -> do
    priv <- appPriv
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    mpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    proj <- (liftLIO $ unlabelP priv $ fromJust mpdoc) >>= fromDocument
    alludocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let allnames = map (\doc -> "name" `at` doc) alludocs
    let memDocs = filter (\u -> ("name" `at` u) `elem` (projectMembers proj)) alludocs
    liftLIO $ withTaskPolicyModule $ addNotifs memDocs ((T.unpack user) ++ " edited a project: " ++ (projectTitle proj)) priv
    respond $ respondHtml "Edit" $ editProject proj user allnames
  
  -- Remove a project and redirect to home page
  post "/projects/:pid/remove" $ withUserOrDoAuth $ \user -> do
    priv <- appPriv
    liftLIO $ do
      clr <- getClearance
      setClearanceP priv $ (priv %% True) `lub` clr
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    mlpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    pdoc <- liftLIO $ unlabelP priv $ fromJust mlpdoc
    let projmembers = "members" `at` pdoc
    alldocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let memDocs = trace (show alldocs) $ filter (\u -> ("name" `at` u) `elem` projmembers) alldocs
    liftLIO $ withTaskPolicyModule $ addNotifs memDocs (T.unpack user ++ " removed a project: " ++ ("title" `at` pdoc)) priv
    liftLIO $ withTaskPolicyModule $ removeProj projmembers pid priv
    respond $ redirectTo "/"

  -- Allow user to leave a project and redirect to home page
  post "/projects/:pid/leave" $ withUserOrDoAuth $ \user -> do
    priv <- appPriv
    liftLIO $ do
      clr <- getClearance
      setClearanceP priv $ (priv %% True) `lub` clr
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    mlpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    pdoc <- liftLIO $ unlabelP priv $ fromJust mlpdoc
    let oldmems = "members" `at` pdoc
    let oldleads = "leaders" `at` pdoc
    let newmems = filter (\m -> m /= user) oldmems
    let newleads = filter (\m -> m /= user) oldleads
    let newdoc = merge ["members" -: newmems, "leaders" -: newleads] pdoc
    --newRec <- fromDocument newdoc 
    --req <- request
    --lnewRec <- liftLIO $ label (labelOf req) (newRec :: Project)
    --liftLIO $ withTaskPolicyModule $ saveLabeledRecordP priv (lnewRec :: DCLabeled Project)
    liftLIO $ withTaskPolicyModule $ saveP priv "projects" newdoc
    mludoc <- liftLIO $ withTaskPolicyModule $ findOne $ select [ "name" -: user ] "users"
    udoc <- liftLIO $ unlabelP priv $ fromJust mludoc 
    let oldprojs = "projects" `at` udoc
    let newprojs = filter (\p -> p /= pid) oldprojs
    let newdoc = merge ["projects" -: newprojs] udoc
    --newRec <- fromDocument newdoc 
    --req <- request
    --lnewRec <- liftLIO $ label (labelOf req) (newRec :: User)
    --liftLIO $ withTaskPolicyModule $ saveLabeledRecordP priv lnewRec
    liftLIO $ withTaskPolicyModule $ saveP priv "users" newdoc
    respond $ redirectTo "/"


-- Tasks -----

  -- Remove task from database
  post "/tasks/:tid/remove" $ do
    priv <- appPriv
    liftLIO $ do
      clr <- getClearance
      setClearanceP priv $ (priv %% True) `lub` clr
    (Just sid) <- queryParam "tid"
    let tid = read (S8.unpack sid) :: ObjectId
    mltdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: tid] "tasks"
    tdoc <- liftLIO $ unlabelP priv $ fromJust mltdoc
    let projId = read ("project" `at` tdoc) :: ObjectId
    mlproj <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: (projId :: ObjectId)] "projects"
    proj <- liftLIO $ unlabelP priv $ fromJust mlproj
    let tasks = trace ("old task length: " ++ (show $ length (("tasks" `at` proj) :: [ObjectId]))) $ filter (\t -> t /= tid) ("tasks" `at` proj)
    let newProj = trace ("new tasks length: " ++ (show $ length tasks)) $ merge ["tasks" -: tasks] proj
    liftLIO $ withTaskPolicyModule $ saveP priv "projects" newProj
    redirectBack

  -- Mark a task as completed
  post "/tasks/:tid/edit" $ do
    priv <- appPriv
    liftLIO $ do
      clr <- getClearance
      setClearanceP priv $ (priv %% True) `lub` clr
    (Just sid) <- queryParam "tid"
    let tid = read (S8.unpack sid) :: ObjectId
    let completed  = ["completed" -: ("True" :: String)]
    mltdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: tid] "tasks" 
    tdoc <- liftLIO $ unlabelP priv $ fromJust mltdoc
    let newtdoc = merge completed tdoc
    let members = "members" `at` tdoc
    allusers <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let memdocs = filter (\u -> (("name" `at` u) :: UserName) `elem` members) allusers
    let projId = read ("project" `at` tdoc) :: ObjectId
    mlproj <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: (projId :: ObjectId)] "projects"
    proj <- liftLIO $ unlabelP priv $ fromJust mlproj
    liftLIO $ withTaskPolicyModule $ addNotifs memdocs (("A task was marked as completed: " ++ ("name" `at` tdoc) ++ " in the project: " ++ ("title" `at` proj)) :: String) priv
    liftLIO $ withTaskPolicyModule $ saveP priv "tasks" newtdoc
    redirectBack

  -- Process a new task
  post "/projects/:pid/tasks" $ do
    priv <- appPriv
    liftLIO $ do
      clr <- getClearance
      setClearanceP priv $ (priv %% True) `lub` clr
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    taskdoc <- include ["name", "members", "project", "completed", "priority"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabelP priv))
    let members = ("members" `at` taskdoc)
    let task = merge ["members" -: (members :: [String])] taskdoc 
    tid <- liftLIO $ withTaskPolicyModule $ insertP priv "tasks" task
    mlpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select [ "_id" -: pid ] "projects"
    pdoc <- liftLIO $ unlabelP priv $ fromJust mlpdoc
    let curTasks = "tasks" `at` pdoc
    let newTasks = tid:curTasks
    let newDoc = merge ["tasks" -: newTasks] pdoc
    alludocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let memDocs = filter (\u -> ("name" `at` u) `elem` members) alludocs
    liftLIO $ withTaskPolicyModule $ do
      saveP priv "projects" newDoc
      addTasks memDocs tid priv
      liftLIO $ withTaskPolicyModule $ addNotifs memDocs (("You were assigned a task: " ++ ("name" `at` task) ++ " in the project: " ++ ("title" `at` pdoc)) :: String) priv
    respond $ redirectTo ("/projects/" ++ show pid)   


-- Users -----

  -- Add a new user
  post "/people" $ do
    priv <- appPriv
    userdoc <- include ["name", "tasks"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabelP priv))
    let usrdoc = merge [ "tasks"    -: ([] :: [ObjectId])
                       , "notifs" -: ([] :: [String])
                       , "projects" -: ([] :: [ObjectId]) ] userdoc
    liftLIO $ withTaskPolicyModule $ insertP priv "users" usrdoc
    respond $ redirectTo "/"
 
  -- Show all users
  get "/people" $ do
    people <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let users =  map (\u -> "name" `at` u) people
    respond $ respondHtml "Users" $ showUsers users

  -- Remove a notification at a given index
  post "/notifs/:index/remove" $ do
    priv <- appPriv
    user <- getHailsUser
    (Just ind) <- queryParam "index"
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    let index = read (S8.unpack ind) :: Int
    mluserdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
    userdoc <- liftLIO $ unlabelP priv $ fromJust mluserdoc
    let oldnotifs = "notifs" `at` userdoc :: [String]
    let newnotifs = (take (index) oldnotifs ++ drop (index + 1) oldnotifs) -- remove the notification at the given index
    let newdoc = trace ("new notifs: " ++ show newnotifs ++ " old notifs: " ++ show oldnotifs) $ merge ["notifs" -: newnotifs] userdoc
    liftLIO $ withTaskPolicyModule $ saveP priv "users" newdoc
    matype <- requestHeader "accept"
    case matype of
      Just atype | "application/json" `S8.isInfixOf` atype ->
         return $ ok "application/json" (encode $ toJSON ([] :: [User]))
      _ -> redirectBack

  -- Remove all notifications from database
  post "/notifs/removeall" $ do
    priv <- appPriv
    user <- getHailsUser 
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    mluserdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
    userdoc <- liftLIO $ unlabelP priv $ fromJust mluserdoc
    let newdoc = merge ["notifs" -: ([] :: [String])] userdoc
    liftLIO $ withTaskPolicyModule $ saveP priv "users" newdoc
    matype <- requestHeader "accept"
    case matype of
      Just atype | "application/json" `S8.isInfixOf` atype ->
         return $ ok "application/json" (encode $ toJSON ([] :: [User]))
      _ -> redirectBack


-- Comments -----

commentController :: RESTController
commentController = do
  REST.index $ withUserOrDoAuth $ \user -> indexComs user

  REST.create $ withUserOrDoAuth $ \user -> trace "create" $ do
    priv <- appPriv
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withTaskPolicyModule $ insertP priv "comments" ldoc
    indexComs user

  REST.update $ withUserOrDoAuth $ \user -> trace "update" $ do
    priv <- appPriv
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    doc <- include ["_id", "author", "proj", "text", "parent"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. unlabelP priv))
    sid <- lookup "_id" doc
    let cid = read sid :: ObjectId
    let newdoc = merge [ "_id" -: cid ] doc
    liftLIO $ withTaskPolicyModule $ saveP priv "comments" newdoc
    indexComs user

indexComs username = do
  sid <- queryParam "pid"
  let str = S8.unpack $ fromJust sid  -- proj id as a string
  let pid = read str -- proj id as an ObjectId
  comments <- liftLIO . withTaskPolicyModule $ D.findAll $ select [] "comments" 
  matype <- requestHeader "accept"
  case matype of
    Just atype | "application/json" `S8.isInfixOf` atype ->
       return $ ok "application/json" (encode $ toJSON comments)
    _ -> return $ respondHtmlC (toHtml $ T.unpack username) $ showPage comments username pid


---- Helper Functions --- 

-- Modifies the database by adding the second argument notif to each user document's "notif" field 
addNotifs :: [HsonDocument] -> String -> DCPriv -> DBAction () 
addNotifs memDocs notif priv = do
  if memDocs == []
    then return ()
    else do
      let doc = head memDocs
      let curNotifs = "notifs" `at` doc
      let newNotifs = notif:curNotifs
      let newDoc = merge ["notifs" -: newNotifs] doc
      saveP priv "users" newDoc
      addNotifs (tail memDocs) notif priv

-- Modifies the database by adding the second argument taskId to each user document's "tasks" field
addTasks :: [HsonDocument] -> ObjectId -> DCPriv -> DBAction ()
addTasks memDocs taskId priv = do
  if memDocs == []
    then return ()
    else do
      let doc = head memDocs
      let curTasks = "tasks" `at` doc
      let newTasks = taskId:curTasks
      let newDoc = merge ["tasks" -: newTasks] doc
      trace "saving new task" $ saveP priv "users" newDoc
      addTasks (tail memDocs) taskId priv

-- Modifies the database by adding the second argument pId to each user document's "projects" field
addProjects :: [HsonDocument] -> ObjectId -> DCPriv -> DBAction ()
addProjects memDocs pId priv = do
  if memDocs == []
    then return ()
    else do
      let doc = head memDocs
      let curProjects = "projects" `at` doc
      if (pId `elem` curProjects) then addProjects (tail memDocs) pId priv
      else do 
        let newProjects = pId:curProjects
        let newDoc = merge ["projects" -: newProjects] doc
        saveP priv "users" newDoc
        addProjects (tail memDocs) pId priv

-- Modifies the database by removing the second argument proj from each uer document's "projects" field
removeProj :: [UserName] -> ObjectId -> DCPriv -> DBAction () 
removeProj users proj priv = do
  if users == []
    then return ()
    else do
      let user = head users
      userdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
      u <- liftLIO $ unlabelP priv $ fromJust userdoc
      projs <- lookup "projects" u
      let newprojs = filter (/= proj) projs
      let newdoc = merge ["projects" -: newprojs] u
      liftLIO $ withTaskPolicyModule $ saveP priv "users" newdoc
      removeProj (tail users) proj priv

-- Returns a list of all documents in the database satisfying the Query
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
