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
import Text.Blaze.Html5 hiding (Tag, map, head, select)

import qualified Web.Simple.Responses as W

import Task.Policy
import Task.Models
import Task.Views

server :: Application
server = mkRouter $ do
  routeVar "pid" $ do
    routeName "comments" commentController
 
-- Projects ----

  -- The Home Page displays the user's projects and notifications, and allows the user to create a new project
  get "/" $ trace "47 start" $ withUserOrDoAuth $ trace "47 mid" $ \user -> trace "47 end" $ do
    musr <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
    case musr of
      Nothing ->  do     -- New user
        trace "51" $ respond $ respondHtml "Projects" $ trace "creating new user" $ newUser user
      Just usr -> do     -- Existing user
        u <- (liftLIO $ powerUnlabel usr) >>= fromDocument 
        let pids = trace "54" $ userProjects u
        clearance <- liftLIO $ getClearance
        mprojects <- trace (show clearance) $ liftLIO $ withTaskPolicyModule $ mapM (findBy "projects" "_id") pids 
        let projects = trace (show mprojects) $ map fromJust mprojects
        trace "57" $ respond $ respondHtml "Projects" $ displayHomePage user projects (userNotifs u)
 
  -- Display the New Project page
  get "/projects/new" $  withUserOrDoAuth $ \user -> do
    allUserdocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    allUsers <- mapM (\ud -> fromDocument ud) allUserdocs
    let allUserNames = map (\u -> userName u) allUsers
    respond $ respondHtml "newProject" $ newProject user allUserNames 

  -- Process the information for an edited project
  post "/projects/edit" $ do
    pdoc <- include ["_id", "title", "desc", "members", "completed", "startTime", "endTime", "leaders"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. powerUnlabel))
    let pid = read (drop 5 $ at "_id" pdoc) :: ObjectId
    mloldproj <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    oldproj <- liftLIO $ powerUnlabel $ fromJust mloldproj
    let members = ("members" `at` pdoc)
    let leaders = ("leaders" `at` pdoc)
    let project = merge [ "members" -: (members :: [String])
                        , "leaders" -: (leaders :: [String])
                        , "_id" -: pid
                        , "tasks" -: (("tasks" `at` oldproj) :: [ObjectId]) ]
                        pdoc 
    --liftLIO $ withTaskPolicyModule $ updateDB project "projects"
    proj <- fromDocument project :: DCLabeled Project
    liftLIO $ withTaskPolicyModule $ updateDB proj
    alldocs <- liftLIO $ withTaskPolicyModule $ findAllL $ select [] "users"
    memDocs <- liftLIO $ filterM (\ldoc -> do
                                    doc <- liftLIO $ powerUnlabel ldoc 
                                    return (("name" `at` doc) `elem` members) :: LIO DCLabel Bool)
                                 alldocs
    liftLIO $ withTaskPolicyModule $ addProjects memDocs pid
    --remove project from the documents of users who were removed
    let oldMembers = "members" `at` oldproj
    let removedMembers = filter (\member -> not $ member `elem` members) oldMembers
    forM_ removedMembers $ \mem -> do 
      mlmemdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: mem] "users"
      memdoc <- liftLIO $ powerUnlabel $ fromJust mlmemdoc
      let projs = "projects" `at` memdoc
      let newprojs = filter (\p -> p /= pid) projs
      let newdoc = merge ["projects" -: newprojs] memdoc
      mem <- fromDocument memdoc
      liftLIO $ withTaskPolicyModule $ updateDB mem
    respond $ redirectTo ("/projects/" ++ show pid)
    
  -- Display the Project Page  
  get "/projects/:pid" $ trace "99" $ withUserOrDoAuth $ \user -> do
    --liftLIO $ withTaskPolicyModule $ resetLabel user
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    mpdoc <- trace "102" $ liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    case mpdoc of
      Nothing -> trace "104" $ respond notFound 
      Just pdoc -> trace "105" $ do
        proj <- trace "106" $ (liftLIO $ trace "powerUnlabeling" $ powerUnlabel pdoc) >>= (trace "fromdocumenting" $ fromDocument)
        if not $ user `elem` (projectMembers proj)
          then respond $ trace "108" $ redirectTo "/"
          else do
            mudoc <- trace "110" $ liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
	    u <- trace "111" $ (liftLIO $ powerUnlabel $ trace "fromJust 111 display project page" $ fromJust mudoc) >>= fromDocument
            let tids = trace "112" $ projectTasks proj
            mtasks <- trace "113" $ liftLIO $ withTaskPolicyModule $ mapM (\t -> findOne $ select ["_id" -: t] "tasks") tids  -- [Maybe Labeled Doc]
            let ltaskdocs = trace "114" $ (map fromJust mtasks :: [LabeledHsonDocument])
            taskdocs <- trace "115" $ (mapM (\ldoc -> liftLIO $ powerUnlabel ldoc) ltaskdocs)
            tasks <- trace "116" $ mapM fromDocument (taskdocs :: [Document]) -- [Task]
            matype <- trace "line 117 " $ requestHeader "accept"
            case matype of
              Just atype | "application/json" `S8.isInfixOf` atype ->
                 return $ ok "application/json" (encode $ toJSON tasks)
              _ -> return $ trace "121" $ respondHtml "Tasks" $ displayProjectPage u tasks proj

  -- Process the information for a new project
  post "/projects" $ do
    user <- getHailsUser
    pdoc <- include ["title", "desc", "members", "completed", "startTime", "endTime", "leaders", "tasks"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. powerUnlabel))
    let members = ("members" `at` pdoc) :: [UserName]
    let leaders = ("leaders" `at` pdoc)
    let project = merge [ "members" -: (members :: [UserName])
                        , "leaders" -: (leaders :: [String])
                        , "tasks" -: ([] :: [ObjectId])
                        , "completed" -: ("False" :: String)]
                        pdoc
    --proj <- fromDocument project 
    pid <- liftLIO $ withTaskPolicyModule $ insertProj project
    alldocs <- liftLIO $ trace "135" $ withTaskPolicyModule $ findAllL $ select [] "users"
    memDocs <- trace "136" $ liftLIO $ filterM (\ldoc -> do
                                    doc <- liftLIO $ powerUnlabel ldoc 
                                    return (("name" `at` doc) `elem` members) :: LIO DCLabel Bool)
                                 alldocs
    (Just lpdoc) <- trace "140" $ liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    liftLIO $ withTaskPolicyModule $ addNotifs memDocs ("You were added to a new project: " ++ ("title" `at` project) ++ " by " ++ (T.unpack $ fromJust user)) lpdoc
    alldocs' <- trace "addNotifs succeeded" $ liftLIO $ withTaskPolicyModule $ findAllL $ select [] "users"
    memDocs' <- trace "143" $ liftLIO $ filterM (\ldoc -> do
                                    doc <- liftLIO $ powerUnlabel ldoc 
                                    return (("name" `at` doc) `elem` members) :: LIO DCLabel Bool)
                                 alldocs'
    trace "147" $ liftLIO $ withTaskPolicyModule $ addProjects memDocs' pid
    trace "148" $ respond $ redirectTo ("/projects/" ++ show pid)

  -- Display the Edit Project page
  get "/projects/:pid/edit" $ withUserOrDoAuth $ \user -> do
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    mpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    let lpdoc = fromJust mpdoc
    proj <- (liftLIO $ powerUnlabel $ fromJust mpdoc) >>= fromDocument
    alludocs <- liftLIO $ withTaskPolicyModule $ findAllL $ select [] "users"
    alldocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"

    let allnames = map (\doc -> "name" `at` doc) alldocs
    let members = projectMembers proj
    memDocs <- liftLIO $ filterM (\ldoc -> do
                                    doc <- liftLIO $ powerUnlabel ldoc 
                                    return (("name" `at` doc) `elem` members) :: LIO DCLabel Bool)
                                 alludocs
    liftLIO $ withTaskPolicyModule $ addNotifs memDocs ((T.unpack user) ++ " edited a project: " ++ (projectTitle proj)) lpdoc
    respond $ respondHtml "Edit" $ editProject proj user allnames
  
  -- Remove a project and redirect to home page
  post "/projects/:pid/remove" $ withUserOrDoAuth $ \user -> do
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    mlpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    let lpdoc = fromJust mlpdoc
    pdoc <- liftLIO $ powerUnlabel lpdoc
    let projmembers = "members" `at` pdoc
    alldocs <- liftLIO $ withTaskPolicyModule $ findAllL $ select [] "users"
    memDocs <- liftLIO $ filterM (\ldoc -> do
                                    doc <- liftLIO $ powerUnlabel ldoc 
                                    return (("name" `at` doc) `elem` projmembers) :: LIO DCLabel Bool)
                                 alldocs
    liftLIO $ withTaskPolicyModule $ addNotifs memDocs (T.unpack user ++ " removed a project: " ++ ("title" `at` pdoc)) lpdoc
    liftLIO $ withTaskPolicyModule $ removeProj projmembers pid
    respond $ redirectTo "/"

  -- Allow user to leave a project and redirect to home page
  post "/projects/:pid/leave" $ withUserOrDoAuth $ \user -> do
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    mlpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
    pdoc <- liftLIO $ powerUnlabel $ fromJust mlpdoc
    let oldmems = "members" `at` pdoc
    let oldleads = "leaders" `at` pdoc
    let newmems = filter (\m -> m /= user) oldmems
    let newleads = filter (\m -> m /= user) oldleads
    let newdoc = merge ["members" -: newmems, "leaders" -: newleads] pdoc
    liftLIO $ withTaskPolicyModule $ save "projects" newdoc
    mludoc <- liftLIO $ withTaskPolicyModule $ findOne $ select [ "name" -: user ] "users"
    udoc <- liftLIO $ powerUnlabel $ fromJust mludoc 
    let oldprojs = "projects" `at` udoc
    let newprojs = filter (\p -> p /= pid) oldprojs
    let newdoc = merge ["projects" -: newprojs] udoc
    liftLIO $ withTaskPolicyModule $ save "users" newdoc
    respond $ redirectTo "/"


-- Tasks -----

  -- Remove task from database
  post "/tasks/:tid/remove" $ do
    (Just sid) <- queryParam "tid"
    let tid = read (S8.unpack sid) :: ObjectId
    mltdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: tid] "tasks"
    tdoc <- liftLIO $ powerUnlabel $ fromJust mltdoc
    let projId = read ("project" `at` tdoc) :: ObjectId
    mlproj <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: (projId :: ObjectId)] "projects"
    proj <- liftLIO $ powerUnlabel $ fromJust mlproj
    let tasks = trace ("old task length: " ++ (show $ length (("tasks" `at` proj) :: [ObjectId]))) $ filter (\t -> t /= tid) ("tasks" `at` proj)
    let newProj = trace ("new tasks length: " ++ (show $ length tasks)) $ merge ["tasks" -: tasks] proj
    liftLIO $ withTaskPolicyModule $ save "projects" newProj
    redirectBack

  -- Mark a task as completed
  post "/tasks/:tid/edit" $ do
    (Just sid) <- queryParam "tid"
    let tid = read (S8.unpack sid) :: ObjectId
    let completed  = ["completed" -: ("True" :: String)]
    mltdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: tid] "tasks" 
    let ltdoc = fromJust mltdoc
    tdoc <- liftLIO $ powerUnlabel $ fromJust mltdoc
    let newtdoc = merge completed tdoc
    let members = ("members" `at` tdoc :: [UserName])
    allusers <- liftLIO $ withTaskPolicyModule $ findAllL $ select [] "users"
    --let memDocs = allusers
    memDocs <- liftLIO $ filterM (\ldoc -> do
                                    doc <- liftLIO $ powerUnlabel ldoc 
                                    return (("name" `at` doc) `elem` members) :: LIO DCLabel Bool)
                                 allusers
    let projId = read ("project" `at` tdoc) :: ObjectId
    mlproj <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: (projId :: ObjectId)] "projects"
    proj <- liftLIO $ powerUnlabel $ fromJust mlproj
    liftLIO $ withTaskPolicyModule $ addNotifs memDocs (("A task was marked as completed: " ++ ("name" `at` tdoc) ++ " in the project: " ++ ("title" `at` proj)) :: String) ltdoc
    
    liftLIO $ withTaskPolicyModule $ save "tasks" newtdoc
    redirectBack

  -- Process a new task
  post "/projects/:pid/tasks" $ trace "post /tasks called " $ do
    (Just sid) <- queryParam "pid"
    let pid = read (S8.unpack sid) :: ObjectId
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    taskdoc <- include ["name", "members", "project", "completed", "priority"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. powerUnlabel))
    ldoc <- request >>= labeledRequestToHson  -- Labeled Document (entire thing)
    let members = ("members" `at` taskdoc)
    let task = merge ["members" -: (members :: [String])] taskdoc 
    --task' <- fromDocument task
    tid <- liftLIO $ withTaskPolicyModule $ insertTask task 
    mlpdoc <- liftLIO $ trace "insert success" $ withTaskPolicyModule $ findOne $ select [ "_id" -: pid ] "projects"
    pdoc <- liftLIO $ powerUnlabel $ trace "253" $ fromJust mlpdoc
    let curTasks = trace "254" $ "tasks" `at` pdoc
    let newTasks = tid:curTasks
    let newDoc = merge ["tasks" -: newTasks] pdoc
    alludocs <- liftLIO $ withTaskPolicyModule $ findAllL $ select [] "users"
    memDocs <- liftLIO $ filterM (\ldoc -> do
                                    doc <- liftLIO $ powerUnlabel ldoc 
                                    return (("name" `at` doc) `elem` members) :: LIO DCLabel Bool)
                                 alludocs
    --alldocs <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"  -- powerUnlabeled version
    --let powerUnlabeledmemDocs = filter (\u -> ("name" `at` u) `elem` members) alldocs
    (Just ltdoc) <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: tid] "tasks"
    liftLIO $ withTaskPolicyModule $ trace "264" $ do
      proj <- fromDocument newDoc
      updateDB proj
      liftLIO $ trace "addTasks successful" $ withTaskPolicyModule $ addNotifs memDocs (("You were assigned a task: " ++ ("name" `at` task) ++ " in the project: " ++ ("title" `at` pdoc)) :: String) ltdoc
    respond $ redirectTo ("/projects/" ++ show pid)   


-- Users -----

  -- Add a new user
  post "/people" $ do
    userdoc <- include ["name"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO . powerUnlabel))
    let usrdoc = merge [ "notifs" -: ([] :: [String])
                       , "projects" -: ([] :: [ObjectId]) ] userdoc
    liftLIO $ withTaskPolicyModule $ insert "users" usrdoc
    respond $ redirectTo "/"
 
  -- Show all users
  get "/people" $ do
    people <- liftLIO $ withTaskPolicyModule $ findAll $ select [] "users"
    let users =  map (\u -> "name" `at` u) people
    respond $ respondHtml "Users" $ showUsers users

  -- Remove a notification at a given index
  post "/notifs/:index/remove" $ do
    user <- getHailsUser
    (Just ind) <- queryParam "index"
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    let index = read (S8.unpack ind) :: Int
    mluserdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
    userdoc <- liftLIO $ powerUnlabel $ fromJust mluserdoc
    let oldnotifs = "notifs" `at` userdoc :: [String]
    let newnotifs = (take (index) oldnotifs ++ drop (index + 1) oldnotifs) -- remove the notification at the given index
    let newdoc = trace ("new notifs: " ++ show newnotifs ++ " old notifs: " ++ show oldnotifs) $ merge ["notifs" -: newnotifs] userdoc
    liftLIO $ withTaskPolicyModule $ save "users" newdoc
    matype <- requestHeader "accept"
    case matype of
      Just atype | "application/json" `S8.isInfixOf` atype ->
         return $ ok "application/json" (encode $ toJSON ([] :: [User]))
      _ -> redirectBack

  -- Remove all notifications from database
  post "/notifs/removeall" $ do
    user <- getHailsUser 
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    mluserdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
    userdoc <- liftLIO $ powerUnlabel $ fromJust mluserdoc
    let newdoc = merge ["notifs" -: ([] :: [String])] userdoc
    liftLIO $ withTaskPolicyModule $ save "users" newdoc
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
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withTaskPolicyModule $ insert "comments" ldoc
    indexComs user

  REST.update $ withUserOrDoAuth $ \user -> trace "update" $ do
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    doc <- include ["_id", "author", "proj", "text", "parent"] `liftM` (request >>= labeledRequestToHson >>= (liftLIO. powerUnlabel))
    sid <- lookup "_id" doc
    let cid = read sid :: ObjectId
    let newdoc = merge [ "_id" -: cid ] doc
    liftLIO $ withTaskPolicyModule $ save "comments" newdoc
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

{-
-- Modifies the database by adding the second argument notif to each user document's "notif" field 
addNotifs :: [HsonDocument] -> String -> DBAction () 
addNotifs memDocs notif = do
  if memDocs == []
    then return ()
    else do
      let doc = head memDocs
      let curNotifs = "notifs" `at` doc
      let newNotifs = notif:curNotifs
      let newDoc = merge ["notifs" -: newNotifs] doc
      save "users" newDoc
      addNotifs (tail memDocs) notif
-}

-- Modifies the database by removing the second argument proj from each uer document's "projects" field
removeProj :: [UserName] -> ObjectId -> DBAction () 
removeProj users proj = do
  if users == []
    then return ()
    else do
      let user = head users
      userdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["name" -: user] "users"
      u <- liftLIO $ powerUnlabel $ fromJust userdoc
      projs <- lookup "projects" u
      let newprojs = filter (/= proj) projs
      let newdoc = merge ["projects" -: newprojs] u
      liftLIO $ withTaskPolicyModule $ save "users" newdoc
      removeProj (tail users) proj

-- Returns a list of all labeled documents in the database satisfying the Query
findAllL :: Query -> DBAction [LabeledHsonDocument]
findAllL q = do
        cur <- find q
        getAll cur []
        where getAll cur list = do
              mldoc <- next cur
              case mldoc of
                Nothing -> return list
                Just ldoc -> do
                        getAll cur (list ++ [ldoc])

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
                        doc <- liftLIO $ powerUnlabel ldoc
                        getAll cur (list ++ [doc])

