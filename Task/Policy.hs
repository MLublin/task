{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings #-}

module Task.Policy (
   TaskPolicyModule
   , withTaskPolicyModule
   , addProjects
   , addNotifs
   , insertTask
   , insertProj
   , updateProject
   , findWhereWithGroupP
   , powerUnlabel
  ) where

import Prelude hiding (lookup)
import Data.Typeable
import Debug.Trace
import LIO
import LIO.DCLabel
import Hails.Database
import Hails.PolicyModule
import Hails.PolicyModule.DSL
import Hails.Web.User
import Data.Maybe
import qualified Data.List as List
import qualified Data.Text as T
import           Hails.PolicyModule.Groups
import qualified Data.ByteString.Char8 as S8
import Hails.Database.Structured
import LIO.TCB
import Task.Models
import Control.Monad

data TaskPolicyModule = TaskPolicyModuleTCB DCPriv deriving Typeable

instance PolicyModule TaskPolicyModule where
  initPolicyModule priv = do
    setPolicy priv $ do
      database $ do
        readers ==> unrestricted 
        writers ==> unrestricted
        admins ==> this
      collection "users" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \doc -> do
          readers ==> unrestricted
          writers ==> unrestricted
        field "name" key
      collection "tasks" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \doc -> do
          --let pid = "project" `at` doc
          --mlpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
          --let pdoc = unlabel $ fromJust mlpdoc
          --let members = "members" `at` pdoc
          --readers ==> List.foldl' (\/) this members
          --writers ==> List.foldl' (\/) this members 
          let projid = ("#projId=" :: String) ++ (show $ ("project" `at` doc :: ObjectId)) :: String
          readers ==> projid \/ this
          writers ==> projid \/ this
        --    readers ==> unrestricted
	--    writers ==> unrestricted
      collection "projects" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \doc -> do
          let projid = ("#projId=" :: String) ++ (show $ ("_id" `at` doc :: ObjectId)) :: String
          readers ==> projid \/ this
          writers ==> projid \/ this
          --let members = map T.unpack ("members" `at` doc :: [UserName])
          --readers ==> List.foldl' (\/) this members
          --writers ==> List.foldl' (\/) this members
          --readers ==> unrestricted
          --writers ==> unrestricted
      collection "comments" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \doc -> do
          --let pid = "proj" `at` doc
          ----mlpdoc <- liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"
          --mlpdoc <- findOneProj pid
          --let pdoc = unlabel $ fromJust mlpdoc
          --let members = "members" `at` pdoc
          --readers ==> List.foldl' (\/) this members
          --writers ==> List.foldl' (\/) this members
          readers ==> unrestricted
          writers ==> unrestricted
        field "_id" key
    return $ TaskPolicyModuleTCB priv
        where this = privDesc priv

withTaskPolicyModule :: DBAction a -> DC a
withTaskPolicyModule act = withPolicyModule (\(_ :: TaskPolicyModule) -> act)

findOneProj pid = do
    return liftLIO $ withTaskPolicyModule $ findOne $ select ["_id" -: pid] "projects"

updateProject :: Project -> DBAction ()
updateProject proj = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB pmpriv) -> do
  saveRecordP pmpriv proj

-- Modifies the database by adding the second argument pId to each user document's "projects" field
addProjects :: [LabeledHsonDocument] -> ObjectId -> DBAction ()
addProjects lmemdocs pId = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB pmpriv) -> do
  mlproj <- findOne $ select ["_id" -: pId] "projects"
  let lproj = fromJust mlproj
  if (length lmemdocs) == 0
  then trace "97" $ return ()
  else do 
    memDocs <- mapM (liftLIO . unlabel) lmemdocs
    trace ("memDocs: " ++ show memDocs) $ do
      let ldoc = head lmemdocs
      let curProjects = trace "100" $ "projects" `at` (head memDocs)
      if (pId `elem` curProjects) then trace "101" $ addProjects (tail lmemdocs) pId
      else trace "102" $ do
        let newProjects = pId:curProjects
        doc <- liftLIO $ unlabel ldoc
        let newDoc = merge ["projects" -: newProjects] doc
        saveP pmpriv "users" newDoc
        addProjects (tail lmemdocs) pId

-- Modifies the database by adding the second argument notif to each user document's "notif" field 
-- ldoc is the labeled document for the source of the notification
addNotifs :: [LabeledHsonDocument] -> String -> LabeledHsonDocument -> DBAction ()
addNotifs lmemdocs notif ldoc = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB pmpriv) -> do
  if (length lmemdocs == 0)
  then trace "added Notifs success" $ return ()
  else do
  --mluser <- findOne $ select ["name" -: tid] "users"
  --let luser = fromJust mluser
    memDocs <- mapM (liftLIO . unlabel) lmemdocs
    let doc = head memDocs
    let curNotifs = "notifs" `at` doc
    let newNotifs = trace ("current notifs: " ++ show curNotifs) $ notif:curNotifs
    let newDoc = trace ("new notifs: " ++ show newNotifs) $ merge ["notifs" -: newNotifs] doc
    trace ("new document: " ++ show newDoc) $ saveP pmpriv "users" newDoc
    addNotifs (tail lmemdocs) notif ldoc
    

instance Groups TaskPolicyModule where
  groupsInstanceEndorse = TaskPolicyModuleTCB (PrivTCB $ toCNF True)
  groups _ p pgroup = trace (show pgroup) $ case () of
    _ | "#projId=" `S8.isPrefixOf` group -> do
      let _id = read (S8.unpack $ S8.drop 8 group) :: ObjectId
      mproj <- findOne $ select ["_id" -: (_id :: ObjectId)]  "projects" 
      case mproj of
        Nothing -> return [pgroup]
        Just lproj -> do
	  proj <- liftLIO $ unlabelP p lproj
	  return . map toPrincipal $ "members" `at` proj
    _ -> return [pgroup]
    where group = principalName pgroup
          toPrincipal = principal . T.unpack  
          reviewPaperId = "#reviewPaperId="

insertTask :: Task -> DBAction ObjectId
insertTask task = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB pmpriv) -> do
  tid  <- trace ("inserting " ++ show task) $ insertRecordP pmpriv task 
  return tid

insertProj :: HsonDocument -> DBAction ObjectId
insertProj proj = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB pmpriv) -> do
  pid <- liftLIO $ withTaskPolicyModule $ trace ("inserting" ++ show proj) $  insertP pmpriv "projects" proj
  trace "insert success" $ return pid

--resetLabel :: UserName -> DBAction ()
--resetLabel user = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB pmpriv) -> do
--  liftLIO $ setLabelP pmpriv (("True" :: String) %% T.unpack user)

findWhereWithGroupP :: (DCRecord a, MonadDB m) => DCPriv -> Query -> m (Maybe a)
findWhereWithGroupP p query  = liftDB $ do
  mldoc <- findOneP p query
  c <- liftLIO $ getClearance
  case mldoc of
    Just ldoc' -> do ldoc <- labelRewrite (undefined :: TaskPolicyModule) ldoc'
                     if canFlowToP p (labelOf ldoc) c 
                       then fromDocument `liftM` (liftLIO $ unlabelP p ldoc)
                       else return Nothing
    _ -> return Nothing

instance DCRecord Project where
  fromDocument doc = do
    let pid = trace "pid lookup" $ lookupObjId "_id" doc
    title <- trace "title lookup" $ lookup "title" doc
    members <- trace "members lookup" $ lookup "members" doc
    completed <- trace "completed lookup" $ lookup "completed" doc
    startTime <- trace "startTime lookup" $ lookup "startTime" doc
    endTime <- trace "endTime lookup" $ lookup "endTime" doc
    leaders <- trace "leaders lookup" $ lookup "leaders" doc
    tasks <- trace "tasks lookup" $ lookup "tasks" doc
    desc <- trace "desc lookup" $ lookup "desc" doc
    trace "returning project" $ return Project { projectId = pid
                   , projectTitle = title
                   , projectMembers = members
                   , projectCompleted = read completed 
                   , projectStartTime = startTime
                   , projectEndTime = endTime
                   , projectLeaders = leaders
                   , projectTasks = tasks
                   , projectDesc = desc }

  toDocument t =
    (maybe [] (\tid -> ["_id"  -: tid]) $ projectId t) ++
    [ "title" -: projectTitle t
    , "members" -: (projectMembers t :: [UserName])
    , "completed" -: show (projectCompleted t)
    , "startTime" -: projectStartTime t
    , "endTime" -: projectEndTime t
    , "leaders" -: projectLeaders t
    , "tasks" -: projectTasks t 
    , "desc" -: projectDesc t ]

  findWhereP = findWhereWithGroupP
  
  recordCollection _ = "projects"


instance DCRecord User where
  fromDocument doc = trace "fromDoc user" $ do
    let uid = lookupObjIdh "_id" doc
    name <- lookup "name" doc
    notifs <- lookup "notifs" doc
    let projects = at "projects" doc
    let invites = at "invites" doc
    trace "returning user" $ return User { userId = uid
                , userName = name
                , userNotifs = notifs
                , userInvites = invites
                , userProjects = projects }

  toDocument u = trace "toDoc user" $
    [ "_id"  -: userId u
    , "name" -: userName u
    , "notifs" -: userNotifs u
    , "invites" -: userInvites u
    , "projects" -: userProjects u]


  recordCollection _ = "tasks"


instance DCRecord Task where
  fromDocument doc = trace "fromDoc task" $ do
    let tid = lookupObjIdh "_id" doc
    name <- lookup "name" doc
    members <- lookup "members" doc
    completed <- trace "completed" $ lookup "completed" doc
    priority <- trace "priority" $ lookup "priority" doc
    project <- lookup "project" doc
    return Task { taskId = tid
                , taskName = name
                , taskMembers = members
                , taskCompleted = read completed
                , taskPriority = priority
                , taskProject = read project }

  toDocument t =
    (maybe [] (\tid -> ["_id"  -: tid]) $ taskId t) ++
    [ "name" -: taskName t
    , "members" -: (taskMembers t :: [UserName])
    , "completed" -: show (taskCompleted t)
    , "priority" -: taskPriority t
    , "project" -: taskProject t]

  findWhereP = findWhereWithGroupP

  recordCollection _ = "tasks"


instance DCRecord Comment where
  fromDocument doc = do
    let cid = lookupObjId "_id" doc
    author <- lookup "author" doc
    text <- lookup "text" doc -- body text
    proj <- lookupObjId "proj" doc -- associated proj
    let parent = lookupObjId "parent" doc -- the comment it's in reply to
    return Comment { commentId = cid
                   , commentAuthor = author
                   , commentAssocProj = proj
                   , commentText = text
                   , commentInReplyTo = parent }

  toDocument c =
    let mparent = commentInReplyTo c
        parent = if isJust mparent
                   then [ "parent" -: fromJust mparent ]
                   else []
        mid = commentId c
        id = if isJust mparent
               then [ "_id" -: fromJust mid ]
               else []
    in id ++
       [ "author" -: commentAuthor c
       , "post" -: commentAssocProj c
       , "text" -: commentText c ]
       ++ parent

  recordCollection _ = "comments"

--powerUnlabel :: DCLabeled l a -> DC
powerUnlabel a = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB pmpriv) -> do
  liftLIO $ unlabelP pmpriv a
