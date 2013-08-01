{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings #-}

module Task.Policy (
   TaskPolicyModule
   , withTaskPolicyModule
   , addProjects
   , addNotifs
   , insertTask
   , insertProj
   , updateProject
  ) where

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
      let _id = read (S8.unpack $ S8.drop 7 group) :: ObjectId
      mproj <- findOne $ select ["_id" -: (_id :: ObjectId)]  "projects" 
      case mproj of
        Nothing -> return [pgroup]
        Just lproj -> do
	  proj <- liftLIO $ unlabel lproj
	  return . map toPrincipal $ "members" `at` proj
    _ -> return [pgroup]
    where group = principalName pgroup
          toPrincipal = principal . T.unpack  
          reviewPaperId = "#reviewPaperId="

insertTask :: Task -> DBAction ObjectId
insertTask task = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB pmpriv) -> do
  tid  <- trace ("inserting " ++ show task) $ insertRecordP pmpriv task 
  return tid

insertProj :: Project -> DBAction ObjectId
insertProj proj = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB pmpriv) -> do
  pid <- liftLIO $ withTaskPolicyModule $ insertRecordP pmpriv proj
  return pid

--resetLabel :: UserName -> DBAction ()
--resetLabel user = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB pmpriv) -> do
--  liftLIO $ setLabelP pmpriv (("True" :: String) %% T.unpack user)

