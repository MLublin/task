{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings #-}

module Task.Policy (
   TaskPolicyModule
   , withTaskPolicyModule
   , addProjects
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
          --mlpdoc <- findOne $ select ["_id" -: pid] "projects"
          --let pdoc = unlabel $ fromJust mlpdoc
          --let members = "members" `at` pdoc
          --readers ==> List.foldl' (\/) this members
          --writers ==> List.foldl' (\/) this members
          readers ==> unrestricted
          writers ==> unrestricted
      collection "projects" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \doc -> do
          --let members = map T.unpack ("members" `at` doc :: [UserName])
          --readers ==> List.foldl' (\/) this members
          --writers ==> List.foldl' (\/) this members
          readers ==> unrestricted
          writers ==> unrestricted
      collection "comments" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \_ -> do
          readers ==> unrestricted
          writers ==> unrestricted
        field "_id" key
    return $ TaskPolicyModuleTCB priv
        where this = privDesc priv

withTaskPolicyModule :: DBAction a -> DC a
withTaskPolicyModule act = withPolicyModule (\(_ :: TaskPolicyModule) -> act)

-- Modifies the database by adding the second argument pId to each user document's "projects" field
addProjects :: [LabeledHsonDocument] -> ObjectId -> DBAction ()
addProjects lmemdocs pId = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB mypriv) -> do
  mlproj <- findOne $ select ["_id" -: pId] "projects"
  let lproj = fromJust mlproj
  if (length lmemdocs) == 0
  then trace "97" $ return ()
  else do
    if (labelOf (head lmemdocs)) `canFlowTo` (labelOf lproj)
    then trace "94" $ do
      memDocs <- mapM (liftLIO . unlabel) lmemdocs
      trace ("memDocs: " ++ show memDocs) $ do
        let ldoc = head lmemdocs
        let curProjects = trace "100" $ "projects" `at` (head memDocs)
        if (pId `elem` curProjects) then trace "101" $ addProjects (tail lmemdocs) pId
        else trace "102" $ do
          let newProjects = pId:curProjects
          doc <- liftLIO $ unlabel ldoc
          let newDoc = merge ["projects" -: newProjects] doc
          saveP mypriv "users" newDoc
          addProjects (tail lmemdocs) pId
    else trace "addProjects: label was not high enough" $ return ()

{-
-- Modifies the database by adding the second argument taskId to each user document's "tasks" field
addTasks :: [LabeledHsonDocument] -> ObjectId -> DBAction ()
addTasks lmemdocs tid = liftLIO $ withPolicyModule $ \(TaskPolicyModuleTCB mypriv) -> do
  if (length lmemdocs == 0)
  then return ()
  else do
    mltask <- findOne $ select ["_id" -: tid] "tasks"
    task <- unlabel $ fromJust mltask
    let pid = "project" `at` task
    mlproj <- findOne $ select ["_id" -: pid] "projects"
    let lproj = fromJust mlproj
    if (labelOf (head lmemdocs)) `canFlowTo` (labelOf lproj)
    then do
      memDocs <- mapM (liftLIO . unlabel) lmemdocs
      let doc = head memDocs
      let curTasks = "tasks" `at` doc
      let newTasks = taskId:curTasks
      let newDoc = merge ["tasks" -: newTasks] doc
      saveP mypriv "users" newDoc
      addTasks (tail memDocs) taskId
    else trace "addProjects: label was not high enough" $ return ()
-}
