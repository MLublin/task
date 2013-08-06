{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings #-}

module Task.Policy (
   TaskPolicyModule
   , withTaskPolicyModule
   ) where

import Data.Typeable

import LIO
import LIO.TCB
import LIO.DCLabel
import Hails.Database
import Hails.PolicyModule
import Hails.PolicyModule.DSL
import Hails.PolicyModule.Groups
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Debug.Trace

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
        field "_id" key
      collection "tasks" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \doc -> do
          let projid = ("#projId=" :: String) ++ (show $ (read ("project" `at` doc) :: ObjectId)) :: String
          readers ==> projid \/ this \/ principal "taskell"
          writers ==> projid \/ this \/ principal "taskell"
        field "_id" key
      collection "projects" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \doc -> do
          let projid = ("#projId=" :: String) ++ (show $ ("_id" `at` doc :: ObjectId)) :: String
          readers ==> projid \/ this \/ principal "taskell"
          writers ==> projid \/ this \/ principal "taskell"
        field "_id" key
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

instance Groups TaskPolicyModule where
  groupsInstanceEndorse = TaskPolicyModuleTCB (PrivTCB $ toCNF True)
  groups _ p pgroup = trace ("GROUPS FUNCTION " ++ show pgroup) $ case () of
    _ | "#projId=" `S8.isPrefixOf` group -> do
      let _id = read (S8.unpack $ S8.drop 8 group) :: ObjectId
      mproj <- findOne $ select ["_id" -: (_id :: ObjectId)]  "projects" 
      case mproj of
        Nothing -> return [pgroup]
        Just lproj -> do
          proj <- liftLIO $ unlabelP p lproj
          trace (show $  map toPrincipal $ "members" `at` proj) $ do
            return . map toPrincipal $ "members" `at` proj
    _ -> return [pgroup]
    where group = principalName pgroup
          toPrincipal = principal . T.unpack  

