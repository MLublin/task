{-# LANGUAGE OverloadedStrings #-}
module Task.Views where

import Prelude hiding (div, span, id, lookup, head)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

import Control.Monad
import Debug.Trace

import Hails.HttpServer hiding (Query)
import Hails.Web hiding (body)
import Hails.Web.Frank
import Hails.Web.Controller hiding (body)
import Hails.Data.Hson
import Hails.Web.User
import Hails.Database
import LIO hiding (label)
import LIO.DCLabel
import Data.Maybe
import Data.Time.Clock

import           Text.Blaze.Html5 hiding (Tag, map)
import           Text.Blaze.Html5.Attributes hiding ( label, form, span
                                                    , title, style )
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.String as SR
import           Text.Regex

import Task.Models


-- Projects -----

displayHomePage :: UserName -> [Project] -> Html
displayHomePage user projects = do
  h1 $ toHtml $ "Welcome " ++ T.unpack user
  p $ a ! href "/projects/new" $ "Create new project"
  h2 $ "Projects In Progress"
  let unfinished = filter (not . projectCompleted) projects
  ul $ forM_ unfinished $ \proj -> do
    let pid = show $ fromJust $ projectId proj
    li $ a ! href (toValue ("/projects/" ++ pid)) $ toHtml (projectTitle proj)
  h2 $ "Completed Projects"
  let finished = filter (projectCompleted) projects
  ul $ forM_ finished $ \proj -> do
    let pid = show $ fromJust $ projectId proj
    li $ a ! href (toValue ("/projects/" ++ pid)) $ toHtml (projectTitle proj)

displayProjectPage :: User -> [Task] -> Project -> Html
displayProjectPage user tasks project = do
  script ! src "http://code.jquery.com/jquery-latest.min.js" $ ""
  script ! src "/static/tasks.js" $ ""
  div $ h1 ! class_ "top" ! id "name" $ toHtml $ projectTitle project
  if (userName user `elem` projectLeaders project) 
    then p $ a ! href (toValue ("/projects/" ++ (show $ fromJust $ projectId project) ++ "/edit")) $ "Edit Project"
    else ""
  div $ p ! class_ "top" ! id "desc" $ toHtml $ projectDesc project
  div $ do
    p $ toHtml ("Start date: " ++ projectStartTime project)
    p $ toHtml ("End date: " ++ projectEndTime project)
  div $ do
    h1 $ "Project members"
    ul $ forM_ (projectMembers project) $ \user -> li $ toHtml $ T.unpack user
  div $ do
    h1 ! class_ "top" $ "Team Tasks"
  if not $ projectCompleted project 
  then div $ do
    h3 $ "Add a new task:"
    let pid = show $ fromJust $ projectId project
    let act = "/projects/" ++ pid ++ "/tasks"
    form ! id "taskform" ! action (toValue act) ! method "post" $ do
      p $ do
        label ! for "name" $ "Task name: "
        input ! type_ "text" ! name "name"
      p $ do
        label ! for "members" $ "Invite members"
        input ! type_ "text" ! name "members"
      input ! type_ "hidden" ! name "project" ! value (toValue pid)
      input ! type_ "hidden" ! name "completed" ! value "False"
      button ! type_ "submit" $ "Add Task"
  else ""
  div $ do
    h3 $ "My tasks"
    let mytasks = filter (\t -> (userName user) `elem` (taskMembers t)) tasks
    ul $ forM_ mytasks $ \task -> li $ toHtml $ (taskName task ++ ": " ++ (showStr (taskMembers task) ""))
  div $ do
    h3 $ "Other tasks"
    let otasks = filter (\t -> not $ (userName user) `elem` (taskMembers t)) tasks
    ul $ forM_ otasks $ \task -> li $ toHtml $ (taskName task ++ ": " ++ (showStr (taskMembers task) ""))
  div $ do
    a ! href "/" $ "Home Page"

newProject :: UserName -> Html
newProject user = do
  form ! id "newprojectform" ! action "/projects" ! method "post" $ do
    p $ do
      label ! for "title" $ "Project title: "
      input ! type_ "text" ! name "title"
    p $ do
      label ! for "desc" $ "Project description: "
      textarea ! name "desc" $ ""
    p $ do
      label ! for "members" $ "Project members: "
      input ! type_ "text" ! name "members"
    p $ do
      label ! for "leaders" $ "Project leaders: "
      input ! type_ "text" ! name "leaders"
    p $ do
      label ! for "startTime" $ "Project start time: "
      input ! type_ "date" ! name "startTime"
    p $ do
      label ! for "endTime" $ "Project end time: "
      input ! type_ "date" ! name "endTime"
    input ! type_ "hidden" ! name "completed" ! value "False"
    input ! type_ "hidden" ! name "tasks[]" ! value ""
    button ! type_ "submit" $ "Add Project"

editProject :: Project -> UserName -> Html
editProject project user = do
  form ! id "editprojectform" ! action "/projects/edit" ! method "post" $ do 
    p $ do
      label ! for "title" $ "Project title: "
      input ! type_ "text" ! name "title" ! value (toValue $ projectTitle project)
    p $ do
      label ! for "desc" $ "Project description: "
      textarea ! name "desc" $ "" ! value (toValue $ projectDesc project)
    p $ do
      label ! for "members" $ "Project members: "
      input ! type_ "text" ! name "members" ! value (toValue $ showStr (projectMembers project) "")
    p $ do
      label ! for "leaders" $ "Project leaders: "
      input ! type_ "text" ! name "leaders" ! value (toValue $ showStr (projectLeaders project) "")
    p $ do
      label ! for "startTime" $ "Project start time: "
      input ! type_ "date" ! name "startTime" ! value (toValue $ projectStartTime project)
    p $ do
      label ! for "endTime" $ "Project end time: "
      input ! type_ "date" ! name "endTime" ! value (toValue $ projectEndTime project)
    p $ do
      label ! for "completed" $ "Project completed: "
      p $ do
        input ! type_ "radio" ! name "completed" ! value "True"  
        "Completed"
      p $ do
        input ! type_ "radio" ! name "completed" ! value "False"
        "In Progress"
    input ! type_ "hidden" ! name "tasks[]" ! value (toValue $ show $ projectTasks project)
    input ! type_ "hidden" ! name "_id" ! value (toValue $ show $ projectId project)
    button ! type_ "submit" $ "Edit Project"

-- Users -----

showUsers :: [UserName] -> Html
showUsers users = do
  h3 $ "Users"
  ul $ forM_ users $ \user -> li $ toHtml $ T.unpack user

newUser :: UserName -> Html
newUser user = trace "newUser" $ do
  form ! id "people" ! action "/people" ! method "post" $ do
    input ! type_ "hidden" ! name "name" ! value (toValue $ T.unpack user)
    input ! type_ "text" ! name "tasks[]" ! value ""
    input ! type_ "text" ! name "projects[]" ! value ""
  script $ "document.getElementById('people').submit();"


-- Utils -----

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

respondHtml ctitle content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    stylesheet "/static/css/stylesheet.css"
    title ctitle
  body $ do
    script ! src "/static/js/jquery.js" $ ""
    script ! src "/static/js/bootstrap.js" $ ""
    content

headL :: [a] -> a
headL (x:_) =  x

showStr :: [UserName] -> String -> String
showStr list str = 
  if list == []
    then drop 1 str
    else  
      let name = T.unpack $ headL list
      in showStr (tail list) (" " ++ name ++ str)
