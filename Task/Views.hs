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
import LIO hiding (label)
import LIO.DCLabel
import Data.Maybe
import Data.List hiding (head)
import Data.Ord
import Data.Bson (timestamp)
import Data.Time.Clock
import Data.Time.LocalTime

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
  script ! src "/static/js/tasks.js" $ ""
  div $ h1 ! class_ "top" ! id "name" $ toHtml $ projectTitle project
  if (userName user `elem` projectLeaders project) 
    then p $ a ! href (toValue ("/projects/" ++ (show $ fromJust $ projectId project) ++ "/edit")) $ "Edit Project"
    else ""
  if (userName user `elem` projectLeaders project) 
    then p $ a ! href (toValue ("/projects/" ++ (show $ fromJust $ projectId project) ++ "/remove")) $ "Remove Project"
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
        label ! for "members" $ "Invite members: "
        input ! type_ "text" ! name "members"
      p $ do
        label ! for "priority" $ "Task priority: "
        select ! name "priority" $ do
          option ! value "1" $ "Low"
          option ! value "2" $ "Medium"
          option ! value "3" $ "High"
      input ! type_ "hidden" ! name "project" ! value (toValue pid)
      input ! type_ "hidden" ! name "completed" ! value "False"
      button ! type_ "submit" $ "Add Task"
  else ""
  div $ do
    h3 $ "My tasks"
    let mytasks = filter (\t -> (userName user) `elem` (taskMembers t)) tasks
    h4 $ "In progress:"
    let incomplete = sortBy (comparing taskPriority) $ filter (not . taskCompleted) mytasks
    ul $ forM_ incomplete $ \task -> showTask task
    h4 $ "Completed:"
    let complete = sortBy (comparing taskPriority) $ filter taskCompleted mytasks
    ul $ forM_ complete $ \task -> showTask task
  div $ do
    h3 $ "Other tasks"
    let otasks = filter (\t -> not $ (userName user) `elem` (taskMembers t)) tasks
    ul $ forM_ otasks $ \task -> li $ toHtml $ (taskName task ++ ": " ++ (showStr (taskMembers task) ""))
  div $ do
    let pid = show $ fromJust $ projectId project
    iframe ! height "600" ! width "500" ! id "commentframe" ! src (toValue ("/" ++ pid ++ "/comments")) $ ""  -- todo: set height/width in css file
  div $ do
    a ! href "/" $ "Home Page"

newProject :: UserName -> [UserName] -> Html
newProject user members = do
  script ! src "http://code.jquery.com/jquery-latest.min.js" $ ""
  script ! src "/static/js/user_select.js" $ ""
  form ! id "newprojectform" ! action "/projects" ! method "post" $ do
    p $ do
      label ! for "title" $ "Project title: "
      input ! type_ "text" ! name "title"
    p $ do
      label ! for "desc" $ "Project description: "
      textarea ! name "desc" $ ""

    div ! id "memberSelect" $ do
      p $ "Select members for this project:"
      forM_ members $ \member -> do
                          toHtml $ T.unpack member  
                          input ! type_ "checkbox" ! class_ "memberCheckbox" ! name "members[]" ! value (toValue member) 
      
    div ! id "leaderSelect" $ do
      p $ "select leaders for this project:"
      forM_ members $ \member -> do
                          div ! id (toValue $ T.unpack member) $ do
                            toHtml $ T.unpack member  
                            input ! type_ "checkbox" ! class_ "leaderCheckbox" ! name "leaders[]" ! value (toValue member) 
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


-- Tasks -----

checkTask :: Task -> Html
checkTask task = trace "checkTask" $ do
  let tid = show $ fromJust $ taskId task
  let fid = toValue ("form" ++ tid)
  let act = "/tasks/" ++ tid ++ "/edit"
  form ! id fid ! action (toValue act) ! method "post" $ do
    input ! type_ "hidden" ! name "completed" ! value "True"

showTask :: Task -> Html
showTask task = do
  let tid = toValue $ show $ fromJust $ taskId task
  li ! id tid ! class_ "task" $ do
    toHtml $ taskName task
    br
    "Priority: "
    case taskPriority task of
      "1" -> "Low"
      "2" -> "Medium"
      "3" -> "High"
      _ -> "Not set"
    br
    toHtml ("Members: " ++ (showStr (taskMembers task) ""))
  checkTask task

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


-- Comments -----

showPage :: [Comment] -> UserName -> ObjectId -> Html
showPage comments user pid = do
  h1 $ "Comments"
  li ! id "username" $ toHtml user
  script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js" $ ""
  script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
  script ! src "/static/js/comments.js" $ ""
  stylesheet "/static/css/comments.css"
  newComment user pid Nothing -- show form for making new comment
  indexComments comments pid user -- index all comments

newComment :: UserName -> ObjectId -> Maybe ObjectId -> Html
newComment username projId mparent = do
  let act = ("/" ++ (show projId) ++ "/comments")
  let pid = toValue $ show projId
  form ! id "newCommentForm" ! action (toValue act) ! method "POST" $ do
    input ! type_ "hidden" ! name "author" ! id "author" ! value (toValue username)
    input ! type_ "hidden" ! name "proj" ! id "proj" ! value pid
    input ! type_ "hidden" ! name "parent" ! value ""
    div $ do
      label ! for "text" $ h5 $ "Post a comment"
      textarea ! type_ "text" ! name "text" ! id "text" $ ""
    p $ input ! type_ "submit" ! value "Post"

indexComments :: [Comment] -> ObjectId -> UserName -> Html
indexComments coms pid user = do
  let comments = sortBy (comparing (timestamp . fromJust . commentId)) coms
  ul ! id "root" $ do
    forM_ comments $ \c -> do
      if (commentAssocProj c) == pid
        then case (commentInReplyTo c) of
          Nothing -> do
            let divid = show $ fromJust $ commentId c
            div ! class_ "comment" ! id (toValue divid) $ do
              --h6 $ "line break"
              showComment c comments user
              let rid = toValue("rb" ++ divid)
              button ! id rid ! class_ "reply-button" $ "Reply"
          Just reply -> ""  -- it'll be taken care of in showAllReplies
        else ""

showComment :: Comment -> [Comment] -> UserName -> Html
showComment comment allComments user = do
  let cid = commentId comment
  let ltime = show $ utcToLocalTime (pdt) $ timestamp $ fromJust cid
  let divid = show $ fromJust cid
  let tid = "text" ++ divid  --for comment text
  let lid = "p" ++ divid  --for parent
  let eid = "eb" ++ divid  --for edit button
  --div ! id (toValue divid) ! class_ "comment" $ do
  h3 $ toHtml $ commentAuthor comment
  p $ toHtml $ take ((length ltime) - 3) ltime
  blockquote ! id (toValue tid) $ toHtml $ (commentText comment)
  let parent = commentInReplyTo comment
  case parent of
    Just r -> li ! id (toValue lid) $ toHtml $ show r
    Nothing -> li ! id (toValue lid) $ ""
  let author = commentAuthor comment
  if (author == user) && (author /= "Anonymous")
    then button ! id (toValue eid) ! class_ "edit-button" $ "Edit"
    else ""
  showAllReplies comment allComments user

showAllReplies :: Comment -> [Comment] -> UserName -> Html
showAllReplies comment allComments user = do
  let cid = commentId comment
  forM_ allComments $ \c -> do
    if ((commentInReplyTo c) == cid)
      then do
        let divid = show $ fromJust cid
        div ! id (toValue divid) ! class_ "comment" $ showComment c allComments user
      else ""

pdt :: TimeZone
pdt = TimeZone { timeZoneMinutes = -420,
                 timeZoneSummerOnly = True, 
                 timeZoneName = "PDT" }


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
