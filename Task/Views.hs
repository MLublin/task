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
import Data.List hiding (head, span)
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

displayHomePage :: UserName -> [Project] -> [String] -> Html
displayHomePage user projects notifs = do
  div ! class_ "welcome" $ do
   h2 $ toHtml $ "Welcome " ++ T.unpack user
   a ! href "/projects/new" ! id "create-project" $ "Create new project"
  div ! class_ "row-fluid" $ do
    div ! class_ "span4" ! id "left" $ do
      div ! id "projects" $ do
        let unfinished = filter (not . projectCompleted) projects
        if (unfinished == []) 
          then ""
          else do 
            ul $ forM_ unfinished $ \proj -> do
              let pid = show $ fromJust $ projectId proj
              li $ a ! href (toValue ("/projects/" ++ pid)) $ toHtml (projectTitle proj)
        let finished = filter (projectCompleted) projects
        if (finished == [])
          then ""
          else do 
            h2 $ "Completed Projects"
            ul $ forM_ finished $ \proj -> do
              let pid = show $ fromJust $ projectId proj
              li $ a ! href (toValue ("/projects/" ++ pid)) $ toHtml (projectTitle proj)
    div ! class_ "span8" ! id "notifications" $ do
      if (notifs == [] )
        then ""
        else do
          h2 $ "Notifications"
	  form ! action "/notifs/removeall" ! id "removeallnotifs" ! method "post" $ do
	    button ! class_ "removeNotif" ! type_ "submit" $ "Remove All Notifications"
          ul $ forM_ notifs $ \notif -> li ! class_ "notifs" $ do
	    form ! class_ "removenotif" ! action (toValue ("/notifs/" ++ (show $ fromJust $ elemIndex notif notifs) ++ "/remove")) ! method "post" $ do
	      button ! class_ "removeNotif" ! type_ "submit" $ "X"
	      toHtml notif

displayProjectPage :: User -> [Task] -> Project -> Html
displayProjectPage user tasks project = do
  p ! id "username" ! class_ "hidden" $ toHtml $ T.unpack (userName user)
  div ! id "projectinfo" $ do
    div $ h2 ! class_ "top" ! id "name" $ toHtml $ projectTitle project
    div $ blockquote ! class_ "top" ! id "desc" $ toHtml $ projectDesc project
    let pid = show $ fromJust $ projectId project
    div $ do
      let act = "/projects/" ++ pid ++ "/leave"
      form ! id "leaveproj" ! action (toValue act) ! method "post" $ do
        button ! class_ "inline pull-left" ! type_ "submit" $ "Leave project"
    div $ do
      if (userName user `elem` projectLeaders project) 
      then do
        let act = "/projects/" ++ pid ++ "/remove"
        form ! id "removeproj" ! action (toValue act) ! method "post" $ do
          button ! class_ "inline pull-left" ! type_ "submit" $ "Remove Project"
      else ""
    div $ do
      if (userName user `elem` projectLeaders project) 
      then form ! action (toValue ("/projects/" ++ pid ++ "/edit")) $ do
        button ! type_ "submit" $ "Edit Project"
      else ""
    div $ do
      p $ do
        span ! class_ "timetitle" $ "Start Date: " 
        toHtml $ projectStartTime project 
        span ! class_ "timetitle" $ "   |   End Date: " 
        toHtml $ projectEndTime project
  div ! class_ "row-fluid" ! id "page" $ do
    div ! class_ "span2" $ do
      h4 $ "Members:"
      ul $ forM_ (projectMembers project) $ \user -> li $ toHtml $ T.unpack user
    div ! class_ "span5" ! id "alltasks" $ displayTasks user tasks project
>>>>>>> 14a0e373270611a482cde1bd173080783b16ad67
    div ! class_ "span5" ! id "comments" $ do
      h2 $ "Project Chat"
      let pid = show $ fromJust $ projectId project
      iframe ! id "commentframe" ! src (toValue ("/" ++ pid ++ "/comments")) $ ""

  
newProject :: UserName -> [UserName] -> Html
newProject user members = do
  form ! id "newprojectform" ! action "/projects" ! method "post" $ do
    p $ do
      label ! for "title" $ "Project title: "
      input ! type_ "text" ! name "title"
    p $ do
      label ! for "desc" $ "Project description: "
      textarea ! name "desc" $ ""
    div ! id "memberSelect" $ do
      h5 $ "Select members for this project:"
      forM_ members $ \member -> do
                          input ! type_ "checkbox" ! class_ "memberCheckbox" ! name "members[]" ! value (toValue member) 
                          toHtml $ T.unpack member  
    div ! id "leaderSelect" $ do
      h5 $ "Select leaders for this project:"
      forM_ members $ \member -> do
                          div ! class_ (toValue ("leaderCheckbox " ++ (T.unpack member))) $ do
                            input ! type_ "checkbox" ! class_ (toValue ("leaderCheckbox " ++ (T.unpack member))) ! name "leaders[]" ! value (toValue member) 
                            toHtml $ T.unpack member  
    h5 $ do
      label ! for "startTime" $ "Project start time: "
      input ! type_ "date" ! name "startTime"
    h5 $ do
      label ! for "endTime" $ "Project end time: "
      input ! type_ "date" ! name "endTime"
    input ! type_ "hidden" ! name "completed" ! value "False"
    input ! type_ "hidden" ! name "tasks[]" ! value ""
    button ! type_ "submit" $ "Add Project"

editProject :: Project -> UserName -> [UserName] -> Html
editProject project user allnames  = do
  form ! id "editprojectform" ! action "/projects/edit" ! method "post" $ do 
    p $ do
      label ! for "title" $ "Project title: "
      input ! type_ "text" ! name "title" ! value (toValue $ projectTitle project)
    p $ do
      label ! for "desc" $ "Project description: "
      textarea ! name "desc" $ "" ! value (toValue $ projectDesc project)
    div ! id "memberSelect" $ do
      p $ "Select members for this project:"
      forM_ allnames $ \member -> do
                          if (member `elem` (projectMembers project)) 
			  then input ! type_ "checkbox" ! checked "checked" ! class_ "memberCheckbox" ! name "members[]" ! value (toValue member) 
			  else input ! type_ "checkbox" ! class_ "memberCheckbox" ! name "members[]" ! value (toValue member) 
                          toHtml $ T.unpack member  
      
    div ! id "leaderSelect" $ do
      p $ "Select leaders for this project:"
      forM_ allnames $ \member -> do
                          div ! class_ (toValue ("leaderCheckbox " ++ (T.unpack member))) $ do
                            if (member `elem` (projectLeaders project))
			    then input ! type_ "checkbox" ! checked "checked" ! class_ (toValue ("leaderCheckbox " ++ (T.unpack member))) ! name "leaders[]" ! value (toValue member) 
			    else input ! type_ "checkbox" ! class_ (toValue ("leaderCheckbox " ++ (T.unpack member))) ! name "leaders[]" ! value (toValue member) 
                            toHtml $ T.unpack member  
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
        input ! type_ "radio" ! checked "checked" ! name "completed" ! value "False"
        "In Progress"
    input ! type_ "hidden" ! name "tasks[]" ! value (toValue $ show $ projectTasks project)
    input ! type_ "hidden" ! name "_id" ! value (toValue $ show $ projectId project)
    button ! type_ "submit" $ "Edit Project"


-- Tasks -----

displayTasks :: User -> [Task] -> Project -> Html
displayTasks user tasks project = do
  div $ do
    h2 ! class_ "top" $ "Team Tasks"
  div $ do
    if not $ projectCompleted project 
    then do
      button ! id "newtaskbttn" $ "Add New Task"
      let pid = show $ fromJust $ projectId project
      let act = "/projects/" ++ pid ++ "/tasks"
      form ! id "newtaskform" ! action (toValue act) ! method "post" $ do
        p $ do
          label ! for "name" $ "Task: "
          textarea ! name "name" $ ""
        p $ do 
          "Members:"
          br
          forM_ (projectMembers project) $ \member -> do
                              input ! type_ "checkbox" ! class_ "memberCheckbox" ! name "members[]" ! value (toValue member) 
                              toHtml $ T.unpack member  
        p $ do
          label ! for "priority" $ "Task priority: "
          select ! name "priority" $ do
            option ! value "3" $ "Low"
            option ! value "2" $ "Medium"
            option ! value "1" $ "High"
        input ! type_ "hidden" ! name "project" ! value (toValue pid)
        input ! type_ "hidden" ! name "completed" ! value "False"
        button ! type_ "submit" $ "Add Task"
    else ""
  div ! id "tasks" $ do
    let mytasks = filter (\t -> (userName user) `elem` (taskMembers t)) tasks
    if (mytasks == [])
      then ""
      else h3 ! id "taskheader" $ "My tasks" 
    div ! id "incomplete_tasks" $ do
      let incomplete = filter (not . taskCompleted) mytasks
      let low = filter (\t -> (taskPriority t) == "3") incomplete
      let med = filter (\t -> (taskPriority t) == "2") incomplete
      let high = filter (\t -> (taskPriority t) == "1") incomplete
      div ! id "tasks1" ! class_ "tasklist" $ do
        if (high == []) then ""
        else do
          h5 ! id "HighHeader" ! class_ "blue headertp1" $ "High Priority: "
          forM_ high $ \task -> showTask task
      div ! id "tasks2" ! class_ "tasklist" $ do
        if (med == []) then ""
        else do
          h5 ! id "MediumHeader" ! class_ "blue headertp2" $ "Medium Priority: "
          forM_ med $ \task -> showTask task
      div ! id "tasks3" ! class_ "tasklist" $ do
        if (low == []) then ""
        else do
          h5 ! id "LowHeader" ! class_ "blue headertp3" $ "Low Priority: "
          forM_ low $ \task -> showTask task
    div $ do
      let complete = sortBy (comparing taskPriority) $ filter taskCompleted mytasks
      let low = filter (\t -> (taskPriority t) == "3") complete
      let med = filter (\t -> (taskPriority t) == "2") complete
      let high = filter (\t -> (taskPriority t) == "1") complete
      if (complete == [])
        then ""
        else h4 $ "Completed:"
      div ! id "complete_tasks" ! class_ "tasklist" $ do
        p ! class_ "hightasks" $ forM_ high $ \task -> showCompletedTask task
        p ! class_ "medtasks" $ forM_ med $ \task -> showCompletedTask task
        p ! class_ "lowtasks" $ forM_ low $ \task -> showCompletedTask task
    div ! id "other_tasks" $ do
      let otasks = filter (\t -> not $ (userName user) `elem` (taskMembers t)) tasks
      if (otasks == [])
        then ""
        else do
          h3 ! id "othertasksheader" $ "Other tasks"
          forM_ otasks $ \task -> 
            li $ toHtml $ showTask task
    h2 $ ""

showTask :: Task -> Html
showTask task = do
  let tid = toValue $ show $ fromJust $ taskId task
  li ! id tid ! class_ "taskbullet" $ do
      let tid = show $ fromJust $ taskId task
      let fid = toValue ("form" ++ tid)
      let act = "/tasks/" ++ tid ++ "/edit"
      form ! id fid ! class_ (toValue ("complete_tasks_form taskp" ++ (taskPriority task))) ! action (toValue act) ! method "post" $ do
        input ! type_ "hidden" ! name "completed" ! value "True"
        button ! type_ "submit" $ "X"
        toHtml (taskName task)
        br
        blockquote $ toHtml ("Members: " ++ (showStr (taskMembers task) ""))

showCompletedTask :: Task -> Html
showCompletedTask task = do
  let tid = toValue $ show $ fromJust $ taskId task
  li ! id tid ! class_ "taskbullet" $ do
      let tid = show $ fromJust $ taskId task
      let fid = toValue ("complete" ++ tid)
      let act = "/tasks/" ++ tid ++ "/remove"
      form ! id fid ! class_ "remove_tasks_form" ! action (toValue act) ! method "post" $ do
        button ! type_ "submit" $ "X"
        toHtml (taskName task)
        br
        blockquote $ toHtml ("Members: " ++ (showStr (taskMembers task) ""))  


-- Users -----

showUsers :: [UserName] -> Html
showUsers users = do
  h3 $ "Users"
  ul $ forM_ users $ \user -> li $ toHtml $ T.unpack user


newUser :: UserName -> Html
newUser user = do
  form ! id "people" ! action "/people" ! method "post" $ do
    input ! type_ "hidden" ! name "name" ! value (toValue $ T.unpack user)
    input ! type_ "text" ! name "notifs[]" ! value ""
    input ! type_ "text" ! name "tasks[]" ! value ""
    input ! type_ "text" ! name "projects[]" ! value ""
  script $ "document.getElementById('people').submit();"


-- Comments -----

showPage :: [Comment] -> UserName -> ObjectId -> Html
showPage comments user pid = do
  li ! id "username" $ toHtml user
  script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js" $ ""
  script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
  script ! src "/static/js/comments.js" $ ""
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
  let comments = reverse $ sortBy (comparing (timestamp . fromJust . commentId)) coms
  ul ! id "root" $ do
    forM_ comments $ \c -> do
      if (commentAssocProj c) == pid
        then case (commentInReplyTo c) of
          Nothing -> do
            let divid = show $ fromJust $ commentId c
            div ! class_ "comment" ! id (toValue divid) $ do
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
    stylesheet "/static/css/bootstrap.css"
    title ctitle
  body ! id "body" $ do
    div ! class_ "navbar navbar-fixed-top navbar-inverse" ! id "page-nav" $ do
      div ! class_ "navbar-inner" $ do
        div ! class_ "container-fluid" $ do
          a ! href "/" ! class_ "brand" $ "Home" 
    script ! src "/static/js/jquery.js" $ ""
    script ! src "/static/js/bootstrap.js" $ ""
    script ! src "http://code.jquery.com/jquery-latest.min.js" $ ""
    script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
    script ! src "/static/js/jquery.form.js" $ ""
    script ! src "/static/js/jquery.js" $ ""
    script ! src "/static/js/jquery.mockjax.js" $ ""
    script ! src "/static/js/jquery-1.6.4.js" $ ""
    script ! src "/static/js/jquery-1.7.2.js" $ ""
    script ! src "/static/js/jquery-1.8.3.js" $ ""
    script ! src "/static/js/jquery-1.9.0.js" $ ""
    script ! src "/static/js/jquery.validate.js" $ ""
    script ! src "/static/js/jquery.validate.min.js" $ ""
    script ! src "/static/js/additional-methods.js" $ ""
    script ! src "/static/js/additional-methods.min.js" $ ""
    script ! src "/static/js/tasks.js" $ ""
    script ! src "/static/js/newtask.js" $ ""
    script ! src "/static/js/user_select.js" $ ""
    content

respondHtmlC ctitle content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    stylesheet "/static/css/comments.css"
    stylesheet "/static/css/bootstrap.css"
    --stylesheet "/static/css/task2.css"
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
