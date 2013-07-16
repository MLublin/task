{-# LANGUAGE OverloadedStrings #-}
module Task.Views where
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
import LIO
import LIO.DCLabel
import Data.Maybe

import Task.Models


displayPage :: UserName -> [Task] -> String
displayPage user tasks =  "<html> \
   \ <head> <script src=\"http://code.jquery.com/jquery-latest.min.js\"></script><script src=\"/static/social.js\"></script> \
   \ <link href=\"/static/css/stylesheet.css\" type=\"text/css\" rel=\"stylesheet\"/><title> Social Network </title></head> \
   \ <body> \
   \   <h1 class=\"top\" id=\"name\">" ++ T.unpack user ++ " </h1> \
   \   <div id=\"div\"> \ 
   \   <h1 class=\"top\" class=\"statuses\"> Team Tasks</h1> \
   \   <p id=\"mystatus\"> where tasks would go </p> \
   \   <form id =\"statusform\"action=\"/task\" method=\"post\">\
   \     <label for=\"name\">task name</label>\
   \     <input type=\"text\" name=\"name\">\
   \     <label for=\"members\">members</label>\
   \     <input type=\"text\" name=\"members\">\
   \     <input type=\"hidden\" name=\"completed\" value=\"False\">\
   \     <button type=\"submit\">SEND</button>\
   \   </form> </div> <ul>"
   ++ listTasks tasks "" ++ "</ul>" ++
   "   <iframe id=\"peopleframe\" src=\"/people\"></iframe> \
   \ </body> \
   \ </html>"

showUsers :: [UserName] -> String
showUsers users = "<html> \
   \ <ol>" ++ loopUsers users [] ++ " </ol> </html>"
   where  loopUsers :: [UserName] -> String -> String
          loopUsers users str = 
            if users == [] then str 
            else do
              let user = head users
              loopUsers (tail users) (str ++ "<li>" ++ T.unpack user ++ "</li>")   

listTasks tasks str =
  if tasks == []
    then str
    else listTasks (tail tasks) (str ++ "<li>" ++ (taskName $ head tasks) ++ "</li>")


