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

import           Text.Blaze.Html5 hiding (Tag, map)
import           Text.Blaze.Html5.Attributes hiding ( label, form, span
                                                    , title, style )
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.String as SR
import           Text.Regex

import Task.Models


displayPage :: UserName -> [Task] -> Html
displayPage user tasks = do
  script ! src "http://code.jquery.com/jquery-latest.min.js" $ ""
  script ! src "/static/social.js" $ ""
  stylesheet "/static/css/stylesheet.css"
  title "Task Manager"
  h1 ! class_ "top" ! id "name" $ toHtml $ T.unpack user
  div $ do
    h1 ! class_ "top" $ "Team Tasks"
    p ! id "mytasks" $ "Where tasks would go"
  div $ do
    form ! id "taskform" ! action "/task" ! method "post" $ do
      label ! for "name" $ "task name"
      input ! type_ "text" ! name "name"
      label ! for "members" $ "members"
      input ! type_ "text" ! name "members"
      input ! type_ "hidden" ! name "completed" ! value "False"
      button ! type_ "submit" $ "SEND"
  ul $ toHtml $ listTasks tasks ""
  iframe ! id "peopleframe" ! src "people" $ ""

showUsers :: [UserName] -> String
showUsers users = "<html> \
   \ <ol>" ++ loopUsers users [] ++ " </ol> </html>"
   where  loopUsers :: [UserName] -> String -> String
          loopUsers users str = 
            if users == [] then str 
            else do
              let user = headL users
              loopUsers (tail users) (str ++ "<li>" ++ T.unpack user ++ "</li>")   

listTasks :: [Task] -> String -> String
listTasks tasks str =
  if tasks == []
    then str
    else listTasks (tail tasks) (str ++ "<li>" ++ (taskName $ headL tasks) ++ "</li>")

newUser :: UserName -> String
newUser user = "<html> \ 
   \ <head> <script src=\"http://code.jquery.com/jquery-latest.min.js\"></script><script src=\"/static/social.js\"></script> \
   \ <link href=\"/static/css/stylesheet.css\" type=\"text/css\" rel=\"stylesheet\"/><title> Social Network </title></head> \
   \ <body> \
   \   <form id=\"people\" action=\"/people\" method=\"post\"> \
   \   <input type=\"hidden\" name=\"name\" value=\"" ++ (T.unpack user) ++ "\"> \
   \   <input type=\"text\" name=\"tasks[]\"> \
   \   </form> \
   \ <script> document.getElementById(\"people\").submit(); </script> \ 
   \ </body> </html>" -- ++ displayPage user []

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

respondHtml ctitle content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    title ctitle
  body $ do
    script ! src "/static/js/jquery.js" $ ""
    script ! src "/static/js/bootstrap.js" $ ""
    content

headL                    :: [a] -> a
headL (x:_)              =  x

