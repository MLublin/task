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
  ul $ trace ("Printing tasks: " ++ show tasks) $ forM_ tasks $ \task -> li $ toHtml $ taskName task
  iframe ! id "peopleframe" ! src "people" $ ""

showUsers :: [UserName] -> Html
showUsers users = do
  h3 $ "Users"
  ul $ forM_ users $ \user -> li $ toHtml $ T.unpack user

newUser :: UserName -> Html
newUser user = trace "newUser" $ do
  form ! id "people" ! action "/people" ! method "post" $ do
    input ! type_ "hidden" ! name "name" ! value (toValue $ T.unpack user)
    input ! type_ "text" ! name "tasks[]" ! value ""
  script $ "document.getElementById('people').submit();"

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

headL                    :: [a] -> a
headL (x:_)              =  x

