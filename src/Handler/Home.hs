{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    allComments <- runDB $ getAllComments
    lastStats <- runDB $ getLastStats

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Heartcore Collusion"
        $(widgetFile "homepage")

showStats :: Entity Stats -> Widget
showStats (Entity statId stat) = do
  gmUser <- handlerToWidget $ runDB $ get404 $ statsGmuserId stat
  [whamlet|
      <tr>
        <td>#{gMUserName gmUser}
        <td>#{statsRating stat}
        <td>#{statsHearts stat}
        <td>#{statsMessageCount stat}
        <td>#{statsHeartsPerPost stat}
        <td>#{statsHeartsRatio stat}
        <td>#{statsHeartsGiven stat}
  |]


getLastStats :: DB [Entity Stats]
getLastStats = do
  run <- selectFirst [] [Desc RunId]
  let Entity runId _ = case run of
                        Just r -> r
                        Nothing -> error "No such run"
  selectList [StatsRunId ==. runId] [Asc StatsId]

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    allComments <- runDB $ getAllComments
    lastStats <- runDB $ getLastStats

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

{-
getLastStats :: DB [Entity Stats]
getLastStats = selectList [StatsRunId ==. latestRun] [Asc StatsId]
  where latestRun = case getLatestRun of
                      Nothing -> 0
                      Just (Entity runId run) -> runId
  --lastRun <- runDB $ selectList [StatsRunId ==. getLatestRun] [Asc StatsId]
  --where sid = return (selectFirst [] [] :: [Run])

--getLatestRun :: DB (Maybe(Entity Run ))
getLatestRun =
  lift $ runDB $ selectFirst [] [Desc RunId]
-}


{--
getPhotos uid = do
    photos <- runDB $ selectList [PhotoUser ==. uid] []
  return (photos :: [Entity Photo])

getAppHomeR :: Handler Html
getAppHomeR = do
    aid <- requireAuthId
  photos <- getPhotos aid
  defaultLayout $ do
        setTitle "Welcome To Yesod!"
    $(widgetFile "app/home")
--}


getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]
