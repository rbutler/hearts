{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Run where

import Import
import Handler.Stat (showStats)

import Network.HTTP.Simple
import Text.Printf
import Data.Aeson
import System.Environment

getRunR :: RunId -> Handler Html
getRunR runId = do
  run <- runDB $ get404 runId
  stats <- runDB $ selectList [StatsRunId ==. runId] [Desc StatsGmuserId]
  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Collusion stats"
    $(widgetFile "run")

getAllRunsR :: Handler Html
getAllRunsR = do
  runs <- runDB $ selectList [] [Desc RunId]
  defaultLayout $ do
    setTitle "Collusion stat runs"
    $(widgetFile "run-list")

getUpdateRunsR :: Handler Value
getUpdateRunsR = do
  messages <- getMessagesFromGroupme
  returnJson $ messages
  --let stats = statsFromMessages
  --createRun
  -- inserst stats for run
  --returnJSON run




data GroupmeResponse = GroupmeResponse
  { count :: Int
  , messages :: [Message]
  } deriving (Generic, Show)

instance FromJSON GroupmeResponse
instance ToJSON GroupmeResponse

data MessageResponse = MessageResponse
  { response :: GroupmeResponse
  } deriving (Generic, Show)

instance FromJSON MessageResponse
instance ToJSON MessageResponse

data Message = Message
  { id  :: String
  , createdAt :: Int
  , userID :: String
  , name :: String
  , groupID :: String
  , text :: String
  , favoritedBy :: [String]
  } deriving (Generic, Show)

instance FromJSON Message where
  parseJSON = genericParseJSON customOptions
instance ToJSON Message

customOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' }

--instance ToJSON Message where
  --toJSON m = object
    --[ "name" .= name Message ]
getMessagesFromGroupme :: Handler MessageResponse
getMessagesFromGroupme = do
  App {..} <- getYesod
  let limit = 1
      endpoint = "GET https://api.groupme.com/v3/groups/" ++ groupmeGroupID ++ "/messages?token=" ++ groupmeToken ++ "&limit=" ++ show limit 
      -- ++ "&before_id=" ++ show 0
  request <- parseRequest endpoint
  resp <- httpJSON $ request
  let body = getResponseBody resp :: MessageResponse
  print body
  return body

  
  


