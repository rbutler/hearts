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
import qualified Prelude as P
import qualified Data.Map.Strict as Map

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
  msgs <- getMessagesFromGroupme []
  let stats = generateStats msgs
  returnJson $ stats
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
  --, text :: Maybe String
  , favoritedBy :: [String]
  } deriving (Generic, Show)

instance FromJSON Message where
  parseJSON = genericParseJSON customOptions
instance ToJSON Message

customOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' }

{-
getMessagesFromGroupme :: [Message] -> Handler [Message]
getMessagesFromGroupme acc = do
  App {..} <- getYesod
  let limit = 100
      endpoint = "GET https://api.groupme.com/v3/groups/" ++ groupmeGroupID ++ "/messages?token=" ++ groupmeToken ++ "&limit=" ++ show limit ++ beforeParams
  case acc of
    [] -> print $ "first -- " ++ (show $ length acc)
    x -> print $ (Handler.Run.id $ P.last x) ++ " -- " ++ (show $ length acc)
  request <- parseRequest endpoint
  resp <- httpJSON $ request
  let responseCode = getResponseStatusCode resp
  print responseCode
  case responseCode of
    304 -> return acc
    _ -> do
           let body = getResponseBody resp :: MessageResponse
               m = messages $ response body
           getMessagesFromGroupme (acc ++ m)
  where beforeParams = case acc of
                         [] -> ""
                         x -> "&before_id=" ++ (Handler.Run.id $ P.last x)
-}

getMessagesFromGroupme :: [Message] -> Handler [Message]
getMessagesFromGroupme acc = do
  App {..} <- getYesod
  let limit = 100
      endpoint = "GET https://api.groupme.com/v3/groups/" ++ groupmeGroupID ++ "/messages?token=" ++ groupmeToken ++ "&limit=" ++ show limit ++ beforeParams
  case acc of
    [] -> print $ "first -- " ++ (show $ length acc)
    x -> print $ (Handler.Run.id $ P.last x) ++ " -- " ++ (show $ length acc)
  request <- parseRequest endpoint
  resp <- httpJSON $ request
  let body = getResponseBody resp :: MessageResponse
      m = messages $ response body
  if (length m) < limit
  then return (acc ++ m)
  else getMessagesFromGroupme (acc ++ m)
  where beforeParams = case acc of
                         [] -> ""
                         x -> "&before_id=" ++ (Handler.Run.id $ P.last x)

data Stat = Stat
  { sUserID :: String
  , sUserName :: String
  , messageCount :: Int
  , heartsReceived :: Int
  , heartsGiven :: Int
  } deriving (Generic, Show)
instance FromJSON Stat
instance ToJSON Stat

updateStats :: Message -> Maybe Stat -> Maybe Stat
updateStats m l
  | (Nothing) <- l =  Just Stat
                       { sUserID = userID m
                       , sUserName = name m 
                       , messageCount = 1
                       , heartsReceived = length $ favoritedBy m
                       , heartsGiven = 0
                       }
  | (Just s) <-  l = Just Stat
                        { sUserID = sUserID s
                        , sUserName = sUserName s
                        , messageCount = (messageCount s) + 1
                        , heartsReceived = (heartsReceived s) + (length $ favoritedBy m)
                        , heartsGiven = 0
                        }

addStats :: Map.Map String Stat -> [Message] -> Map.Map String Stat
addStats statsMap msgs
  | [] <- msgs = statsMap
  | (m:ms) <- msgs = do
      let updated = Map.alter (updateStats m) (userID m) statsMap
      addStats updated ms
  
  

groupedMessages msgs = do
  let initialMap = Map.empty :: Map.Map String Stat
  addStats initialMap msgs

generateStats msgs = do
  let g = groupedMessages msgs
  g

{-
getMessagesFromGroupme :: Handler MessageResponse
getMessagesFromGroupme = do
  return $ getLimitedMessages []

getLimitedMessages acc = do
  App {..} <- getYesod
  let limit = 100
      endpoint = "GET https://api.groupme.com/v3/groups/" ++ groupmeGroupID ++ "/messages?token=" ++ groupmeToken ++ "&limit=" ++ show limit ++ beforeParams
  request <- parseRequest endpoint
  resp <- httpJSON $ request
  let body = getResponseBody resp :: MessageResponse
      m = messages $ response body
  return $ case m of
             [] -> m
             x -> x ++ (getLimitedMessages x)
  where beforeParams = case acc of
                         [] -> ""
                         x -> "&before_id=" ++ (Handler.Run.id $ last x)
-}
