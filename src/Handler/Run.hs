{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.Run where

import Import
import Handler.Stat (showStats)

import Network.HTTP.Simple
import Text.Printf
import Data.Aeson
import System.Environment
import qualified Prelude as P
import qualified Data.Map.Strict as Map
import qualified Data.Text as T


getRunR :: RunId -> Handler Html
getRunR runId = do
  run <- runDB $ get404 runId
  stats <- runDB $ selectList [StatsRunId ==. runId] [Desc StatsHeartsPerPost]
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
  now <- liftIO getCurrentTime
  runID <- runDB $ insert $ Run
                              { runRecordedAt = now
                              }
  let e = Map.elems stats :: [Stat]
  insertRunStats runID e
  returnJson True
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


getMessagesFromGroupme :: [Message] -> Handler [Message]
getMessagesFromGroupme acc = do
  App {..} <- getYesod
  let limit = 100
      endpoint = "GET https://api.groupme.com/v3/groups/" ++ groupmeGroupID ++ "/messages?token=" ++ groupmeToken ++ "&limit=" ++ show limit ++ beforeParams
  -- case acc of
  --   [] -> print $ "first -- " ++ (show $ length acc)
  --   x -> print $ (Handler.Run.id $ P.last x) ++ " -- " ++ (show $ length acc)
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
  | (Nothing) <- l = Just Stat
                       { sUserID = userID m
                       , sUserName = name m 
                       , messageCount = 1
                       , heartsReceived = length $ favoritedBy m
                       , heartsGiven = 1
                       }
  | (Just s) <- l = Just Stat
                      { sUserID = sUserID s
                      , sUserName = sUserName s
                      , messageCount = (messageCount s) + 1
                      , heartsReceived = (heartsReceived s) + (length $ favoritedBy m)
                      , heartsGiven = heartsGiven s
                      }

addHeartGiven :: Maybe Stat -> Maybe Stat
addHeartGiven l
  | (Nothing) <- l = Nothing
  | (Just s) <- l = Just Stat
                      { sUserID = sUserID s
                      , sUserName = sUserName s
                      , messageCount = messageCount s
                      , heartsReceived = heartsReceived s
                      , heartsGiven = (heartsGiven s) + 1
                      }

addHearts :: Map.Map String Stat -> [String] -> Map.Map String Stat
addHearts statsMap favs
  | [] <- favs = statsMap
  | (f:fs) <- favs = do
      let updated = Map.alter addHeartGiven f statsMap
      addHearts updated fs

addStats :: Map.Map String Stat -> [Message] -> Map.Map String Stat
addStats statsMap msgs
  | [] <- msgs = statsMap
  | (m:ms) <- msgs = do
      let updated = Map.alter (updateStats m) (userID m) statsMap
      let updated' = addHearts updated (favoritedBy m)
      addStats updated' ms

generateStats msgs = do
  let initialMap = Map.empty :: Map.Map String Stat
  addStats initialMap msgs

insertRunStats runID stats
  | [] <- stats = return True
  | (s:xs) <- stats = do
    now <- liftIO getCurrentTime
    let userID = if sUserID s == "system"
                 then 0
                 else P.read $ sUserID s
    gmUser <- runDB $ selectFirst [GMUserGmid ==. userID] []
    gmUserID <- case gmUser of
      Nothing -> runDB $ insert $ GMUser
        { gMUserGmid = userID
        , gMUserName = T.pack $ sUserName s
        , gMUserBio = T.pack $ ""
        }
      Just (Entity gid _) -> return gid

    runDB $ insert $ Stats
                       { statsRunId = runID
                       , statsGmuserId = gmUserID
                       , statsHearts = heartsReceived s
                       , statsMessageCount = messageCount s
                       , statsHeartsGiven = heartsGiven s
                       , statsHeartsPerPost = fromIntegral (heartsReceived s) / fromIntegral (messageCount s)
                       , statsHeartsRatio = fromIntegral (heartsReceived s) / fromIntegral (heartsGiven s)
                       , statsRating = fromIntegral (heartsReceived s) / fromIntegral (messageCount s)
                       }
    insertRunStats runID xs
