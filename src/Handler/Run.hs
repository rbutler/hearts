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
  messages <- getMessagesFromGroupme []
  let stats = generateStats messages
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

--instance ToJSON Message where
  --toJSON m = object
    --[ "name" .= name Message ]
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

data Stat = Stat
  { sUserID :: String
  , sUserName :: String
  , messages :: Int
  , heartsReceived :: Int
  , heartsGiven :: Int

updateValue :: Maybe Stat -> Message -> Maybe Stat
updateValue
  | Nothing m = Just Stat
  | (Just s) m = Just Stat
                    { sUserID = sUserID s
                    , sUserName = sUserName 
                    , messages = (messages s) + 1
                    , heartsReceived = (heartsReceived s) + (length $ favoritedBy)
                    }
  

groupedMessages messages =
  let m - Map.empty :: Map string Stat
  

generateStats messages = do
  let g = groupedMessages messages
  
  


