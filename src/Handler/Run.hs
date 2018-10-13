{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Run where

import Import
import Handler.Stat (showStats)

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
