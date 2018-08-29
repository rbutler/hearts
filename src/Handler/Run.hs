{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Run where

import Import

getRunR :: RunId -> Handler Html
getRunR runId = do
  run <- runDB $ get404 runId
  stats <- runDB $ selectList [StatsRunId ==. runId] [Desc StatsGmuserId]
  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Collusion stats"
    $(widgetFile "run")
