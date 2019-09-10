{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.User where

import Import

getUserR :: GMUserId -> Handler Html
getUserR gmuserId = do
  gmuser <- runDB $ get404 gmuserId
  defaultLayout $ do
    $(widgetFile "user")

getAllUsersR :: Handler Html
getAllUsersR = do
  users <- runDB $ selectList [] [Desc GMUserId]
  defaultLayout $ do
    setTitle "Users"
    $(widgetFile "user-list")
