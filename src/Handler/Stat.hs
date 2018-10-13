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
module Handler.Stat where

import Import

showStats :: Entity Stats -> Widget
showStats (Entity statId stat) = do
  gmUser <- handlerToWidget $ runDB $ get404 $ statsGmuserId stat
  [whamlet|
      <tr>
        <td>#{gMUserName gmUser}
        <td>#{statsHeartsPerPost stat}
        <td>#{statsMessageCount stat}
        <td>#{statsHearts stat}
        <td>#{statsHeartsGiven stat}
        <td>#{statsHeartsRatio stat}
  |]

