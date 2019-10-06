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
import Numeric

showStats :: Entity Stats -> Widget
showStats (Entity statId stat) = do
  gmUser <- handlerToWidget $ runDB $ get404 $ statsGmuserId stat
  [whamlet|
      <tr>
        <td>#{gMUserName gmUser}
        <td>#{formatDecimal statsHeartsPerPost stat}
        <td>#{statsMessageCount stat}
        <td>#{statsHearts stat}
        <td>#{statsHeartsGiven stat}
        <td>#{formatDecimal statsHeartsRatio stat}
  |]
  where
    formatDecimal f s = showFFloat (Just 2) (f s) ""
