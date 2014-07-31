{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Search where

--this is a test 

import qualified Data.ByteString.Lazy as L
import Conduit

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), (>=.), (<=.), update)
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import Text.Julius
import FormUtils
import Utils
import BaseForm

data Search = Search {
               startDate::Maybe Day,
               endDate:: Maybe Day
{-             , dead::Bool,
               bunnyluv::Bool,
               adopted::Bool,
               euthanized::Bool
               ,shelter::Bool
               ,other::Bool
-}               

                 }

field txt id = FieldSettings txt (Just txt) (Just id) (Just id) []

searchForm::Html -> MForm Handler (FormResult Search, Widget)
searchForm extra = do
    (startDateRes, startDateView) <- mopt textField (field "astart" "startD") Nothing
    (endDateRes, endDateView) <-mopt textField (field "aend" "endD") Nothing
    let tstartDate = text2dateM startDateRes
    let tendDate =   text2dateM endDateRes
    let searchRes = Search <$> tstartDate <*> tendDate
    let searchw = do
          [whamlet|
              #{extra}
              <div #startDate>
                Start date (leave blank if no start date):  ^{fvInput startDateView}
              <div #stopDate>
                End date (leave blank if no end date): ^{fvInput endDateView}
             <input type=submit value="submit">
                        |]
          toWidget [lucius|
                ##{fvId startDateView} {
                      width:7em;
                 }
              |]
    return (searchRes, searchw)

getSearchR::Handler Html
getSearchR = do
  (formWidget, enctype) <- generateFormPost searchForm
  let menu = 
           [whamlet|
             <div #addCance style="text-align:left; margin-top:5px; margin-bottom:8px;">
                <b> Search
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{HomeR}> cancel </a>
                   |]
  let form =   [whamlet|  <form method=post action=@{SearchR} enctype=#{enctype}>
                 ^{formWidget}
                 |]
  baseForm "Search" menu form


doStart::Maybe Day->Day
doStart Nothing = doparseTime "1/1/1890"
doStart (Just date) = date

doEnd::Maybe Day->Day
doEnd Nothing = doparseTime "12/30/2200"
doEnd (Just date) = date

querySearch (Search start stop) = runSqlite bunnyLuvDB $ 
 select $ from $ \r -> do
    where_ ((r ^. RabbitDateIn >=. val (doStart start)) &&.
           (r ^. RabbitDateIn <=. val (doEnd stop)) )
    orderBy [asc (r ^. RabbitDateIn)]
    return r
    
             
postSearchR::Handler Html
postSearchR = do
  ((result, _), _) <-runFormPost searchForm
  case result of
    FormSuccess  search  -> do
        res <- querySearch search
        base "Rabbits by Intake Dates" res
    _ -> redirect HomeR
 
