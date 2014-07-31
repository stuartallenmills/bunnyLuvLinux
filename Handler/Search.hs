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
import Yesod hiding ((!=.), (==.), (=.), (>=.), (<=.), (||.), update)
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
               startDate::Maybe Day
              , endDate:: Maybe Day
              , bunnyluv::Bool
              , dead::Bool
              , adopted::Bool
              , euthanized::Bool
               ,shelter::Bool
               ,other::Bool
               

                 }

field txt id = FieldSettings txt (Just txt) (Just id) (Just id) []

searchForm::Html -> MForm Handler (FormResult Search, Widget)
searchForm extra = do
    (startDateRes, startDateView) <- mopt textField (field "astart" "startD") Nothing
    (endDateRes, endDateView) <- mopt textField (field "aend" "endD") Nothing
    (iBlRes, iBlView) <- mreq boolField (field "abl" "blD") (Just True)
    (iDiedRes, iDiedView) <- mreq boolField (field "adied" "diedD") (Just True)
    (iAdoptRes, iAdoptView) <- mreq boolField (field "aadopt" "adoptD") (Just True)
    (iEuthRes, iEuthView) <- mreq boolField (field "aeuth" "euthD") (Just True)
    (iSheltRes, iSheltView) <- mreq boolField (field "ashelt" "sheltD") (Just True)
    (iOtherRes, iOtherView) <- mreq boolField (field "aother" "otherD") (Just True)
    let tstartDate = text2dateM startDateRes
    let tendDate =   text2dateM endDateRes
    let searchRes = Search <$> tstartDate <*> tendDate <*> iBlRes <*> iDiedRes <*> iAdoptRes <*> iEuthRes <*> iSheltRes <*> iOtherRes
    let searchw = do
          [whamlet|
              #{extra}
             <div #gsearchForm>
              <div #startDate>
                <label .topL for="startD">Intake start date: </label>  ^{fvInput startDateView}   (leave blank if no start date)
              <div #stopDate>
               <label .topL for="endD">Intake end date : </label> ^{fvInput endDateView}   (leave blank if no end date)
              <div #blInc>
                <label .topL for="blD"> Include BunnyLuv rabbits: </label> ^{fvInput iBlView}
              <div #diedInc>
                 <label .topL for="diedD"> Include rabbits that died: </label> ^{fvInput iDiedView}
              <div #adoptInc>
                 <label .topL for="adoptD"> Include adopted rabbits: </label> ^{fvInput iAdoptView}
              <div #euthInc>
                 <label .topL for="euthD"> Include euthanized rabbits: </label> ^{fvInput iEuthView}
              <div #sheltInc>
                 <label .topL for="sheltD">Include rabbits from shelter: </label> ^{fvInput iSheltView}
              <div #otherInc>
                 <label .topL for="otherD">Include rabbits not from shelter: </label> ^{fvInput iOtherView}

              <input type=submit value="submit">
                        |]
          toWidget [lucius|
                ##{fvId startDateView} {
                      width:7em;
                 }
                ##{fvId endDateView} {
                    width:7em;
                 }
                #gsearchForm input {
                    display:inline;
                 }
                #gsearchForm div {
                   margin-top:10px;
                   margin-bottom:10px;
                 }

                #gsearchForm .topL {
                   display:inline-block;
                   width:35%;
                 }
                #gsearchForm label {
                    margin-right:10px;
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

dstate r tsome = (r ^. RabbitDateIn <=. val tsome)
querySearch (Search start stop bl died adopt euth shelt tother) = runSqlite bunnyLuvDB $ do
 let sh = if shelt then "Shelter" else "None"
 let ot = if tother then "Other" else "None"
 rbl<- if bl then
        select $ from $ \r -> do
              where_ ((r ^. RabbitDateIn >=. val (doStart start)) &&.
               (r ^. RabbitDateIn <=. val (doEnd stop)) &&.
                (r ^. RabbitStatus ==. val "BunnyLuv")  &&.
                ( (r ^. RabbitSourceType ==. val sh) ||. (r ^. RabbitSourceType ==. val ot)) 
               
                     )
              orderBy [asc (r ^. RabbitDateIn)]
              return r
        else return []
 rd<- if died then
        select $ from $ \r -> do
              where_ ((r ^. RabbitDateIn >=. val (doStart start)) &&.
               (r ^. RabbitDateIn <=. val (doEnd stop)) &&.
               (r ^. RabbitStatus ==. val "Died") &&.
               ( (r ^. RabbitSourceType ==. val sh) ||. (r ^. RabbitSourceType ==. val ot))
                     )
              orderBy [asc (r ^. RabbitDateIn)]
              return r
        else return []
 radop<- if adopt then
        select $ from $ \r -> do
              where_ ((r ^. RabbitDateIn >=. val (doStart start)) &&.
               (r ^. RabbitDateIn <=. val (doEnd stop)) &&.
               (r ^. RabbitStatus ==. val "Adopted") &&.
               ( (r ^. RabbitSourceType ==. val sh) ||. (r ^. RabbitSourceType ==. val ot))
                     )
              orderBy [asc (r ^. RabbitDateIn)]
              return r
        else return []
 reuth<- if euth then
        select $ from $ \r -> do
              where_ ((r ^. RabbitDateIn >=. val (doStart start)) &&.
               (r ^. RabbitDateIn <=. val (doEnd stop)) &&.
               (r ^. RabbitStatus ==. val "Euthenized")&&.
               ( (r ^. RabbitSourceType ==. val sh) ||. (r ^. RabbitSourceType ==. val ot))
                     )
              orderBy [asc (r ^. RabbitDateIn)]
              return r
        else return []
 return (rbl++rd++radop ++ reuth)

getDay Nothing = "None"
getDay (Just date) = showtime date

parseSearch::Search->(String, String)
parseSearch (Search sday eday bl dd ad eu sh ot) = (datestring, outstring) where
  tstartDate = "Start date: " ++ (unpack (getDay sday))
  tendDate = ", End date: " ++ (unpack (getDay eday))
  bls = if bl then ", BunnyLuv" else ""
  dds = if dd then ", Died" else ""
  ads = if ad then ", Adopted" else ""
  eus = if eu then ", Euthanized" else ""
  shs = if sh then ", From shelter" else ""
  ots = if ot then ", From other" else ""
  datestring = "Search: " ++ tstartDate++tendDate
  outstring = bls++dds++ads++eus++shs++ots
  
             
postSearchR::Handler Html
postSearchR = do
  ((result, _), _) <-runFormPost searchForm
  case result of
    FormSuccess  search  -> do
        res <- querySearch search
        let (st, en) = parseSearch search
        let cap = toHtml (pack (st++en))       
        base cap res
    _ -> redirect HomeR
 
