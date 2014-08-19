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
import Text.Printf

data Search = Search {
               startDate::Maybe Day
              , endDate:: Maybe Day
              , bunnyluv::Maybe Bool
              , dead::Maybe Bool
              , adopted::Maybe Bool
              , euthanized::Maybe Bool
               ,shelter::Maybe Bool
               ,other::Maybe Bool
               ,source::Maybe Text
               ,status::Maybe Text
               

                 }


searchForm::Html -> MForm Handler (FormResult Search, Widget)
searchForm extra = do
    (startDateRes, startDateView) <- mopt textField (field "astart" "startD") Nothing
    (endDateRes, endDateView) <- mopt textField (field "aend" "endD") Nothing
    (iBlRes, iBlView) <- mopt checkBoxField (field "abl" "blD") (Just (Just True))
    (iDiedRes, iDiedView) <- mopt checkBoxField (field "adied" "diedD") (Just (Just True))
    (iAdoptRes, iAdoptView) <-mopt checkBoxField (field "aadopt" "adoptD") (Just (Just True))
    (iEuthRes, iEuthView) <-mopt checkBoxField (field "aeuth" "euthD") (Just (Just True))
    (iSheltRes, iSheltView) <- mopt checkBoxField (field "ashelt" "sheltD") (Just (Just True))
    (iOtherRes, iOtherView) <-mopt checkBoxField (field "aother" "otherD") (Just (Just True))
    (iSourceRes, iSourceView) <-mopt textField (field "asource" "sourceD") Nothing
    (iStatusRes, iStatusView) <-mopt textField (field "astatus" "statusD") Nothing
    let tstartDate = text2dateM startDateRes
    let tendDate =   text2dateM endDateRes
    let searchRes = Search <$> tstartDate <*> tendDate <*> iBlRes <*> iDiedRes <*> iAdoptRes <*> iEuthRes <*> iSheltRes <*> iOtherRes <*> iSourceRes <*> iStatusRes
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
                 <label .topL for="sheltD">Include rabbits from shelters: </label> ^{fvInput iSheltView}
              <div #otherInc>
                 <label .topL for="otherD">Include rabbits not from shelters: </label> ^{fvInput iOtherView}
              <div #sourcInc>
                 <label .topL for="sourceD">Source text contains : </label> ^{fvInput iSourceView}
              <div #statusInc>
                 <label .topL for="statusD">Status text contains : </label> ^{fvInput iStatusView}

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

doTest::Maybe Bool->Bool
doTest Nothing = False;
doTest (Just val) = val;

goStr::Maybe Bool->Text->Text
goStr val str = if doTest val then str else "ZZZ"

testStat r stat = (r ^. RabbitStatus ==. val stat)

doStat r bls dds ads eus = testStat r bls ||. testStat r dds ||. testStat r ads ||. testStat r eus

doSource r sh ot = (r ^. RabbitSourceType ==. val sh) ||. (r ^. RabbitSourceType ==. val ot)

doSourceContent r Nothing = (r ^. RabbitSource `like` val "%")
doSourceContent r (Just theval) = (r ^. RabbitSource `like` (%)++. val theval ++. (%))

doStatus r Nothing = (r ^. RabbitStatusNote `like` val "%")
doStatus r (Just theval) = (r ^. RabbitStatusNote `like` (%)++. val theval ++. (%))

                                
dstate r tsome = (r ^. RabbitDateIn <=. val tsome)
querySearch (Search start stop bl died adopt euth shelt tother tsource tstat) = runDB $ do
 let sh = if doTest shelt then "Shelter" else "None"
 let ot = if doTest tother then "Other" else "None"
 let bls = goStr bl "BunnyLuv"
 let dds = goStr died "Died"
 let ads = goStr adopt "Adopted"
 let eus = goStr euth "Euthanized"
 result<-    select $ from $ \r -> do
              where_ ((r ^. RabbitDateIn >=. val (doStart start)) &&.
               (r ^. RabbitDateIn <=. val (doEnd stop)) &&.
                (doStat r bls dds ads eus) &&.
                (doSource r sh ot) &&.
                (doSourceContent r tsource) &&.
                (doStatus r tstat)
               
                     )
              orderBy [asc (r ^. RabbitStatus), asc (r ^. RabbitDateIn)]
              return r

 return result

getDay Nothing = "None"
getDay (Just date) = showtime date


test:: Search-> Html
test (Search sday eday bl dd ad eu sh ot tsource tstat)=
  [shamlet|
    <div #head style="font-size:95%; font-weight:normal; float:left; text-align:left; display:inline; width:90%; padding-left:5px;" >
           <div #dates style="width:100%; float:left; border-bottom:1px solid #6f6f6f; padding-bottom:2px;" >
            <div #startDS style="width:45%; float:left;" >
             Start date: #{getDay sday}            
            <div #endDS style="width:45%; float:left;" >
              End date: #{getDay eday}
          <div #statType style="width:85%; float:left; padding-bottom:2px;" >
             Status/Source:&nbsp;
             $if (doTest bl)
                 BunnyLuv; 
             $if (doTest dd)
                 Died; 
             $if (doTest ad)
                 Adopted; 
             $if  (doTest ad)
                  Euthenized; 
             $if (doTest sh)
                  Shelter;
             $if (doTest ot)
                 &nbsp;Other
          <div #txtF style="width:100%; border-top:1px solid #6f6f6f; float:left">
              $maybe source <- tsource
               <div #sourceS style="width:55%; float:left;">
                    Source contains: #{source}
              $maybe stat <- tstat 
               <div #astatS sytle="width:45%; float:left;">
                    Status contains: #{stat}
           
              

  |]

parseSearch::Search->(String, String)
parseSearch (Search sday eday bl dd ad eu sh ot tsource tstat) = (datestring, outstring) where
  tstartDate = "Start date: " ++ (unpack (getDay sday))
  tendDate = ", End date: " ++ (unpack (getDay eday))
  bls = if doTest bl  then ", BunnyLuv" else ""
  dds = if doTest dd  then ", Died" else ""
  ads = if doTest ad  then ", Adopted" else ""
  eus = if doTest eu  then ", Euthanized" else ""
  shs = if doTest sh  then ", From shelter" else ""
  ots = if doTest ot  then ", From other" else ""
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
        base "Advanced Search" (test search) res
    _ -> redirect HomeR
 
