{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, QuasiQuotes #-}

module Handler.Reports where

--this is a test 

import qualified Data.ByteString.Lazy as L
import Conduit

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update)
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack, append)
import Database.Esqueleto
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import AgeForm
import Utils

isProcedureAlter::VetVisit->Bool
isProcedureAlter vvR = tval where
  proce = vetVisitProcedures vvR
  tval | proce =="Spayed" = True
       | proce =="Neutered" = True
       | otherwise = False


reportbase atitle result = do 
     (formWidget, enctype) <- generateFormPost getNameForm
     (ageWidget, age_enctype) <-generateFormPost getAgeForm
     bnames <-  getNamesDB
     msg <-getMessage
     maid <- maybeAuthId
     auth <- isAdmin
     impath <- liftIO getImagePath
     let imgpath = unpack impath
     let mode = (maid == Just "demo")
     let isAuth=(auth==Authorized)
     today<- liftIO  getCurrentDay
     defaultLayout $ do
        setTitle atitle
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
        toWidget [julius| $( document ).ready(function(){
                             if (#{isAuth}) { 
                              $( "#cssmenu li:eq(1)" ).show(); 
                              $( "#blAdmin" ).show(); }                                                       else {
                              $( "#cssmenu li:eq(1)" ).hide(); 
                              $( "#blAdmin" ).hide(); }

                           });
                             |]
        [whamlet|
        <div #blHeaderD>
        ^{getNameWidget bnames formWidget enctype}
         ^{headerLogWid imgpath maid}
         ^{mainMenu mode}
         ^{getAgeWidget ageWidget age_enctype}
        <div #formResults>
         <div #atitleD> 
              <b> #{atitle} 
         ^{result}
         |]
        $(widgetFileNoReload def "nameC")
        toWidget [lucius| #atitleD {
                                width:100%;
                                float:left;
                                text-align:center;
                                background:#e8e8e8;
                                padding-bottom:5px;
                                padding-top:5px;
                                border-bottom:thin solid #404040;
                        }
              |]
 
twid drr = do
          [whamlet| 
           $forall (Entity drId dr) <- drr
             <div .blockDR >
               <div .rDR >
                  <div #dateDR style="float:left;">
                     #{showtime (dailyReportDate dr)}
                  <div #personDR style="float:right;">
                     #{dailyReportPerson dr}
               <div .rDR>
                    #{dailyReportReport dr}
            |]
          toWidget [lucius|
                   .blockDR {
                       border-bottom:1px solid #7f7f7f;
                       border-top:1px solid #7f7f7f;
                       width:100%;
                       float:left;
                     }

                   .rDR {
                       margin:5px;
                       margin-right:10px;
                       width:100%;
                       display:block;
                       float:left;
                     }
            |]

getDailyViewR::Handler Html
getDailyViewR = do
  drr<- runDB $ select $
               from $ \tdr-> do
               where_ ( tdr ^. DailyReportPerson !=. val "")
               return tdr
  let ls = length drr
  let ti = "Daily Report  " ++ (show ls)
  reportbase (toHtml ti) (twid drr)
                   
  
    


doAdoptedReport = runDB $ 
  select $ from $ \(tr, tvv)-> do
    where_ (tvv ^. AdoptedRabbit ==. tr ^. RabbitId)
    orderBy [desc ( tvv ^. AdoptedDate)]
    return (tr, tvv)
  
adoptedReport adoptReport = $(widgetFileNoReload def "adoptedReport")

getAdoptedViewR::Handler Html
getAdoptedViewR   = do
    aReport <-doAdoptedReport
    reportbase "Adopted Report" (adoptedReport aReport)

           
           
doWellnessReport = runDB $ 
  select $ from $ \(tr, tvv)-> do
    where_ (tvv ^. WellnessRabbit ==. tr ^. RabbitId)
    orderBy [desc ( tvv ^.WellnessDate)]
    return (tr, tvv)
  
weReport wellReport = $(widgetFileNoReload def "wellnessReport")

getWellViewR::Handler Html
getWellViewR   = do
   wellReport <-doWellnessReport
   reportbase "Wellness Report" (weReport wellReport)

doVetVisits = runDB $ 
  select $ from $ \(tr, tvv)-> do
    where_ (tvv ^. VetVisitRabbit ==. tr ^. RabbitId)
    orderBy [desc ( tvv ^. VetVisitDate)]
    return (tr, tvv)
  
vvReport vetVisits = $(widgetFileNoReload def "vetvisitReport")

getVVViewR::Handler Html
getVVViewR   = do
    vetvisits <-doVetVisits
    reportbase "Vet Visits" (vvReport vetvisits)

treatmentReport treatments = $(widgetFileNoReload def "TreatmentsReport");

getTreatmentReportR::Handler Html
getTreatmentReportR = do
  treatments <- queryTreatments
  let ls = length treatments
  let ti = append (pack (show ls)) " Treatments "
  reportbase (toHtml ti) (treatmentReport treatments)
  
