{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

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
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar



reportbase atitle result = do 
     msg <-getMessage
     maid <- maybeAuthId
     auth <- isAdmin
     impath <- liftIO getImagePath
     let imgpath = unpack impath
     let mode = (maid == Just "demo")
     let isAuth=(auth==Authorized)
     today<- liftIO $ getCurrentDay
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
         <div #dummy > nothing here
         ^{headerLogWid imgpath maid}
         ^{mainMenu mode}
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
 





doAdoptedReport = runDB $ do
  zapt <- select $ from $ \(tr, tvv)-> do
    where_ (tvv ^. AdoptedRabbit ==. tr ^. RabbitId)
    orderBy [desc ( tvv ^. AdoptedDate)]
    return ((tr, tvv))
  return zapt
  
adoptedReport adoptReport = $(widgetFileNoReload def "adoptedReport")

getAdoptedViewR::Handler Html
getAdoptedViewR   = do
    aReport <-doAdoptedReport
    reportbase "Adopted Report" (adoptedReport aReport)

           
           
doWellnessReport = runDB $ do
  zapt <- select $ from $ \(tr, tvv)-> do
    where_ (tvv ^. WellnessRabbit ==. tr ^. RabbitId)
    orderBy [desc ( tvv ^.WellnessDate)]
    return ((tr, tvv))
  return zapt
  
weReport wellReport = $(widgetFileNoReload def "wellnessReport")

getWellViewR::Handler Html
getWellViewR   = do
   wellReport <-doWellnessReport
   reportbase "Wellness Report" (weReport wellReport)

doVetVisits = runDB $ do
  zapt <- select $ from $ \(tr, tvv)-> do
    where_ (tvv ^. VetVisitRabbit ==. tr ^. RabbitId)
    orderBy [desc ( tvv ^. VetVisitDate)]
    return ((tr, tvv))
  return zapt
  
vvReport vetVisits = $(widgetFileNoReload def "vetvisitReport")

getVVViewR::Handler Html
getVVViewR   = do
    vetvisits <-doVetVisits
    reportbase "Vet Visits" (vvReport vetvisits)
