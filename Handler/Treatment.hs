{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, RankNTypes #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}


module Handler.Treatment where

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
import Data.Text (Text, unpack)
import Database.Esqueleto
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import FormUtils
import Data.Time

data Instruct = Instruct {
            idate::Day
           ,instruct::Textarea
           ,ifreq::Maybe Text
           ,istopday::Maybe Day
            } deriving Show

data Treat = Treat {
             tdate::Day
            ,tinstructs::[Instruct]
            ,tstop::Maybe Day
           } deriving Show


insertTreatmentB treat = runDB $ insert treat

testTreatDay::forall m. Text->Maybe TreatmentB->(TreatmentB->Day)->Maybe Text
testTreatDay now  Nothing field = Just now
testTreatDay now (Just atreat) field = Just (showtime (field atreat))

goStop::Text->Maybe Day->Maybe Text
goStop now Nothing = Just now
goStop now (Just day) = Just (date2text day)

testStopDayOpt::Text->Maybe TreatmentB->(TreatmentB->Maybe Day)->Maybe (Maybe Text)
testStopDayOpt now Nothing field = Just Nothing
testStopDayOpt now (Just atreat) field = Just (goStop now (field atreat))

testTreatField Nothing _ = Nothing
testTreatField (Just treat) f = Just (f treat)

treatmentForm::Maybe Text->RabbitId->Maybe TreatmentB->Html-> MForm Handler (FormResult TreatmentB, Widget)
treatmentForm user rabID treatment extra = do
    local_time <- liftIO  getLocalTime
    let stime = showtime (localDay local_time)
    (treatDateRes, treatDateView)<-mreq textField "nope" (testTreatDay stime treatment treatmentBStart )
    (treatReasonRes, treatReasonView)<-mreq textField "nope" (testTreatField treatment treatmentBReason)
    (treatTreatmentRes,treatTreatmentView)<-mreq textareaField "nope" (testTreatField treatment treatmentBInstruct)
    (treatStopRes, treatStopView)<- mopt textField "nope"  (testStopDayOpt stime treatment treatmentBStop)
    
    let date = text2date treatDateRes
    let stopdate = text2dateM treatStopRes
    let treatmentRes = TreatmentB rabID <$> date <*> 
                         treatReasonRes <*> treatTreatmentRes <*> stopdate
    let twid = $(widgetFileNoReload def "Treatment");
    return (treatmentRes, twid)
    

extractTreat (Entity treatID treat) = treat

getTreatmentR::RabbitId->Handler Html
getTreatmentR rabId  = do
    maid <- maybeAuthId
    treatments<-queryTreatmentB rabId
    let numtreats = length treatments
    let treatment = if null treatments then Nothing else (Just (extractTreat (Prelude.head treatments)))
    Just rabbit <-runDB  $ get rabId
    (treatmentWid, enctype) <-generateFormPost (treatmentForm maid rabId treatment)
    let menu = [whamlet|
               <div #addCance style="float:inherit; text-align:left; margin:10px;">
                <b> Treatment  for &nbsp;  #{rabbitName rabbit} : (#{numtreats})
                <div #treatCan style="float:right; display:inline;">
                  <div .cancelBut #wellEdCan style="display:inline; float:right;">
                   <a href=@{ViewR rabId }> cancel </a>
                |]
    let form = [whamlet| 
              <form method=post action=@{TreatmentR rabId} enctype=#{enctype}>
                 ^{treatmentWid}
            |]

    baseForm "Treatment" menu form



postTreatmentR::RabbitId->Handler Html
postTreatmentR rabId = do
  maid <-maybeAuthId
  treatments<-queryTreatmentB rabId
  let treatment = if null treatments then Nothing else (Just (extractTreat (Prelude.head treatments)))
  ((tresult, _), _) <-runFormPost (treatmentForm maid rabId treatment)
  case tresult of
    FormSuccess atreatment -> do
      runDB $ do
        insert atreatment
        return ()
      redirect (ViewR rabId)
    _ -> do
          msg<-getMessage
          defaultLayout [whamlet| Error in Posting Form  
                          $maybe ms <-msg
                              ms
                      |]
 
