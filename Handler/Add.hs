{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Add where

--this is a test 

import qualified Data.ByteString.Lazy as L
import Conduit

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.))
import Yesod.Default.Util
import Foundation

import Data.Text (Text, unpack)
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative





headerWidget::Widget
headerWidget = $(widgetFileNoReload def "header")

-- menuWidget::Widget
-- menuWidget = $(widgetFileNoReload def "menu")



queryWellness rabID = runSqlite "test5.db3" $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. WellnessRabbit ==. val rabID)
     orderBy [desc (r ^. WellnessDate)]
     return (r)
  return zipt

queryVetVisits rabID = runSqlite "test5.db3" $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. VetVisitRabbit ==. val rabID)
     orderBy [desc (r ^. VetVisitDate)]
     return (r)
  return zipt
  
test mrab field = case mrab of
                    Nothing->Nothing
                    Just ri->Just (field ri)


opttest::(Maybe Rabbit)->(Rabbit->Text)->(Maybe Text)
opttest mrab field= case mrab of
                   Nothing-> Just ""
                   Just ri-> Just (field ri)


    
wellnessForm::RabbitId->Html-> MForm Handler (FormResult Wellness, Widget)
wellnessForm rabID extra = do
    (wellDateRes, wellDateView)<-mreq textField "nope" Nothing
    (wellLbsRes, wellLbsView)<-mreq intField "nope" Nothing
    (wellOzRes, wellOzView)<-mreq intField "nope" Nothing
    (wellTempRes, wellTempView)<-mopt doubleField "nope" Nothing
    (wellNoteRes, wellNotesView)<-mreq textField "nope" Nothing
    (wellGroomedRes, wellGroomedView)<-mreq boolField "nope" (Just False)
    (wellTreatmentRes, wellTreatmentView)<-mreq textField "nope" Nothing
    (wellResponsibleRes, wellResponsibleView)<-mreq textField "nope" Nothing
    
    let date = fmap (doparseTime.unpack) wellDateRes
    let wellnessRes = Wellness rabID <$> date <*> wellGroomedRes <*>
                        wellTempRes <*> (Weight <$> wellLbsRes <*> wellOzRes) <*>
                         wellNoteRes <*> wellTreatmentRes <*> wellResponsibleRes
    let twid = $(widgetFileNoReload def "wellness")
    return (wellnessRes, twid)
    

showWellness wellness =   $(widgetFileNoReload def "showwellness")

    

rabbitForm ::(Maybe Rabbit, Maybe [Entity Wellness])-> Html -> MForm Handler (FormResult Rabbit, Widget)
rabbitForm (mrab, rabID) extra = do
    let tname = case mrab of
          Nothing -> Nothing
          Just rb -> (Just (rabbitName rb))
    (nameRes, nameView) <- mreq textField "this is not used" tname
    (descRes, descView) <- mreq textField "neither is this"  (test mrab rabbitDesc)
    (dateInRes, dateInView)<-mreq textField "  " (test mrab rabbitDateIn)
    (sourceRes, sourceView) <- mreq textField "neither is this" (test mrab rabbitSource)
    (sexRes, sexView) <- mreq (selectFieldList sex) "not" (test mrab rabbitSex)
    (alteredRes, alteredView) <- mreq (selectFieldList altered) "not" (test mrab rabbitAltered)
    (statusRes, statusView) <- mreq (selectFieldList status) "who" (test mrab rabbitStatus)
    (ageIntakeRes, ageIntakeView) <- mreq textField "zip" (test mrab rabbitAgeIntake)
    (sourceTypeRes, sourceTypeView) <- mreq (selectFieldList sourceType) "zip" (test mrab rabbitSourceType)
    (statusDateRes, statusDateView) <- mreq textField " nope" (opttest mrab rabbitStatusDate)
    (statusNoteRes, statusNoteView) <- mreq textField "nope" (opttest mrab rabbitStatusNote)
    let rabbitUpdateRes =Rabbit <$>  nameRes <*>  descRes <*> dateInRes <*> sourceTypeRes <*> sourceRes <*> sexRes <*> alteredRes <*> ageIntakeRes <*> statusRes <*> statusDateRes <*> statusNoteRes      
 --   let rabbitRes = rabbitRes1 <*> (FormResult (Just True)) <*> (FormResult Nothing) Nothing
    let awid=  $(widgetFileNoReload def "add")
    return (rabbitUpdateRes, awid)




getAddR ::Handler Html    
getAddR  = do
    (formWidget, enctype) <- generateFormPost (rabbitForm (Nothing,Nothing))
    defaultLayout $ do
         setTitle "Add Rabbit"
         [whamlet|
             ^{headerWidget}
              <div #addCance style="text-align:center">
                 <b> Add Rabbit
              <form method=post action=@{PostR} enctype=#{enctype}>
                 ^{formWidget}
          |]
  
         
postPostR::Handler Html
postPostR = do
  (((result), _), _) <-runFormPost (rabbitForm (Nothing, Nothing))
  case result of
    FormSuccess  rabi -> do
      runSqlite "test5.db3" $ do
        _ <-insert $ rabi
        return ()
    _ -> return ()
  redirect HomeR

-- update a rabbit 
postUpdateR::RabbitId->Handler Html
postUpdateR rabID = do
  (((result), _), _) <-runFormPost (rabbitForm (Nothing,Nothing))

  case result of
    FormSuccess rabi -> do
      runSqlite "test5.db3" $ do
        _ <-replace  rabID rabi
        return ()
    _ -> return ()

  redirect HomeR

postWellnessR::RabbitId->Handler Html
postWellnessR rabID = do
  (((result), _), _) <-runFormPost (wellnessForm rabID)
  case result of
    FormSuccess wup -> do
      runSqlite "test5.db3" $ do
        insert wup
        return ()
    _ -> return ()
  redirect HomeR
{-
              ^{headerWidget}
              <div #eTitle style="text-align:center; width=100%; margin:0;">
                <b> View Rabbit
                <a href=@{EditR rab}>Edit</a>           
              ^{showWellness wellRs}
           |]
-}

viewRab rab = $(widgetFileNoReload def "viewRabbit")

showgroomed::Wellness->Text
showgroomed wellR = if (wellnessGroomed wellR) then "Y" else "-"


showvetvisit vetVisits = $(widgetFileNoReload def "showvetvisit");

getViewR::RabbitId->Handler Html
getViewR rabId  = do
    rab <-runSqlite "test5.db3"  $ do
                  rabt<- get rabId
                  return rabt
    wellRs<-queryWellness rabId
    vetvisits<-queryVetVisits rabId
    defaultLayout $ do
         setTitle "View Rabbit"
         $(widgetFileNoReload def "cancelbutton")
         [whamlet| 
              ^{headerWidget}
               <div #eTitle style="text-align:left; width=100%; margin:0;">
                <b> View Rabbit </b>
                <div #vrButD style="float:right; display:inline;">
                  <div .cancelBut #vrEdit style="display:inline; float:right;">
                   <a href=@{EditR rabId}> edit </a>
                  <div .cancelBut #vrVet  style="display:inline; float:right;">
                   <a href=@{VetVisitR rabId}> vet </a>
                  <div .cancelBut #vrAdopt  style="display:inline; float:right;">
                   <a href=@{AdoptedR rabId}> adopt </a>
                  <div .cancelBut #vrHome sytle="display:inline; float:right;">
                   <a href=@{HomeR}> home </a>
              <div #viewRabbitBlock>
              $maybe therab <-rab
               ^{viewRab therab}

               ^{showvetvisit vetvisits}
               ^{showWellness wellRs}
                
              $nothing
                  <h3> Rabbit Not Found>
           |]


getEditR::RabbitId->Handler Html
getEditR rabID  = do
    rabbit <-runSqlite "test5.db3"  $ do
                  rabt<- get rabID
                  return rabt
    wellRs<-queryWellness rabID
    (formWidget, enctype) <- generateFormPost (rabbitForm (rabbit, (Just wellRs)))
    (wellnessWidget, enctype) <-generateFormPost (wellnessForm rabID)
    defaultLayout $ do
         setTitle "Edit Rabbit"
         [whamlet|
              ^{headerWidget}
              <div #eTitle style="text-align:center; width=100%; margin:0;">
                <b> View/Edit Rabbit
              <form method=post action=@{UpdateR rabID} enctype=#{enctype}>
                 ^{formWidget}
              <form method=post action=@{WellnessR rabID} enctype=#{enctype}>
                 ^{wellnessWidget}
              ^{showWellness wellRs}
          |]

