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

queryAdopted rabID = runSqlite "test5.db3" $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. AdoptedRabbit ==. val rabID)
     return (r)
  return zipt
  
test mrab field = case mrab of
                    Nothing->Nothing
                    Just ri->Just (field ri)


opttest::(Maybe Rabbit)->(Rabbit->Text)->(Maybe Text)
opttest mrab field= case mrab of
                   Nothing-> Just ""
                   Just ri-> Just (field ri)



showWellness wellness =   $(widgetFileNoReload def "showwellness")

testDateIn now  Nothing = Just now
testDateIn now (Just rab)  = Just (showtime (rabbitDateIn rab))

testStatusDate now Nothing = Just now
testStatusDate now (Just rab) = Just (rabbitStatusDate rab)

testAlteredDate::Maybe Rabbit -> Maybe (Maybe Text)
testAlteredDate Nothing = Nothing
testAlteredDate (Just ( Rabbit _ _ _ _ _ _ _ Nothing _ _ _ _ _) ) = Nothing
testAlteredDate (Just ( Rabbit _ _ _ _ _ _ _ (Just da) _ _ _ _ _) ) = Just (Just (showtime da))

months::[(Text, Integer)]
months =[(pack (show x), x) | x<- [0..11]]
years::[(Text, Integer)]
years = [(pack (show x), x) | x<-[0..20]]

rabbitForm ::(Maybe Rabbit, Maybe [Entity Wellness])-> Html -> MForm Handler (FormResult Rabbit, Widget)
rabbitForm (mrab, rabID) extra = do
    local_time <- liftIO $ getLocalTime
    let today = localDay local_time
    let stime = showtime (today)
    let tname = case mrab of
          Nothing -> Nothing
          Just rb -> (Just (rabbitName rb))
    (nameRes, nameView) <- mreq textField "this is not used" tname
    (descRes, descView) <- mreq textField "neither is this"  (test mrab rabbitDesc)
    (dateInRes, dateInView)<-mreq textField "  " (testDateIn stime  mrab)
    (sourceRes, sourceView) <- mreq textField "neither is this" (test mrab rabbitSource)
    (sexRes, sexView) <- mreq (selectFieldList sex) "not" (test mrab rabbitSex)
    (alteredRes, alteredView) <- mreq (selectFieldList altered) "not" (test mrab rabbitAltered)
    (alteredDateRes, alteredDateView)<-mopt textField "this is not" (testAlteredDate mrab)
    (statusRes, statusView) <- mreq (selectFieldList status) "who" (test mrab rabbitStatus)
    (ageIntakeRes, ageIntakeView) <- mreq textField "zip" (test mrab rabbitAgeIntake)
    (yrsIntakeRes, yrsIntakeView) <- mreq (selectFieldList years) "zip" Nothing
    (mnthsIntakeRes, mnthsIntakeView) <- mreq (selectFieldList months) "zip" Nothing
    (sourceTypeRes, sourceTypeView) <- mreq (selectFieldList sourceType) "zip" (test mrab rabbitSourceType)
    (statusDateRes, statusDateView) <- mreq textField " nope" (testStatusDate stime  mrab)
    (statusNoteRes, statusNoteView) <- mreq textField "nope" (opttest mrab rabbitStatusNote)
    let yrdays = (365*) <$> yrsIntakeRes
    let mndays = (30*) <$> mnthsIntakeRes
    let daysTot = (+) <$> yrdays <*> mndays
    let daysTotNeg = ((-1)*) <$> daysTot
    let date = text2date dateInRes
    let bday = ( addDays) <$> daysTotNeg <*> date 
    let alteredDate = text2dateM alteredDateRes 
    let rabbitUpdateRes =Rabbit <$>  nameRes <*>  descRes <*> date <*> sourceTypeRes <*> sourceRes <*> sexRes <*> alteredRes <*> alteredDate <*> ageIntakeRes <*> statusRes <*> statusDateRes <*> statusNoteRes <*> bday     
 --   let rabbitRes = rabbitRes1 <*> (FormResult (Just True)) <*> (FormResult Nothing) Nothing
    let awid=  $(widgetFileNoReload def "add")
    return (rabbitUpdateRes, awid)




getAddR ::Handler Html    
getAddR  = do
    (formWidget, enctype) <- generateFormPost (rabbitForm (Nothing,Nothing))
    defaultLayout $ do
         setTitle "Add Rabbit"
         $(widgetFileNoReload def "cancelbutton")
         [whamlet|
             ^{headerWidget}
              <div #addCance style="text-align:left; margin-top:5px; margin-bottom:8px;">
                <b> Add Rabbit
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{HomeR}> cancel </a>
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


{-
              ^{headerWidget}
              <div #eTitle style="text-align:center; width=100%; margin:0;">
                <b> View Rabbit
                <a href=@{EditR rab}>Edit</a>           
              ^{showWellness wellRs}
           |]
-}

viewRab rab yrs mnths = $(widgetFileNoReload def "viewRabbit")

showgroomed::Wellness->Text
showgroomed wellR = if (wellnessGroomed wellR) then "Y" else "-"


showvetvisit vetVisits = $(widgetFileNoReload def "showvetvisit");

showadopted adopteds = $(widgetFileNoReload def "showadopted");

getViewR::RabbitId->Handler Html
getViewR rabId  = do
    Just rab <-runSqlite "test5.db3"  $ do
                  rabt<- get rabId
                  return rabt
    wellRs<-queryWellness rabId
    vetvisits<-queryVetVisits rabId
    adopteds<-queryAdopted rabId
    local_time <- liftIO $ getLocalTime
    let today = localDay local_time
    let stime = showtime (today)
    let dage = diffDays today  (rabbitBirthday rab)
    let (yrs,rm) = dage `divMod` 365
    let mnths = rm `div` 30
    let was_adopted = (length adopteds > 0)
    let had_visits = (length vetvisits >0)
    let had_well = (length wellRs > 0)
    let not_dead = not ((rabbitStatus rab == "Died") || (rabbitStatus rab == "Euthanized"))
    let not_adopted = not (rabbitStatus rab == "Adopted")
    defaultLayout $ do
         setTitle "View Rabbit"
         $(widgetFileNoReload def "cancelbutton")
         [whamlet| 
              ^{headerWidget}
               <div #eTitle .subTitle >
                <b> View Rabbit </b>
                <div #vrButD style="float:right; display:inline;">
                  <div .cancelBut #vrHome sytle="display:inline; float:right;">
                    <a href=@{HomeR}> cancel </a>
                  <div .cancelBut #vrEdit style="display:inline; float:right;">
                   <a href=@{EditR rabId}> edit </a>
                  $if not_dead
                   <div .cancelBut #vrVet  style="display:inline; float:right;">
                    <a href=@{VetVisitR rabId}> vet </a>
                   $if not_adopted
                    <div .cancelBut #vrAdopt  style="display:inline; float:right;">
                     <a href=@{AdoptedR rabId}> adopt </a>
                    <div .cancelBut #vrWell style="display:inline; float:right;">
                     <a href=@{WellnessR rabId}>wellness </a>
                    
              <div #viewRabbitBlock>
               ^{viewRab rab yrs mnths}
               $if was_adopted
                   ^{showadopted adopteds}
               $else
                   <span> </span>
               $if had_visits 
                   ^{showvetvisit vetvisits}
               $else
                  <span> </span>
               $if had_well
                   ^{showWellness wellRs}
               $else
                  <span> </span>
                
           |]


getEditR::RabbitId->Handler Html
getEditR rabID  = do
    rabbit <-runSqlite "test5.db3"  $ do
                  rabt<- get rabID
                  return rabt
    wellRs<-queryWellness rabID
    (formWidget, enctype) <- generateFormPost (rabbitForm (rabbit, (Just wellRs)))
    defaultLayout $ do
         setTitle "Edit Rabbit"
         $(widgetFileNoReload def "cancelbutton")
         [whamlet|
              ^{headerWidget}
              <div #eTitle .subTitle>
                <b> Edit Rabbit
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{ViewR rabID}> cancel </a>
              <form method=post action=@{UpdateR rabID} enctype=#{enctype}>
                 ^{formWidget}
          |]

