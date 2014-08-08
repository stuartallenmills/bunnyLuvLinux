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
import Text.Julius
import FormUtils
import Utils



queryWellness rabID = runDB $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. WellnessRabbit ==. val rabID)
     orderBy [desc (r ^. WellnessDate)]
     return (r)
  return zipt

queryVetVisits rabID = runDB $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. VetVisitRabbit ==. val rabID)
     orderBy [desc (r ^. VetVisitDate)]
     return (r)
  return zipt

queryAdopted rabID = runDB $ do
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

testDateIn now  Nothing = Nothing
testDateIn now (Just rab)  = Just (showtime (rabbitDateIn rab))

getDateIn Nothing = Nothing
getDateIn (Just rab) = Just (rabbitDateIn rab)

testStatusDate now Nothing = Nothing
testStatusDate now (Just rab) = Just (rabbitStatusDate rab)

testAlteredDate::Maybe Rabbit -> Maybe (Maybe Text)
testAlteredDate Nothing = Nothing
testAlteredDate (Just ( Rabbit _ _ _ _ _ _ _ Nothing _  _ _ _ _) ) = Nothing
testAlteredDate (Just ( Rabbit _ _ _ _ _ _ _ (Just da) _  _ _ _ _) ) = Just (Just (showtime da))

months::[(Text, Integer)]
months =[(pack (show x), x) | x<- [0..11]]
years::[(Text, Integer)]
years = [(pack (show x), x) | x<-[0..20]]
{-
diedW::Widget
diedW =  [whamlet|
              <div #diedDate>
                Date:  ^{fvInput statusDateView}
              <div #diedNote>
                Notes: ^{fvInput statusNoteView}
          |]
-}
diedForm::Html -> MForm Handler (FormResult Died, Widget)
diedForm extra = do
    local_time <- liftIO $ getLocalTime
    let today = localDay local_time
    let stime = showtime (today)
    (statusDateRes, statusDateView) <- mreq textField " nope" (Just stime) 
    (statusNoteRes, statusNoteView) <- mreq textField "nope" Nothing
    let diedRes = Died <$> statusDateRes <*> statusNoteRes
    let diedw = do
          [whamlet|
              #{extra}
              <div #diedDate>
                Date:  ^{fvInput statusDateView}
              <div #diedNote>
                Notes: ^{fvInput statusNoteView}
             <input type=submit value="submit">
                        |]
          toWidget [lucius|
                ##{fvId statusNoteView} {
                      width:25em;
                 }
              |]
    return (diedRes, diedw)

getDiedR::RabbitId->Handler Html
getDiedR rabid= do
    Just rab <- runDB $ get rabid
    (formWidget, enctype) <- generateFormPost diedForm 
    let menu = [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">
                <b> Death Report for #{rabbitName rab}
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{ViewR rabid}> cancel </a>
                |]
    let form = [whamlet|
                <form method=post action=@{DiedR rabid} enctype=#{enctype}>
                 ^{formWidget}
                 |]

    baseForm "Died" menu form
    
postDiedR::RabbitId->Handler Html
postDiedR rabid = do
  ((result, _), _) <-runFormPost (diedForm )

  case result of
    FormSuccess died -> do
       runDB $ do
        update $ \p -> do
          set p [RabbitStatus =. val "Died", RabbitStatusDate =. val ( (diedDate died)),
                 RabbitStatusNote =. val (diedNotes died) ]
          where_ (p ^. RabbitId ==. val rabid)
          return ()
    _ -> return ()

  redirect (ViewR rabid)



rabbitForm ::(Maybe Rabbit, Maybe [Entity Wellness])-> Html -> MForm Handler (FormResult Rabbit, Widget)
rabbitForm (mrab, rabID) extra = do
    local_time <- liftIO $ getLocalTime
    let today = localDay local_time
    let stime = showtime (today)
    let tname = case mrab of 
          Nothing -> Nothing
          Just rb -> (Just (rabbitName rb))
    (nameRes, nameView) <- mreq textField "this is not used" tname
    (dateInRes, dateInView) <-mreq textField "nope" (testDateIn stime mrab)
    (descRes, descView) <- mreq textField "neither is this"  (test mrab rabbitDesc)
    (sourceRes, sourceView) <- mreq textField "neither is this" (test mrab rabbitSource)
    (sexRes, sexView) <- mreq (selectFieldList sex) "not" (test mrab rabbitSex)
    (alteredRes, alteredView) <- mreq (selectFieldList altered) "not" (test mrab rabbitAltered)
    (alteredDateRes, alteredDateView)<-mopt textField "this is not" (testAlteredDate mrab)
    (statusRes, statusView) <- mreq (selectFieldList status) "who" (test mrab rabbitStatus)
    (yrsIntakeRes, yrsIntakeView) <- mreq (selectFieldList years) "zip" (getYrsDateInM mrab)
    (mnthsIntakeRes, mnthsIntakeView) <- mreq (selectFieldList months) "zip" (getMonthsDateInM mrab)
    (sourceTypeRes, sourceTypeView) <- mreq (selectFieldList sourceType) "zip" (test mrab rabbitSourceType)
    (statusDateRes, statusDateView) <- mreq textField " nope" (testStatusDate stime  mrab)
    (statusNoteRes, statusNoteView) <- mreq textField "nope" (opttest mrab rabbitStatusNote)
    (imgNoteRes, imgNoteView)<- mopt textField "nopt" Nothing
    let yrdays = (365*) <$> yrsIntakeRes
    let mndays = (30*) <$> mnthsIntakeRes
    let daysTot = (+) <$> yrdays <*> mndays
    let daysTotNeg = ((-1)*) <$> daysTot
    let date = text2date dateInRes
    let bday = ( addDays) <$> daysTotNeg <*> date 
    let alteredDate = text2dateM alteredDateRes 
    let rabbitUpdateRes =Rabbit <$>  nameRes <*>  descRes <*> date <*> sourceTypeRes <*> sourceRes <*> sexRes <*> alteredRes <*> alteredDate <*> statusRes <*> statusDateRes <*> statusNoteRes <*> bday  <*> imgNoteRes
 --   let rabbitRes = rabbitRes1 <*> (FormResult (Just True)) <*> (FormResult Nothing) Nothing
    let awid= do
         $(widgetFileNoReload def "add")
         addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
         addScriptRemote "//code.jquery.com/ui/1.11.0/jquery-ui.js"
         addStylesheetRemote "//code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css"

         toWidget [julius|
                    $(function () {
                         $( "#hident3" ).change (function() {
                             $( "#hident13" ).val( $( "#hident3" ).val());                    
                             });
                            });
                         $(function () {
                          $( ".blDate :input" ).change (function() {
                             var str = $( ".blDate :input" ).val();
                              var res = str.split("/");
                             if (res.length != 3) {
                                alert("Date must be m/d/yyyy");
                                $( ".blDate :input" ).clearQueue();
                                $( ".blDate :input" ).val("");
                                $( ".blDate :input").focus();
                             } else {
                               var yr = res [2];
                               if (yr.length !=4) {
                                alert("Year must have 4 characters: yyyy");
                                $( ".blDate :input" ).clearQueue();
                                $( ".blDate :input").val("");
                                $( ".blDate :input").focus();
                              }
                             }                          
                             });
                            });

                         |]
                  
    return (rabbitUpdateRes, awid)


getAddR::Handler Html
getAddR = do
  (formWidget, enctype) <- generateFormPost (rabbitForm (Nothing,Nothing))
  let menu = 
           [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">
                <b> Add Rabbit
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{HomeR}> cancel </a>
                   |]
  let form =   [whamlet|  <form method=post action=@{PostR} enctype=#{enctype}>
                 ^{formWidget}
                 |]
  baseForm "Add Rabbit" menu form

  

postPostR::Handler Html
postPostR = do
  ((result, _), _) <-runFormPost (rabbitForm (Nothing, Nothing))
  link<- case result of
    FormSuccess  rabi -> do
     runDB $ do
        rabid<-insert $ rabi
        return (ViewR rabid)
    _ -> return (HomeR)
  redirect (link)

-- update a rabbit 
postUpdateR::RabbitId->Handler Html
postUpdateR rabID = do
  (((result), _), _) <-runFormPost (rabbitForm (Nothing,Nothing))

  case result of
    FormSuccess rabi -> do
      runDB $ do
        _ <-replace  rabID rabi
        return ()
    _ -> return ()

  redirect (ViewR rabID)




viewRab imgpath rab yrs mnths = $(widgetFileNoReload def "viewRabbit")


viewRabMenu showMenu not_dead not_adopted not_altered rabId = $(widgetFileNoReload def "editmenu")

showvetvisit rabbit vetVisits = $(widgetFileNoReload def "showvetvisit");

showadopted rabbit adopteds = $(widgetFileNoReload def "showadopted");

getViewR::RabbitId->Handler Html
getViewR rabId  = do
    (formWidget, enctype)<- generateFormPost getNameForm
 --   bnames <-  getNamesDB
    bnames <-  getNamesDB
    maid <- maybeAuthId
    impath <- liftIO getImagePath
    let imgpath = unpack impath

    admin <- isAdmin
    let showMenu = (admin==Authorized)
    Just rab <-runDB  $ do
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
    let not_altered = not ((rabbitAltered rab=="Spayed") || (rabbitAltered rab == "Neutered"))
    defaultLayout $ do
         setTitle "View Rabbit"
         $(widgetFileNoReload def "cancelbutton")
         addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
         addScriptRemote "//code.jquery.com/ui/1.11.0/jquery-ui.js"
         addStylesheetRemote "//code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css" 
         toWidget [julius|
           $(function() {
            $("#vetvisits").click(function() {
                $("#showvv").toggle();
             });
            });
           $(function() {
            $("#haswell").click(function() {
               $("#showWell").toggle();             
             });
            });           
          |] 
         [whamlet|
           <div #blHeaderD>
            ^{getNameWidget bnames formWidget enctype}
            ^{headerLogWid imgpath maid}    
            ^{viewRabMenu showMenu not_dead not_adopted not_altered rabId}      
           ^{viewRab  imgpath rab yrs mnths}
              $if showMenu
               $if was_adopted
                   ^{showadopted rab adopteds}
               $if had_visits 
                 <div #vetvisits style="float:left;"> <b> Vet Visits </b> </div>
                 ^{showvetvisit rab vetvisits}
               $if had_well
                 <div #haswell style="float:left;"><b> Wellness </b> </div>
                 ^{showWellness wellRs}               
           |]


getEditR::RabbitId->Handler Html
getEditR rabID  = do
    rabbit <-runDB  $  get rabID
    wellRs<-queryWellness rabID
    (formWidget, enctype) <- generateFormPost (rabbitForm (rabbit, (Just wellRs)))
    let menu = [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">
                <b> Edit Rabbit
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{ViewR rabID}> cancel </a>
               |]
    let form = [whamlet|
              <form method=post action=@{UpdateR rabID} enctype=#{enctype}>
                 ^{formWidget}
                 |]            
    baseForm "Edit Rabbit" menu form

