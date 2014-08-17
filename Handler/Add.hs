{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Add where

--this is a test 

--import qualified Data.ByteString.Lazy as L
import Conduit

--import Data.Conduit
--import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update)
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
--import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
--import Database.Persist.Sql (insert)
--import Control.Monad.IO.Class (liftIO)
--import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
--import Text.Julius
import FormUtils
import Utils



queryWellness rabId = runDB $ 
  select $ from $ \r ->do
     where_ (r ^. WellnessRabbit ==. val rabId)
     orderBy [desc (r ^. WellnessDate)]
     return r

queryVetVisits rabId = runDB $ 
 select $ from $ \r ->do
     where_ (r ^. VetVisitRabbit ==. val rabId)
     orderBy [desc (r ^. VetVisitDate)]
     return r

queryAdopted rabId = runDB $ 
 select $ from $ \r ->do
     where_ (r ^. AdoptedRabbit ==. val rabId)
     return r
  
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
              <div #diedDate .blDate>
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
getDiedR rabId= do
    Just rab <- runDB $ get rabId
    (formWidget, enctype) <- generateFormPost diedForm 
    let menu = [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">
                <b> Death Report for #{rabbitName rab}
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{ViewR rabId}> cancel </a>
                |]
    let form = [whamlet|
                <form method=post action=@{DiedR rabId} enctype=#{enctype}>
                 ^{formWidget}
                 |]

    baseForm "Died" menu form
    
postDiedR::RabbitId->Handler Html
postDiedR rabId = do
  ((result, _), _) <-runFormPost diedForm 

  case result of
    FormSuccess died -> 
       runDB $ 
        update $ \p -> do
          set p [RabbitStatus =. val "Died", RabbitStatusDate =. val  (diedDate died),
                 RabbitStatusNote =. val (diedNotes died) ]
          where_ (p ^. RabbitId ==. val rabId)
          return ()
    _ -> return ()

  redirect (ViewR rabId)



rabbitForm ::(Maybe Rabbit, Maybe [Entity Wellness])-> Html -> MForm Handler (FormResult Rabbit, Widget)
rabbitForm (mrab, rabId) extra = do
    local_time <- liftIO  getLocalTime
    let today = localDay local_time
    let stime = showtime today
    let tname = case mrab of 
          Nothing -> Nothing
          Just rb -> Just (rabbitName rb)
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
         addStylesheetRemote "//code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css"
         toWidget [julius|

                     $(function () {
                               if ( $( "#hident6" ).val() == "2") {
                                 $( "#hident7 option[value='3']" ).remove();
                                 }
                               else {
                                 $( "#hident7 option[value='4']" ).remove();
                                 }
                              });


                     $(function () {
                          $( "#hident6" ).change (function() {
                               if ( $( "#hident6" ).val() == "2") {
                                 $( "#hident7 option[value='3']" ).remove();
                                 $( "#hident7" ).append ("<option value='4'>Neutered</option>");
                                }
                               else {
                                 $( "#hident7 option[value='4']" ).remove();
                                 $( "#hident7" ).append ("<option value='3'>Spayed</option>");
                                 }

                              });
                            });

                     $(function () {
                          $( "#hident3" ).change (function() {
                             var str = $( "#hident3" ).val();
                               { $( "#hident13" ).val(str);}
                             });
                            });


          |]
{-
        
         toWidget [julius|

         



                      
                     $(function () {
                          $( "#hident3" ).change (function() {
                             alert("hident3 change");
                             var str = $( "#hident3" ).val();
                             $( "#hident13" ).val(str);
                            }};

                         |]
 -}
                  
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
    FormSuccess  rabi -> 
     runDB $ do
        rabId<-insert  rabi
        return (ViewR rabId)
    _ -> return HomeR
  redirect link

-- update a rabbit 
postUpdateR::RabbitId->Handler Html
postUpdateR rabId = do
  ((result, _), _) <-runFormPost (rabbitForm (Nothing,Nothing))

  case result of
    FormSuccess rabi -> do
      runDB $ do
        _ <-replace  rabId rabi
        return ()
    _ -> return ()

  redirect (ViewR rabId)




viewRab imgpath rab yrs mnths bonded = $(widgetFileNoReload def "viewRabbit")


viewRabMenu showMenu not_dead not_adopted not_altered rabId = $(widgetFileNoReload def "editmenu")

showvetvisit rabbit vetVisits = $(widgetFileNoReload def "showvetvisit");

showadopted rabbit adopteds = $(widgetFileNoReload def "showadopted");

showtreatments treatments = $(widgetFileNoReload def "showTreatments");

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
    bonded<-queryGetBonded rabId
    treatments<-queryTreatmentB rabId
    local_time <- liftIO $ getLocalTime
    let today = localDay local_time
    let stime = showtime (today)
    let dage = diffDays today  (rabbitBirthday rab)
    let (yrs,rm) = dage `divMod` 365
    let mnths = rm `div` 30
    let was_adopted = (length adopteds > 0)
    let had_visits = (length vetvisits >0)
    let had_well = (length wellRs > 0)
    let had_treatments = (length treatments>0)
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
                $( "#vvRArrow" ).toggle();
                $( "#vvDArrow" ).toggle();
              });
            });
           $(function() {
            $("#haswell").click(function() {
               $("#showWell").toggle();   
               $( "#wDArrow" ).toggle();
               $( "#wRArrow" ).toggle();          
             });
            }); 
           $(function() {
            $("#vtreatments").click(function() {
               $("#showtreatments").toggle();   
               $( "#tDArrow" ).toggle();
               $( "#tRArrow" ).toggle();          
             });
            });           
          |] 
         [whamlet|
           <div #blHeaderD>
            ^{getNameWidget bnames formWidget enctype}
            ^{headerLogWid imgpath maid}    
            ^{viewRabMenu showMenu not_dead not_adopted not_altered rabId}      
           ^{viewRab  imgpath rab yrs mnths bonded}
              $if showMenu
               $if was_adopted
                   ^{showadopted rab adopteds}
               $if had_treatments
                 <div #vtreatments style="float:left;" title="Show/Hide Treatments" >
                   <div id="tRArrow" class=arrow-right>
                   <div id="tDArrow" class=arrow-down> 
                  <b>Injury/Illness Treatments </b> 
                 ^{showtreatments treatments}
                $if had_visits 
                 <div #vetvisits style="float:left;" title="Show/Hide Vet Visits" >
                    <div id="vvRArrow" class=arrow-right>
                    <div id="vvDArrow" class=arrow-down> 
                   <b> Vet Visits </b> 
                 ^{showvetvisit rab vetvisits}
               $if had_well
                 <div #haswell style="float:left;" title="Show/Hide Wellness Reports"> 
                    <div id="wDArrow" class=arrow-down>
                    <div id="wRArrow" class=arrow-right>
                   <b> Wellness </b>
                 ^{showWellness wellRs}               
           |]
         toWidget [lucius|
.arrow-right {
    padding-left:10px;
    border-bottom: 10px solid transparent;
    border-left: 10px solid black;
    border-top: 10px solid transparent;
    display: inline-block;
    float: left;
    height: 0;
    width: 0;
}


.arrow-down {
    border-left: 10px solid transparent;
    border-right: 10px solid transparent;
    border-top: 10px solid;
    display: none;
    float: left;
    height: 0;
    width: 0;
    transform:translateY(5px);
}

|]

getEditR::RabbitId->Handler Html
getEditR rabId  = do
    rabbit <-runDB  $  get rabId
    wellRs<-queryWellness rabId
    (formWidget, enctype) <- generateFormPost (rabbitForm (rabbit, (Just wellRs)))
    let menu = [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">
                <b> Edit Rabbit
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{ViewR rabId}> cancel </a>
               |]
    let form = [whamlet|
              <form method=post action=@{UpdateR rabId} enctype=#{enctype}>
                 ^{formWidget}
                 |]            
    baseForm "Edit Rabbit" menu form

