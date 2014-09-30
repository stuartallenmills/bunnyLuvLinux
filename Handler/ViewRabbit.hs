{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.ViewRabbit where

import Conduit
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update)
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import FormUtils
import Utils


viewRab imgpath rab yrs mnths bonded story = $(widgetFileNoReload def "viewRabbit")

showWellness wellness =   $(widgetFileNoReload def "showwellness")

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
    story<-queryStory rabId
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
    let vlen= length vetvisits;
    let had_visits = vlen >0
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
           ^{viewRab  imgpath rab yrs mnths bonded story}
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
