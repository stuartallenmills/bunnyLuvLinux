{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.ViewAdoption where

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

--getAdoptions::Handler [(Entity AdoptRequest, Entity Person)]
getAdoptions = runDB $
  select $ from $ \(adopt, per) -> do
    where_ ((adopt ^. AdoptRequestPerson) ==. (per ^. PersonId))
    return (adopt, per)


getAdoptForm id = runDB $ 
    select $ from $ \(adopt, per)-> do
     where_ ((adopt ^. AdoptRequestPerson ==. per ^. PersonId) &&. (adopt ^. AdoptRequestId ==. val id))
     return (adopt, per)


getViewAdoptForms::Handler Html
getViewAdoptForms = do
  (formWidget, enctype)<- generateFormPost getNameForm
  adopts<-getAdoptions
  bnames <-  getNamesDB
  maid <- maybeAuthId
  impath <- liftIO getImagePath
  let imgpath = unpack impath

  admin <- isAdmin
  defaultLayout $ do
    setTitle "Adoption Requests"
    $(widgetFileNoReload def "cancelbutton")
--         addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
--         addScriptRemote "//code.jquery.com/ui/1.11.0/jquery-ui.js"
--         addStylesheetRemote "//code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css"
    toWidget [julius|
 .afrow {
    width:100%;
    margin-bottom:2px;
   border-bottom:1px dotted #7f7f7f;
 }
 |]
              
    [whamlet|
     <div #blHeaderD>
     ^{getNameWidget bnames formWidget enctype}
     ^{headerLogWid imgpath maid}    
     <div #afTitle style="width:100%; float:left; text-align:center; background:#cfcfcf;"> 
           <b> Adoption Requests </b>

    <div #adoptReqBlock>
     $forall (Entity aid (AdoptRequest apid adoptinfo), Entity pId per)<-adopts
        <div .afrow>
         <a href=@{ViewAdoptForm aid}>#{personLastName per}, #{personFirstName per} :&nbsp;
                      #{showtime (adoptInfoDate adoptinfo)}</a>
    |]

viewaform::Person->AdoptInfo->Widget
viewaform person adoptinfo= $(widgetFileNoReload def "viewAdoptForm");

doBool::Bool->Text
doBool val = if val then "Yes" else "No"

getViewAdoptForm::AdoptRequestId->Handler Html
getViewAdoptForm  id = do
  (formWidget, enctype)<- generateFormPost getNameForm
  aform<- getAdoptForm id
  bnames <-  getNamesDB
  maid <- maybeAuthId
  impath <- liftIO getImagePath
  let imgpath = unpack impath

  admin <- isAdmin
  defaultLayout $ do
    setTitle "Adoption Request"
    $(widgetFileNoReload def "cancelbutton")
--         addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
--         addScriptRemote "//code.jquery.com/ui/1.11.0/jquery-ui.js"
--         addStylesheetRemote "//code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css"     
    [whamlet|
     <div #blHeaderD>
     ^{getNameWidget bnames formWidget enctype}
     ^{headerLogWid imgpath maid}    
    <div #afTitle style="width:100%; float:left; text-align:center; background:#cfcfcf;"> 
           <b> Adoption Request </b>
    <div #adoptReqBlock style="float:left;">
     $forall (Entity  aide  (AdoptRequest apid adoptinfo) , Entity pId per)<- aform
          <div #afDate style="float:right; width:100%; text-align:right;">
            #{showtime (adoptInfoDate adoptinfo)}
          ^{viewaform per adoptinfo}
    |]
          

