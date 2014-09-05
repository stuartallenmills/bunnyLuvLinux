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

getNotes reqId =  runDB $
   select $ from $ \notes->do
     where_ (notes ^. AdoptNotesAdoptRequest ==. val reqId)
     return notes

getAdoptForm id = runDB $ 
    select $ from $ \(adopt, per)-> do
     where_ ((adopt ^. AdoptRequestPerson ==. per ^. PersonId) &&. (adopt ^. AdoptRequestId ==. val id))
     return (adopt, per)

adoptNotesForm::AdoptRequestId->Html->MForm Handler (FormResult AdoptNotes, Widget)
adoptNotesForm reqId extra= do
  local_time <- liftIO  getLocalTime
  let stime = showtime (localDay local_time)

  (dateRes, dateView)<-mreq textField "nope" (Just stime)
  (actionRes, actionView)<-mreq textareaField "nope" Nothing
  let date = fmap (doparseTime.unpack) dateRes
  let res = AdoptNotes reqId <$> date <*> actionRes
  let wid = do
        [whamlet| #{extra}
             <div .blDate #anDate>
              Date: ^{fvInput dateView}
             <div .required #action>
              <div .bllabel>
               Action:
              ^{fvInput actionView}
             <input .subButton  type=submit value="submit">
            |]
        toWidget [lucius|
               ##{fvId actionView} {
                     width:85%;
                 }
          |]
  
  return (res, wid)

notesWidget::AdoptRequestId->Widget
notesWidget aid = do
  notes<- handlerToWidget $ (getNotes aid) 
  [whamlet|
   <div #adoptNoteBlock>
    $forall (Entity nid (AdoptNotes reqId date action))<-notes
      <div .afrow #anote style="padding-left:8px;">
       <div #arDate>
        #{showtime date} :
       <div #arAction style="padding-left:5px;">
        #{action}
    |]

getNewNoteR::AdoptRequestId->Handler Html
getNewNoteR reqId = do
    (tnotesWidget, n_enctype)<-generateFormPost (adoptNotesForm reqId)
    defaultLayout $
     [whamlet|
      <form method=post action=@{NewNoteR reqId} enctype=#{n_enctype}>
          ^{tnotesWidget}

     |]
postNewNoteR::AdoptRequestId->Handler Html
postNewNoteR reqId = do
  ((result,_),_) <- runFormPost (adoptNotesForm reqId)
  case result of
    FormSuccess note-> do
      runDB $ do
        insert note
        return()
      redirect ViewAdoptForms
    _ -> redirect ViewAdoptForms
  
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
                      #{showtime (adoptInfoDate adoptinfo)}
         <div #addNote><a href=@{NewNoteR aid}>New Action</a>
         ^{notesWidget aid}
 
      |]
    toWidget [lucius|

   .afrow {
    width:100%;
    margin-bottom:2px;
    margin-top:2px;
   border-bottom:1px dotted #7f7f7f;
   }

 #adoptReqBlock div {
    float:left;
  }

  #addNote {
     float:right;   
  }

 #addNote a {
    border:1px solid #7f7f7f;
    border-radius:5px;
    padding-left:4px;
    padding-right:4px;
    padding-top:1px;
    padding-bottom:1px;
    font-size:90%;
    text-decoration:none;
    background:#efefef;
    color:#000000;
}

#addNote a:hover {
  border:1px solid #ff7f7f;
  background:#ffffff;
}

  #adoptReqBlock #addNote {
    float:right;
   }
     
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
          

