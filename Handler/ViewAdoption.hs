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
  adopts<-getAdoptions
  defaultLayout 
    [whamlet|
    <div #adoptReqBlock>
     $forall (Entity aid (AdoptRequest apid adoptinfo), Entity pId per)<-adopts
        <div .row>
        #{personLastName per}, #{personFirstName per} :
                       <a href=@{ViewAdoptForm aid}>#{showtime (adoptInfoDate adoptinfo)}</a>
    |]

viewaform::Person->AdoptInfo->Widget
viewaform person adoptinfo= $(widgetFileNoReload def "viewAdoptForm");

doBool::Bool->Text
doBool val = if val then "Yes" else "No"

getViewAdoptForm::AdoptRequestId->Handler Html
getViewAdoptForm  id = do
  aform<- getAdoptForm id
  defaultLayout 
    [whamlet|
    <div #adoptReqBlock>
     $forall (Entity  aide  (AdoptRequest apid adoptinfo) , Entity pId per)<- aform
        <div .afrow>
         #{showtime (adoptInfoDate adoptinfo)}
         ^{viewaform per adoptinfo}
    |]
          

