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

getAdoptions = runDB $
    select $ from $ \(adopt, per)->
    where_ (adopt ^. AdoptRequestPerson ==. per ^. PersonID)
    return (adopt, per)

getAdoptForm id = runDB $ 
    select $ from $ \(adopt, per)->
    where_ ((adopt ^. AdoptRequestPerson ==. per ^. PersonID) &&. (adopt %. AdoptRequestId ==. val id))
    return (adopt, per)

getViewAdoptForms::Handler Html
getViewAdoptForms = do
  adopts<-getAdoptions
  defaultLayout 
    [whamlet|
    <div #adoptReqBlock>
     $forall (Entity aid adoptreq, Entity pId per)<-adopts
        <div .row>
        #{personLastName per}, #{personFirstName} :  #{adoptRequestDate adoptreq}
    |]

getViewAdoptForm::AdoptRequestID->Html Handler
getViewAdoptForm  id =
  aform<- getAdoptForm id
  defaultLayout 
    [whamlet|
    <div #adoptReqBlock>
     $forall (Entity aid adoptreq, Entity pId per)<- aform
        <div .row>
        #{personLastName per}, #{personFirstName} :  #{adoptRequestDate adoptreq}
    |]
          
